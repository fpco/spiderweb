{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SpiderWeb
    ( -- * Download
      download
    , DownloadOpts
    , addBannedDomain
    ) where

import Conduit
import RIO
import Network.HTTP.Client (HttpExceptionContent (..))
import Network.HTTP.Client (parseUrlThrow, getUri, HttpException (..), withResponseHistory, hrFinalRequest, hrFinalResponse, responseOpenHistory, responseClose)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Simple
import System.IO (openBinaryTempFile)
import Data.Word8 (_semicolon)
import qualified Text.HTML.TagStream.Types as Tag
import qualified Text.HTML.TagStream.ByteString as Tag
import qualified Network.URI as URI
import Data.Foldable (sequenceA_)
import Control.Monad.State.Strict (modify)

import qualified RIO.HashSet as HS
import qualified RIO.Text as T
import qualified RIO.ByteString as B
import qualified RIO.List as List

defaultSpiderWeb :: FilePath
defaultSpiderWeb = "spider.web"

data DownloadOpts = DownloadOpts
    { doRoot :: !Text
    , doBannedDomains :: ![Text]
    , doCheckOutsideRoot :: !Bool
    , doOutputFile :: !FilePath
    , doWorkers :: !Int
    }
instance IsString DownloadOpts where
    fromString root = DownloadOpts
        { doRoot = T.reverse $ T.dropWhile (== '/') $ T.reverse $ T.takeWhile (/= '?') $ T.pack root
        , doBannedDomains = []
        , doCheckOutsideRoot = False -- True
        , doOutputFile = defaultSpiderWeb
        , doWorkers = 8
        }

addBannedDomain :: Text -> DownloadOpts -> DownloadOpts
addBannedDomain x opts = opts { doBannedDomains = x : doBannedDomains opts }

data DownloadState = DownloadState
    { dsVisited :: !(TVar (HashSet Text))
    , dsQueue :: !(TQueue (Text, Maybe URI.URI))
    , dsActive :: !(TVar Int)
    , dsTempDir :: !FilePath
    , dsErrors :: !(TQueue Utf8Builder)
    , dsCaptured :: !(TQueue (Response (), Text, FilePath))
    }

drainTQueue :: TQueue a -> STM [a]
drainTQueue q =
    loop id
  where
    loop front = do
        mx <- tryReadTQueue q
        case mx of
            Nothing -> return $ front []
            Just x -> loop $ front . (x:)

download :: forall env. HasLogFunc env => DownloadOpts -> RIO env FilePath
download DownloadOpts {..} = withSystemTempDirectory "spiderweb" $ \tmpdir -> do
    queue <- newTQueueIO
    atomically $ writeTQueue queue (doRoot, Nothing)
    dstate <- DownloadState
        <$> newTVarIO (HS.singleton doRoot)
        <*> pure queue
        <*> newTVarIO 0
        <*> pure tmpdir
        <*> newTQueueIO
        <*> newTQueueIO
    replicateConcurrently_ doWorkers $ worker dstate

    errs <- atomically $ drainTQueue $ dsErrors dstate

    if null errs
        then do
            captured <- atomically $ drainTQueue $ dsCaptured dstate
            mapM_ (logInfo . displayShow) captured
            -- FIXME write output file
            return doOutputFile
        else do
            logError "Errors occurred during download:"
            mapM_ (logError . display) errs
            error "Download failed"
  where
    worker ds@DownloadState {..} = mask $ \restore -> join $ atomically $ do
        let loop = do
                mitem <- tryReadTQueue dsQueue
                case mitem of
                    Nothing -> do
                        active <- readTVar dsActive
                        guard $ active == 0
                        return $ return ()
                    Just (item, msource) -> do
                        modifyTVar dsActive (+ 1)
                        return $ do
                            restore (handleAny (onError ds item msource) (oneURL ds item)) `finally`
                                atomically (modifyTVar dsActive (subtract 1))
                            worker ds
        loop

    onError DownloadState {..} item msource err = atomically $ writeTQueue dsErrors $
        "When processing URL " <>
        displayShow item <>
        (case msource of
            Nothing -> mempty
            Just source -> ", referenced from " <> displayShow source) <>
        ", received exception: " <>
        (fromString $ displayException err)

    stripPrefixes [] _ = Nothing
    stripPrefixes (x:xs) y = (T.stripPrefix x y $> x) <|> stripPrefixes xs y

    bannedPrefixes = do
      domain <- doBannedDomains
      scheme <- ["http:", "https:", ""]
      return $ T.concat [scheme, "//", domain]

    oneURL _ urlText
        | any (`T.isPrefixOf` urlText) bannedPrefixes = error $ "Invalid domain name: " <> T.unpack urlText
    oneURL DownloadState {..} urlText = do
        logInfo $ "Checking URL: " <> display urlText
        case T.stripPrefix doRoot urlText of
            Nothing
                | doCheckOutsideRoot -> do
                    req <- parseRequest $ T.unpack urlText
                    retryHTTP $ httpLBS (setRequestMethod "HEAD" req) >>= checkResponse
                | otherwise -> return ()
            Just suffix -> do
                req <- parseRequest $ T.unpack $ doRoot <> suffix
                let uri = getUri req
                (fp, h) <- liftIO $ openBinaryTempFile dsTempDir "download"
                man <- liftIO getGlobalManager
                res <- retryHTTP $ bracket
                  (liftIO $ responseOpenHistory req man)
                  (liftIO . responseClose . hrFinalResponse) $ \hr -> liftIO $ do
                    let res = hrFinalResponse hr
                    checkResponse res
                    let contentType = maybe "" (B.takeWhile (/= _semicolon))
                                    $ lookup "Content-Type"
                                    $ getResponseHeaders res

                    -- May have followed a redirect
                    let newUrlText = tshow $ getUri $ hrFinalRequest hr
                        isOtherDomain = isNothing $ T.stripPrefix doRoot newUrlText
                    unless isOtherDomain $ runConduit $ bodyReaderSource (getResponseBody res) .| (getZipSink $
                        ZipSink (sinkHandle h) *>
                        ZipSink (checkContent uri contentType))
                    return $ fmap (const ()) res
                liftIO $ hClose h
                atomically $ writeTQueue dsCaptured (res, suffix, fp)
      where
        checkResponse res
            | 200 <= code && code < 300 = return ()
            | otherwise = error $ T.unpack $ T.concat
                [ "Unexpected status code "
                , tshow code
                , " for URL "
                , urlText
                ]
          where
            code = getResponseStatusCode res
        checkContent uri "text/html" =
            Tag.tokenStream .| (execStateC (0 :: Int) (mapM_C onToken) >>= checkH1Count)
          where
            onToken (Tag.TagOpen "a" attrs _) = forM_ (lookup "href" attrs) addRoute'
            onToken (Tag.TagOpen "link" attrs _) = forM_ (lookup "href" attrs) addRoute'
            onToken (Tag.TagOpen "img" attrs _) = forM_ (lookup "src" attrs) addRoute'
            onToken (Tag.TagOpen "script" attrs _) = forM_ (lookup "src" attrs) addRoute'
            onToken (Tag.TagOpen "h1" _ _) = modify (+ 1)
            onToken _ = return ()

            addRoute' = addRoute uri . T.unpack . T.takeWhile (/= '#') . decodeUtf8With lenientDecode
        checkContent _ "text/css" = return () -- FIXME! Need to implement something here
        checkContent _ _ = return ()

        checkH1Count 1 = return ()
        checkH1Count 0 = error "No <h1> tags found"
        checkH1Count _ = error "Multiple <h1> tags found"

        addRoute _ route | "mailto:" `List.isPrefixOf` route = return ()
        addRoute _ route | "tel:" `List.isPrefixOf` route = return ()
        addRoute _ route | "irc:" `List.isPrefixOf` route = return ()
        addRoute uri route =
            case URI.parseURIReference route of
                Nothing -> error $ "Invalid URI reference: " ++ show route
                Just ref -> do
                    let route' = tshow $ URI.nonStrictRelativeTo ref uri
                    atomically $ do
                        visited <- readTVar dsVisited
                        unless (route' `HS.member` visited) $ do
                            writeTQueue dsQueue (route', Just uri)
                            writeTVar dsVisited $! HS.insert route' visited

retryHTTP :: RIO env a -> RIO env a
retryHTTP inner =
    loop (2 :: Int)
  where
    loop 0 = inner
    loop i = inner `catch` \e ->
      case e of
        HttpExceptionRequest _req (ConnectionFailure _) -> loop (i - 1)
        _ -> throwIO e
