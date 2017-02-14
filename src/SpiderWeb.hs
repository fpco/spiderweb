{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module SpiderWeb
    ( -- * Download
      download
    , DownloadOpts
    , addBannedDomain
    ) where

import ClassyPrelude.Conduit
#if MIN_VERSION_http_client(0, 5, 0)
import Network.HTTP.Client (HttpExceptionContent (..))
#endif
import Network.HTTP.Client (parseUrlThrow, getUri, HttpException (..), withResponseHistory, hrFinalRequest, hrFinalResponse, responseOpenHistory, responseClose)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Simple
import System.IO.Temp (withSystemTempDirectory)
import System.IO (openBinaryTempFile)
import Data.Word8 (_semicolon)
import qualified Text.HTML.TagStream.Types as Tag
import qualified Text.HTML.TagStream.ByteString as Tag
import qualified Network.URI as URI
import Control.Concurrent.Async.Lifted.Safe (Concurrently (..))
import Data.Foldable (sequenceA_)
import Control.Monad.State.Strict (modify)

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
        { doRoot = reverse $ dropWhile (== '/') $ reverse $ takeWhile (/= '?') $ pack root
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
    , dsErrors :: !(TQueue Text)
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

download :: MonadIO m => DownloadOpts -> m FilePath
download DownloadOpts {..} = liftIO $ withSystemTempDirectory "spiderweb" $ \tmpdir -> do
    queue <- newTQueueIO
    atomically $ writeTQueue queue (doRoot, Nothing)
    dstate <- DownloadState
        <$> newTVarIO (singletonSet doRoot)
        <*> pure queue
        <*> newTVarIO 0
        <*> pure tmpdir
        <*> newTQueueIO
        <*> newTQueueIO
    runConcurrently
      $ sequenceA_
        (replicate doWorkers (Concurrently (worker dstate))
            :: [Concurrently IO ()])

    errs <- atomically $ drainTQueue $ dsErrors dstate

    if null errs
        then do
            captured <- atomically $ drainTQueue $ dsCaptured dstate
            mapM_ print captured
            -- FIXME write output file
            return doOutputFile
        else do
            let say = hPutStrLn stderr
            say "Errors occurred during download:"
            mapM_ say errs
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

    onError DownloadState {..} item msource err = atomically $ writeTQueue dsErrors $ concat
        [ "When processing URL "
        , tshow item
        , case msource of
            Nothing -> ""
            Just source -> concat
                [ ", referenced from "
                , tshow source
                ]
        , ", received exception: "
        , pack $ displayException err
        ]

    stripPrefixes [] _ = Nothing
    stripPrefixes (x:xs) y = (stripPrefix x y $> x) <|> stripPrefixes xs y

    bannedPrefixes = do
      domain <- doBannedDomains
      scheme <- ["http:", "https:", ""]
      return $ concat [scheme, "//", domain]

    oneURL _ urlText
        | any (`isPrefixOf` urlText) bannedPrefixes = terror $ "Invalid domain name: " ++ urlText
    oneURL DownloadState {..} urlText = do
        hPut stdout $ encodeUtf8 $ concat ["Checking URL: ", urlText, "\n"]
        case stripPrefix doRoot urlText of
            Nothing
                | doCheckOutsideRoot -> do
                    req <- parseRequest $ unpack urlText
                    retryHTTP $ httpLBS (setRequestMethod "HEAD" req) >>= checkResponse
                | otherwise -> return ()
            Just suffix -> do
                req <- parseRequest $ unpack $ doRoot ++ suffix
                let uri = getUri req
                (fp, h) <- liftIO $ openBinaryTempFile dsTempDir "download"
                man <- liftIO getGlobalManager
                res <- retryHTTP $ bracket
                  (responseOpenHistory req man)
                  (responseClose . hrFinalResponse) $ \hr -> do
                    let res = hrFinalResponse hr
                    checkResponse res
                    let contentType = maybe "" (takeWhile (/= _semicolon))
                                    $ lookup "Content-Type"
                                    $ getResponseHeaders res

                    -- May have followed a redirect
                    let newUrlText = tshow $ getUri $ hrFinalRequest hr
                        isOtherDomain = isNothing $ stripPrefix doRoot newUrlText
                    unless isOtherDomain $ runConduit $ bodyReaderSource (getResponseBody res) .| (getZipSink $
                        ZipSink (sinkHandle h) *>
                        ZipSink (checkContent uri contentType))
                    return $ fmap (const ()) res
                liftIO $ hClose h
                atomically $ writeTQueue dsCaptured (res, suffix, fp)
      where
        checkResponse res
            | 200 <= code && code < 300 = return ()
            | otherwise = error $ unpack $ concat
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

            addRoute' = addRoute uri . unpack . takeWhile (/= '#') . decodeUtf8
        checkContent _ "text/css" = return () -- FIXME! Need to implement something here
        checkContent _ _ = return ()

        checkH1Count 1 = return ()
        checkH1Count 0 = error "No <h1> tags found"
        checkH1Count _ = error "Multiple <h1> tags found"

        addRoute _ route | "mailto:" `isPrefixOf` route = return ()
        addRoute _ route | "tel:" `isPrefixOf` route = return ()
        addRoute _ route | "irc:" `isPrefixOf` route = return ()
        addRoute uri route =
            case URI.parseURIReference route of
                Nothing -> error $ "Invalid URI reference: " ++ show route
                Just ref -> do
                    let route' = tshow $ URI.nonStrictRelativeTo ref uri
                    atomically $ do
                        visited <- readTVar dsVisited
                        unless (route' `member` visited) $ do
                            writeTQueue dsQueue (route', Just uri)
                            writeTVar dsVisited $! insertSet route' visited

retryHTTP :: IO a -> IO a
retryHTTP inner =
    loop (2 :: Int)
  where
    loop 0 = inner
    loop i = inner `catch` \e ->
      case e of
#if MIN_VERSION_http_client(0, 5, 0)
        HttpExceptionRequest _req (ConnectionFailure _) -> loop (i - 1)
#else
        FailedConnectionException {} -> loop (i - 1)
        FailedConnectionException2 {} -> loop (i - 1)
#endif
        _ -> throwIO e
