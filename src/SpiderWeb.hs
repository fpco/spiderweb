{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module SpiderWeb
    ( -- * Download
      download
    , DownloadOpts
    ) where

import ClassyPrelude.Conduit
import Network.HTTP.Client (parseUrlThrow, getUri, HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Simple
import System.IO.Temp (withSystemTempDirectory)
import System.IO (openBinaryTempFile)
import Data.Word8 (_semicolon)
import qualified Text.HTML.TagStream.Types as Tag
import qualified Text.HTML.TagStream.ByteString as Tag
import qualified Network.URI as URI
import Control.Concurrent.Async.Lifted.Safe (Concurrently (..))
import Data.Foldable (sequenceA_)

defaultSpiderWeb :: FilePath
defaultSpiderWeb = "spider.web"

data DownloadOpts = DownloadOpts
    { doRoot :: !Text
    , doCheckOutsideRoot :: !Bool
    , doOutputFile :: !FilePath
    , doWorkers :: !Int
    }
instance IsString DownloadOpts where
    fromString root = DownloadOpts
        { doRoot = reverse $ dropWhile (== '/') $ reverse $ takeWhile (/= '?') $ pack root
        , doCheckOutsideRoot = False -- True
        , doOutputFile = defaultSpiderWeb
        , doWorkers = 8
        }

data DownloadState = DownloadState
    { dsVisited :: !(TVar (HashSet Text))
    , dsQueue :: !(TQueue Text)
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
    atomically $ writeTQueue queue doRoot
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
                    Just item -> do
                        modifyTVar dsActive (+ 1)
                        return $ do
                            restore (handleAny (onError ds item) (oneURL ds item)) `finally`
                                atomically (modifyTVar dsActive (subtract 1))
                            worker ds
        loop

    onError DownloadState {..} item err = atomically $ writeTQueue dsErrors $ concat
        [ "When processing URL "
        , tshow item
        , ", received exception: "
        , pack $ displayException err
        ]

    oneURL DownloadState {..} urlText = do
        req <- parseRequest $ unpack urlText
        hPut stdout $ encodeUtf8 $ concat ["Checking URL: ", urlText, "\n"]
        let uri = getUri req
        case stripPrefix doRoot urlText of
            Nothing
                | doCheckOutsideRoot -> retryHTTP $ httpLBS (setRequestMethod "HEAD" req) >>= checkResponse
                | otherwise -> return ()
            Just suffix -> do
                (fp, h) <- liftIO $ openBinaryTempFile dsTempDir "download"
                res <- retryHTTP $ httpSink req $ \res -> do
                    checkResponse res
                    let contentType = maybe "" (takeWhile (/= _semicolon))
                                    $ lookup "Content-Type"
                                    $ getResponseHeaders res
                    getZipSink $
                        ZipSink (sinkHandle h) *>
                        ZipSink (checkContent uri contentType)
                    return res
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
            Tag.tokenStream .| mapM_C onToken
          where
            onToken (Tag.TagOpen "a" attrs _) = forM_ (lookup "href" attrs) addRoute'
            onToken (Tag.TagOpen "link" attrs _) = forM_ (lookup "href" attrs) addRoute'
            onToken (Tag.TagOpen "img" attrs _) = forM_ (lookup "src" attrs) addRoute'
            onToken (Tag.TagOpen "script" attrs _) = forM_ (lookup "src" attrs) addRoute'
            onToken _ = return ()

            addRoute' = addRoute uri . unpack . takeWhile (/= '#') . decodeUtf8
        checkContent _ "text/css" = return () -- FIXME! Need to implement something here
        checkContent _ _ = return ()

        addRoute _ route | "mailto:" `isPrefixOf` route = return ()
        addRoute _ route | "tel:" `isPrefixOf` route = return ()
        addRoute uri route =
            case URI.parseURIReference route of
                Nothing -> error $ "Invalid URI reference: " ++ show route
                Just ref -> do
                    let route' = tshow $ URI.nonStrictRelativeTo ref uri
                    atomically $ do
                        visisted <- readTVar dsVisited
                        unless (route' `member` visisted) $ do
                            writeTQueue dsQueue route'
                            writeTVar dsVisited $! insertSet route' visisted

retryHTTP :: IO a -> IO a
retryHTTP inner =
    loop 2
  where
    loop 0 = inner
    loop i = inner `catch` \e ->
      case e of
        HttpExceptionRequest _req (ConnectionFailure _) -> loop (i - 1)
        _ -> throwIO e
