{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HSnippet.BuildSnippet where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.String.Conv
import           Data.Text (Text)
import           Network.WebSockets
import           Snap.Core
import           Snap.Snaplet
import           System.Directory
import           System.Process
------------------------------------------------------------------------------
import           HSnippet.BuildTypes
import           HSnippet.Types.App
import           HSnippet.Shared.Types.BuildResults
import           HSnippet.Shared.WsApi
import           HSnippet.Websocket
------------------------------------------------------------------------------

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON a = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ a

ghcjsBuildHandler :: Handler App App ()
ghcjsBuildHandler = do
    setTimeout 300
    mbs <- getParam "snippet"
    case decode . toS =<< mbs of
      Just (String t) -> writeJSON =<< ghcjsBuilder runProcessBatch t
      _ -> writeJSON $ String "<h2 class='red'>Error: no data</h2>"

ghcjsBuilder
    :: MonadIO m
    => (CreateProcess -> SnippetBlob -> Int -> IO (Bool, String, String))
    -> Text
    -> m BuildResults
ghcjsBuilder runner t = do
    let sb = mkSnippetBlob t
    (out, success) <- liftIO $ do
        exists <- doesDirectoryExist $ sbRoot sb
        putStrLn $ "Exists " ++ show exists ++ ": " ++ sbJsOut sb
        if exists
          then do
            out <- readFile $ sbOutput sb
            success <- getSuccess sb
            when (not success) $ removeDirectoryRecursive $ sbRoot sb
            return (out, success)
          else buildSnippet runner t
    let br = BuildResults (sbName sb) out success
    liftIO $ putStrLn $ "Returning " ++ show br
    return br

handleRunSnippet :: Connection -> Text -> IO ()
handleRunSnippet conn t = do
    _ <- forkIO $ do
      br <- ghcjsBuilder (runProcessIncremental conn) t
      putStrLn "Sending Down_BuildFinished"
      wsSend conn $ Down_BuildFinished br
    return ()
