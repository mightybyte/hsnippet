{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HSnippet.BuildSnippet where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Network.WebSockets
import           Snap.Core
import           System.Directory
import           System.Process
------------------------------------------------------------------------------
import           HSnippet.BuildTypes
import           HSnippet.Shared.Types.BuildResults
import           HSnippet.Shared.Types.SnippetContents
import           HSnippet.Shared.WsApi
import           HSnippet.Websocket
------------------------------------------------------------------------------

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON a = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ a

ghcjsBuilder
    :: MonadIO m
    => (CreateProcess -> SnippetBlob -> Int -> IO (Bool, String, String))
    -> SnippetContents
    -> m BuildResults
ghcjsBuilder runner sc = do
    let sb = mkSnippetBlob sc
    (out, success) <- liftIO $ do
        exists <- doesDirectoryExist $ sbRoot sb
        putStrLn $ "Exists " ++ show exists ++ ": " ++ sbJsOut sb
        if exists
          then do
            out <- readFile $ sbOutput sb
            success <- getSuccess sb
            when (not success) $ removeDirectoryRecursive $ sbRoot sb
            return (out, success)
          else buildSnippet runner sc
    let br = BuildResults (sbName sb) out success
    liftIO $ putStrLn $ "Returning " ++ show br
    return br

handleRunSnippet :: Connection -> SnippetContents -> IO ()
handleRunSnippet conn sc = do
    _ <- forkIO $ do
      br <- ghcjsBuilder (runProcessIncremental conn) sc
      putStrLn "Sending Down_BuildFinished"
      wsSend conn $ Down_BuildFinished br
    return ()
