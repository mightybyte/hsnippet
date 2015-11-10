{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HSnippet.BuildSnippet where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Digest.Pure.SHA
import           Data.String.Conv
import qualified Data.Text.IO         as T
import           Data.Text            (Text)
import           Snap.Core
import           Snap.Snaplet
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
------------------------------------------------------------------------------
import           HSnippet.BuildTypes
import           HSnippet.Types.App
import           HSnippet.Shared.Types.BuildResults
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
      Just (String t) -> do
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
                else buildSnippet t
          let br = BuildResults (sbName sb) out success
          liftIO $ putStrLn $ "Returning " ++ show br
          writeJSON br
      _ -> writeJSON $ String "<h2 class='red'>Error: no data</h2>"

