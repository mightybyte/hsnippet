module HSnippet.Reload where

-------------------------------------------------------------------------------
import           Control.Monad
import           Snap.Core
import           Snap.Http.Server    as S
import           Snap.Snaplet.Config
import           System.FSNotify
import           System.Process
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig S.defaultConfig


-------------------------------------------------------------------------------
autoReloadHeist :: String -> [FilePath] -> IO ()
autoReloadHeist r dirs = do
    mgr <- startManager
    cfg <- getConf
    let port = getPort cfg
    case port of
      Nothing -> return ()
      Just p ->
        forM_ dirs $ \ d ->
          watchTree mgr d (const True) (reload r p)


-------------------------------------------------------------------------------
reload :: Show a => String -> a -> Event -> IO ()
reload r p e = do
    -- Debug print statements because I can't figure out why this isn't working
    putStrLn $ "FSWatch got event " ++ show e
    putStrLn $ "Reload running command: " ++ unwords ("curl":[uri])
    void $ rawSystem "curl" [uri]
    putStrLn $ "Done."
  where
    uri = "http://localhost:" ++ show p ++ r
