{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module HSnippet.Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.String.Conv
import qualified Data.Text as T
import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.Postgresql
import           Network.WebSockets
import           Network.WebSockets.Snap
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple (getConnectionString)
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath
import           System.IO
------------------------------------------------------------------------------
import           GroundhogAuth
import           HSnippet.BuildSnippet
import           HSnippet.BuildTypes
import           HSnippet.Types.App
import           HSnippet.Shared.Types.ExampleSnippet
import           HSnippet.Shared.Types.Package
import           HSnippet.Shared.Types.Snippet
import           HSnippet.Shared.WsApi
import           HSnippet.Websocket
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin _ = render "login"


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (writeText . toS . show) (redirect "/")


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = do
        res <- registerUser "login" "password"
        case res of
          Left e -> writeText $ T.pack $ "Error creating user: " ++ show e
          Right _ -> loginUser "login" "password" Nothing
                               (\_ -> handleLogin $ Just "Unknown user or password")
                               (redirect "/")

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("login",       with auth handleLoginSubmit)
         , ("logout",      with auth handleLogout)
         , ("new_user",    with auth handleNewUser)
         , ("heistReload", failIfNotLocal $ with heist heistReloader)
         , ("snippets",    serveDirectory "userbuild/snippets")
         , ("packages",    packagesHandler)
         , ("ws",          handleApi)
         , ("exports",     moduleExportsHandler)
         , ("",            serveDirectory "static")
         ]


moduleExportsHandler :: Handler App App ()
moduleExportsHandler = do
    mm <- getParam "module"
    case mm of
      Nothing -> writeText "Must supply a module"
      Just nm -> do
        let m = Module $ toS nm
        mem <- asks (_appStateModules . _appState)
        es <- liftIO $ foo mem m
        writeText $ T.unlines $ map exportName es

foo :: Map Module [Package] -> Module -> IO [Export]
foo mem m =
    case packageLibDir =<< headMay (fromMaybe [] $ M.lookup m mem) of
      Nothing -> return []
      Just libDir -> getModuleExports libDir m

packagesHandler :: Handler App App ()
packagesHandler = do
    ps <- asks (_appStatePackages . _appState)
    writeText $ T.unlines $ map packageName ps

handleApi :: Handler App App ()
handleApi = do
  ps <- asks (_appStatePackages . _appState)
  es <- asks (_appStateExamples . _appState)
  mem <- asks (_appStateModules . _appState)
  runWebSocketsSnap $ \pendingConn -> do
    conn <- acceptRequest pendingConn
    putStrLn "Forking ping thread..."
    forkPingThread conn 10
    forever $ do
      up <- wsReceive conn
      case up of
        Left e -> do
          putStrLn $ "Websocket parse error: " ++ e
          putStrLn $ "On message: " ++ show up
        Right Up_GetPackages -> do
          putStrLn "Got Up_GetPackages"
          wsSend conn $ Down_Packages ps
        Right Up_GetExamples -> do
          putStrLn "Got Up_GetExamples"
          wsSend conn $ Down_Examples es
        Right (Up_RunSnippet t) -> do
          putStrLn "Got Up_RunSnippet"
          handleRunSnippet conn t
        Right (Up_GetExports m) -> do
          putStrLn "Got Up_GetExports"
          exports <- foo mem m
          wsSend conn $ Down_Exports (m, exports)

      return ()


migrateDB :: (MonadIO m, PersistBackend m) => m ()
migrateDB = do
    runMigration $ do
      migrate (undefined :: Snippet)

------------------------------------------------------------------------------
--lookupFail
--    :: (Show b, Configured b)
--    => String
--    -- ^ A default value
--    -> Config
--    -> Text
--    -- ^ Key beeing looked up
--    -> IO b
--lookupFail def cfg key = do
--    val <- lookupDefault (error msg) cfg key
--    putStrLn $ "Configured " <> T.unpack key <> " = " <> show val
--    return val
--  where
--    msg = "Missing config option " <> T.unpack key <>
--          "! (usually defaults to \"" <> def <> "\")"


getExamples :: IO [ExampleSnippet]
getExamples = do
    contents <- filter notDots <$> getDirectoryContents exampleDir
    mapM mkExample contents
  where
    exampleDir = "userbuild/examples"
    mkExample nm = do
      code <- readFile (exampleDir </> nm)
      return $ ExampleSnippet (dropExtension nm) code
    notDots n = n /= "." && n /= ".."

buildAppState :: Config -> IO AppState
buildAppState conf = do
    let cfg = subconfig "postgres" conf
    connstr <- getConnectionString cfg
    ghPool <- withPostgresqlPool (toS connstr) 3 return

    ps <- getPackageDump
    let ms = M.unionsWith (++) $ map packageToMap ps
    exs <- getExamples

    return $ AppState ghPool ps exs ms

packageToMap :: Package -> Map Module [Package]
packageToMap p = M.fromList $ map (\m -> (m, [p])) $ packageModules p

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    liftIO $ do
      hSetBuffering stdout NoBuffering
      hSetBuffering stderr NoBuffering
    h <- nestSnaplet "" heist $ heistInit ""
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    conf <- getSnapletUserConfig
    as <- liftIO $ buildAppState conf
    liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) (_appStateDb as))

    --a <- nestSnaplet "auth" auth $
    --       initJsonFileAuthManager defAuthSettings sess "users.json"
    a <- nestSnaplet "auth" auth $ initGroundhogAuth sess (_appStateDb as)
    addRoutes routes
    addAuthSplices h auth

    -- See if we need to enable heist autoreloading
    -- Commented out because for some reason it's not working
    --ar <- liftIO $ lookupFail "true" conf "autoreload-templates"
    --when ar $ liftIO $ autoReloadHeist "/heistReload" ["snaplets/heist"]

    return $ App h s a as

