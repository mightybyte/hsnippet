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
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Configurator
import           Data.Configurator.Types
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
import           System.IO
------------------------------------------------------------------------------
import           GroundhogAuth
import           HSnippet.BuildSnippet
import           HSnippet.BuildTypes
import           HSnippet.Types.App
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
                               (\_ -> handleLogin err) (redirect "/")
    err = Just "Unknown user or password"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("login",       with auth handleLoginSubmit)
         , ("logout",      with auth handleLogout)
         , ("new_user",    with auth handleNewUser)
         , ("heistReload", failIfNotLocal $ with heist heistReloader)
         , ("run",         ghcjsBuildHandler)
         , ("snippets",    serveDirectory "userbuild/snippets")
         , ("packages",    packagesHandler)
         , ("ws",          handleApi)
         , ("",            serveDirectory "static")
         ]


packagesHandler :: Handler App App ()
packagesHandler = do
    ps <- asks (_snippetPackages . _appState)
    writeText $ T.unlines $ map packageName ps

handleApi :: Handler App App ()
handleApi = do
  ps <- asks (_snippetPackages . _appState)
  runWebSocketsSnap $ \pendingConn -> do
    conn <- acceptRequest pendingConn
    forever $ do
      up <- wsReceive conn
      case up of
        Left e -> do
          liftIO $ putStrLn $ "Websocket parse error: " ++ e
          liftIO $ putStrLn $ "On message: " ++ show up
        Right Up_GetPackages -> do
          liftIO $ putStrLn "Got Up_GetPackages"
          wsSend conn $ Down_Packages ps
        Right (Up_RunSnippet t) -> do
          liftIO $ putStrLn "Got Up_RunSnippet"
          handleRunSnippet conn t
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


buildAppState :: Config -> IO AppState
buildAppState conf = do
    let cfg = subconfig "postgres" conf
    connstr <- getConnectionString cfg
    ghPool <- withPostgresqlPool (toS connstr) 3 return

    ps <- getBuildEnvPackages

    return $ AppState ghPool ps

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
    liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) (_db as))

    --a <- nestSnaplet "auth" auth $
    --       initJsonFileAuthManager defAuthSettings sess "users.json"
    a <- nestSnaplet "auth" auth $ initGroundhogAuth sess (_db as)
    addRoutes routes
    addAuthSplices h auth

    -- See if we need to enable heist autoreloading
    -- Commented out because for some reason it's not working
    --ar <- liftIO $ lookupFail "true" conf "autoreload-templates"
    --when ar $ liftIO $ autoReloadHeist "/heistReload" ["snaplets/heist"]

    return $ App h s a as

