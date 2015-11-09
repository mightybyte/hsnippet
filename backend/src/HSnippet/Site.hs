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
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Monoid
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.Postgresql
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
import           HSnippet.Reload
import           HSnippet.Types.App
import           HSnippet.Shared.Types.Snippet
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = render "login"


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
         , ("snippets",    serveDirectory "sandbox/snippets")
         , ("",            serveDirectory "static")
         ]


migrateDB :: (MonadIO m, PersistBackend m) => m ()
migrateDB = do
    runMigration $ do
      migrate (undefined :: Snippet)

------------------------------------------------------------------------------
lookupFail
    :: (Show b, Configured b)
    => String
    -- ^ A default value
    -> Config
    -> Text
    -- ^ Key beeing looked up
    -> IO b
lookupFail def cfg key = do
    val <- lookupDefault (error msg) cfg key
    putStrLn $ "Configured " <> T.unpack key <> " = " <> show val
    return val
  where
    msg = "Missing config option " <> T.unpack key <>
          "! (usually defaults to \"" <> def <> "\")"


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
    let cfg = subconfig "postgres" conf
    connstr <- liftIO $ getConnectionString cfg
    ghPool <- liftIO $ withPostgresqlPool (toS connstr) 3 return
    liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) ghPool)

    --a <- nestSnaplet "auth" auth $
    --       initJsonFileAuthManager defAuthSettings sess "users.json"
    a <- nestSnaplet "auth" auth $ initGroundhogAuth sess ghPool
    addRoutes routes
    addAuthSplices h auth

    -- See if we need to enable heist autoreloading
    -- Commented out because for some reason it's not working
    --ar <- liftIO $ lookupFail "true" conf "autoreload-templates"
    --when ar $ liftIO $ autoReloadHeist "/heistReload" ["snaplets/heist"]

    return $ App h s a ghPool

