{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-|

This module allows you to use the auth snaplet with your user database stored
in a groundhog database.  When you run your application with this snaplet, a
config file will be copied into the the @snaplets/groundhog-auth@ directory.
This file contains all of the configurable options for the snaplet and allows
you to change them without recompiling your application.

To use this snaplet in your application enable the session, groundhog, and auth
snaplets as follows:

> data App = App
>     { ... -- your own application state here
>     , _sess :: Snaplet SessionManager
>     , _db   :: Pool Postgresql
>     , _auth :: Snaplet (AuthManager App)
>     }

Then in your initializer you'll have something like this:

> conf <- getSnapletUserConfig
> let cfg = subconfig "postgres" conf
> connstr <- liftIO $ getConnectionString cfg
> ghPool <- liftIO $ withPostgresqlPool (toS connstr) 3 return
> liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) ghPool)
>
> a <- nestSnaplet "auth" auth $ initGroundhogAuth sess ghPool

If you have not already created the database table for users, it will
automatically be created for you the first time you run your application.

-}

module GroundhogAuth
  ( initGroundhogAuth
  ) where

------------------------------------------------------------------------------
import           Prelude
import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad.Trans.Control
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Pool
import           Data.Readable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.Generic
import           Database.Groundhog.Postgresql
import           Database.Groundhog.TH
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Web.ClientSession (getKey)
--import           Paths_snaplet_groundhog_simple
------------------------------------------------------------------------------


instance NeverNull UserId
instance NeverNull HashedPassword

instance PersistField UserId where
    persistName _ = "UserId"
    toPersistValues (UserId bs) = primToPersistValue bs
    fromPersistValues pvs = do
      (a,vs) <- primFromPersistValue pvs
      return (UserId a, vs)
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField UserId where
    toPrimitivePersistValue proxy (UserId a) = toPrimitivePersistValue proxy a
    fromPrimitivePersistValue proxy v = UserId $ fromPrimitivePersistValue proxy v

instance PersistField HashedPassword where
    persistName _ = "HashedPassword"
    toPersistValues (HashedPassword bs) = primToPersistValue $ T.decodeUtf8 bs
    fromPersistValues pvs = do
      (a,vs) <- primFromPersistValue pvs
      return (HashedPassword $ T.encodeUtf8 a, vs)
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

mkPersist (defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle })
  [groundhog|
    - entity: AuthUser
      dbName: snap_auth_user
  |]

data GroundhogAuthManager = GroundhogAuthManager
    { gamPool     :: Pool Postgresql
    }

------------------------------------------------------------------------------
-- | Initializer for the groundhog backend to the auth snaplet.
--
initGroundhogAuth
  :: SnapletLens b SessionManager  -- ^ Lens to the session snaplet
  -> Pool Postgresql -- ^ The groundhog snaplet
  -> SnapletInit b (AuthManager b)
initGroundhogAuth sess pool = makeSnaplet "groundhog-auth" desc datadir $ do
    authSettings <- authSettingsFromConfig
    key <- liftIO $ getKey (asSiteKey authSettings)
    let manager = GroundhogAuthManager pool
    let migrateDB = runMigration $ migrate (undefined :: AuthUser)
    liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) pool)
    rng <- liftIO mkRNG
    return $ AuthManager
      { backend = manager
      , session = sess
      , activeUser = Nothing
      , minPasswdLen = asMinPasswdLen authSettings
      , rememberCookieName = asRememberCookieName authSettings
      , rememberCookieDomain = Nothing
      , rememberPeriod = asRememberPeriod authSettings
      , siteKey = key
      , lockout = asLockout authSettings
      , randomNumberGenerator = rng
      }
  where
    desc = "A Groundhog backend for user authentication"
    datadir = Nothing --Just $ liftM (++"/resources/auth") getDataDir


onFailure :: Monad m => E.SomeException -> m (Either AuthFailure a)
onFailure e = return $ Left $ AuthError $ show e

setUid :: DefaultKey AuthUser -> AuthUser -> AuthUser
setUid k u = u { userId = Just $ UserId $ T.pack $ show $ keyToInt k }

runGH
    :: (MonadBaseControl IO m, MonadIO m)
    => Pool Postgresql
    -> DbPersist Postgresql (NoLoggingT m) a
    -> m a
runGH pool action = runNoLoggingT (withConn (runDbPersist action) pool)

------------------------------------------------------------------------------
-- |
instance IAuthBackend GroundhogAuthManager where
    save GroundhogAuthManager{..} u@AuthUser{..} = do
      case fromText . unUid =<< userId of
        Nothing -> do
          k <- runGH gamPool $ insert u
          return $ Right $ setUid k u
        Just uid -> do
          let go = runGH gamPool $ do
                replace (intToKey uid) u >> return (Right u)
          E.catch go onFailure


    lookupByUserId GroundhogAuthManager{..} uid = do
      case fromText (unUid uid) of
        Nothing -> return Nothing
        Just keyInt -> do
            let key = intToKey keyInt
            mu <- runGH gamPool $ get key
            return $ setUid key <$> mu

    lookupByLogin GroundhogAuthManager{..} login = do
      res <- runGH gamPool $
               project (AutoKeyField, AuthUserConstructor)
                       (UserLoginField ==. login)
      case res of
        [] -> return Nothing
        (k,u):_ -> return $ Just $ setUid k u

    lookupByRememberToken GroundhogAuthManager{..} token = do
      res <- runGH gamPool $
               project (AutoKeyField, AuthUserConstructor)
                       (UserRememberTokenField ==. Just token)
      case res of
        [] -> return Nothing
        (k,u):_ -> return $ Just $ setUid k u

    destroy GroundhogAuthManager{..} AuthUser{..} = do
      runGH gamPool $ delete (UserLoginField ==. userLogin)

pg :: proxy Postgresql
pg = undefined


-------------------------------------------------------------------------------
keyToInt
    :: (PrimitivePersistField (Key a b))
    => Key a b
    -> Int
keyToInt = keyToIntegral


-------------------------------------------------------------------------------
-- | Convert 'Key' to any integral type.
keyToIntegral
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => Key a b
    -> i
keyToIntegral =
    fromPrimitivePersistValue pg . toPrimitivePersistValue pg


-------------------------------------------------------------------------------
-- | Type specialized input for type inference convenience.
intToKey
    :: (PrimitivePersistField (Key a b))
    => Int
    -> Key a b
intToKey = integralToKey


-------------------------------------------------------------------------------
-- | Convert any integral type to 'Key'
integralToKey
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => i
    -> Key a b
integralToKey =
    fromPrimitivePersistValue pg . toPrimitivePersistValue pg
