{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module HSnippet.Types.App where

------------------------------------------------------------------------------
import Control.Lens
import Data.Pool
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
------------------------------------------------------------------------------

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _db    :: Pool Postgresql
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance ConnectionManager App Postgresql where
    withConn f app = withConn f (_db app)
    withConnNoTransaction f app = withConnNoTransaction f (_db app)


------------------------------------------------------------------------------
type AppHandler = Handler App App

