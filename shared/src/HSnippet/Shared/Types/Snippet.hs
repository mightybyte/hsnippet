{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module HSnippet.Shared.Types.Snippet where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Int
import           Data.Text            (Text)
import           Data.Time
import           Data.Typeable
#ifdef _GHCJS_
import           Telescope.Types.Shared.DefaultKey
#else
import           Control.Monad
import           Database.Groundhog.Core
import           Database.Groundhog.TH
import           Database.PostgreSQL.Simple.FromField
import           HSnippet.Shared.Common
#endif
------------------------------------------------------------------------------

data Snippet = Snippet
    { snippetName        :: Text
    , snippetUid         :: Int64
    , snippetDescription :: Text
    , snippetContents    :: Text
    , snippetCreated     :: UTCTime
    } deriving (Show, Eq, Typeable)

$(deriveJSON defaultOptions ''Snippet)

#ifdef _GHCJS_
type instance DefaultKey Snippet = Int64

#else
instance ToJSON (Key Snippet BackendSpecific) where
  toJSON = toJSON . keyToInt

instance FromJSON (Key Snippet BackendSpecific) where
  parseJSON = liftM intToKey . parseJSON

instance FromField (Key Snippet BackendSpecific) where
  fromField a b = liftM intToKey $ fromField a b

mkPersist ghConfig [groundhog|
- entity: Snippet
  dbName: snippet
|]
#endif
