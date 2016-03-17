{-# LANGUAGE TemplateHaskell       #-}

module HSnippet.Shared.Types.BuildMessage where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson.TH
import           Data.Text (Text)
------------------------------------------------------------------------------

data MessageType = BuildError | BuildWarning
  deriving (Eq,Ord,Show,Read)

data BuildMessage = BuildMessage
    { _bmFile  :: Text
    , _bmLine  :: Int
    , _bmCol   :: Int
    , _bmType  :: MessageType
    , _bmLines :: [Text]
    } deriving (Eq,Ord,Show,Read)

makeLenses ''BuildMessage

deriveJSON defaultOptions ''MessageType
deriveJSON defaultOptions ''BuildMessage

