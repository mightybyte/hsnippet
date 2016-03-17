{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module HSnippet.Websocket where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Network.WebSockets
------------------------------------------------------------------------------


wsReceive :: FromJSON a => Connection -> IO (Either String a)
wsReceive conn = do
    dm <- receiveDataMessage conn
    return $ eitherDecode' $ dataToBs dm

wsSend :: ToJSON a => Connection -> a -> IO ()
wsSend conn v = sendTextData conn $ encode v

dataToBs :: DataMessage -> ByteString
dataToBs (Text bs) = bs
dataToBs (Binary bs) = bs
