{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module HSnippet.DBLayer where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           HSnippet.Shared.Types.Package
import           HSnippet.Shared.WsApi
------------------------------------------------------------------------------

data DBLayer t = DBLayer
    { dbPackages :: Dynamic t [Package]
    }

startDbLayer
    :: MonadWidget t m
    => Event t [Up]
    -> m (DBLayer t)
startDbLayer upEvents = do
    pb <- getPostBuild
    rec (downEvents, startEvent) <- openWebSocket $ mergeWith (++)
          [ [Up_GetPackages] <$ pb
          , upEvents
          ]
    packages <- holdDyn [] $ fmapMaybe foo downEvents
    return $ DBLayer packages

foo :: Maybe Down -> Maybe [Package]
foo md = (^? _Down_Packages) =<< md

gateDyn :: Reflex t => Dynamic t Bool -> Event t a -> Event t a
gateDyn d e = attachDynWithMaybe (\b a -> if b then Just a else Nothing) d e

openWebSocket
    :: MonadWidget t m
    => Event t [Up]
    -> m (Event t (Maybe Down), Event t ())
openWebSocket wsUp = do
    wv <- askWebView
    host :: String <- liftIO $ getLocationHost wv
    protocol :: String <- liftIO $ getLocationProtocol wv
    let wsProtocol = case protocol of
          "" -> "ws:" -- We're in GHC
          "about:" -> "ws:" -- We're in GHC
          "file:" -> "ws:"
          "http:" -> "ws:"
          "https:" -> "wss:"
          _ -> error $ "Unrecognized protocol: " <> show protocol
        wsHost = case protocol of
          "" -> "localhost:8000" -- We're in GHC
          "about:" -> "localhost:8000" -- We're in GHC
          "file:" -> "localhost:8000"
          _ -> host
    rec ws <- webSocket (wsProtocol <> "//" <> wsHost <> "/ws") $
          def & webSocketConfig_send .~ send
        websocketReady <- holdDyn False $ fmap (const True) $ _webSocket_open ws
        websocketNotReady <- mapDyn not websocketReady
        buffer <- foldDyn (++) [] $ gateDyn websocketNotReady wsUp
        let send = fmap (fmap (LBS.toStrict . encode)) $ leftmost [ gateDyn websocketReady wsUp
                                                                  , tag (current buffer) (_webSocket_open ws)
                                                                  ]
    return $ (fmap (decode' . LBS.fromStrict)$ _webSocket_recv ws, _webSocket_open ws)
