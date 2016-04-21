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

module HSnippet.FrontendState where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           HSnippet.Shared.Types.BuildMessage
import           HSnippet.Shared.Types.BuildResults
import           HSnippet.Shared.Types.ExampleSnippet
import           HSnippet.Shared.Types.Package
import           HSnippet.Shared.Types.SnippetContents
import           HSnippet.Shared.Types.SnippetImport
import           HSnippet.Shared.WsApi
------------------------------------------------------------------------------

data LeftColumnOut t = LeftColumnOut
    { lcoCode           :: Dynamic t String
    , lcoImports        :: Dynamic t [SnippetImport]
    , lcoRefreshImports :: Event t ()
    , lcoModuleUpdates  :: Event t Module
    }

data BuildStatus = NotBuilt | Building | BuildFailed | Built BuildResults
  deriving (Eq,Show,Ord)

data FrontendState t = FrontendState
    { fsPackages      :: Dynamic t [Package]
    , fsBuildStatus   :: Dynamic t BuildStatus
    , fsBuildOut      :: Dynamic t [Either Text BuildMessage]
    , fsExamples      :: Dynamic t [ExampleSnippet]
    , fsModuleExports :: Dynamic t (Map Module [Export])
    }

mkBuildStatus
    :: MonadWidget t m
    => Event t (Maybe Down)
    -> Event t ()
    -> m (Dynamic t BuildStatus)
mkBuildStatus downEvents buildSnippet = do
    holdDyn NotBuilt $ leftmost
      [ Building <$ buildSnippet
      , resultsToStatus <$> fmapMaybe (isMsg _Down_BuildFinished) downEvents
      ]
  where
    resultsToStatus br = if brSuccess br then Built br else BuildFailed

stateManager
    :: MonadWidget t m
    => LeftColumnOut t
    -> Event t ()
    -> m (FrontendState t)
stateManager lco buildSnippet = do
    pb <- getPostBuild
    let importBlock = T.unlines . map (T.pack . renderImport) <$>
                      current (lcoImports lco)
        inMsg = SnippetContents
               <$> (T.pack <$> current (lcoCode lco))
               <*> importBlock
    let upEvent = mergeWith (++) $ map (fmap (:[]))
          [ Up_GetPackages <$ leftmost [pb, lcoRefreshImports lco]
          , Up_GetExamples <$ pb
          , Up_RunSnippet <$> tag inMsg buildSnippet
          , Up_GetExports <$> lcoModuleUpdates lco
          ]
    (downEvent, _) <- openWebSocket upEvent
    buildStatus <- mkBuildStatus downEvent buildSnippet
    packages <- holdDyn [] $ fmapMaybe (isMsg _Down_Packages) downEvent
    examples <- holdDyn [] $ fmapMaybe (isMsg _Down_Examples) downEvent
    buildOut <- foldDyn ($) [] $ leftmost
      [ (flip (++)) <$> fmapMaybe (isMsg _Down_BuildOutLine) downEvent
      , const [] <$ buildSnippet
      ]
    exportMap <- foldDyn ($) M.empty $
      uncurry M.insert <$> fmapMaybe (isMsg _Down_Exports) downEvent
    return $ FrontendState packages buildStatus buildOut examples exportMap

--foo :: Maybe Down -> Maybe [Package]
isMsg :: Getting (First b) a b -> Maybe a -> Maybe b
isMsg constructorPrism md = (^? constructorPrism) =<< md

gateDyn :: Reflex t => Dynamic t Bool -> Event t a -> Event t a
gateDyn d e = attachDynWithMaybe (\b a -> if b then Just a else Nothing) d e

------------------------------------------------------------------------------
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
        websocketReady <- holdDyn False $ True <$ _webSocket_open ws
        websocketNotReady <- mapDyn not websocketReady
        buffer <- foldDyn (++) [] $ gateDyn websocketNotReady wsUp
        let send = (fmap (LBS.toStrict . encode)) <$> leftmost
                     [ gateDyn websocketReady wsUp
                     , tag (current buffer) (_webSocket_open ws)
                     ]
    let rawReceived = _webSocket_recv ws
    return $ (decode' . LBS.fromStrict <$> rawReceived, _webSocket_open ws)
