{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module HSnippet.Lib
  ( App
  , AppState(..)
  , appMain
  , waitUntilJust
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Char
import           GHCJS.DOM
import           GHCJS.DOM.Document (getElementById, getBody)
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventM (preventDefault, eventTarget, on)
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement
import           Reflex.Dom hiding (getKeyEvent)
import           Reflex.Dom.Contrib.KeyEvent
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------


type App t m a = ReaderT (AppState t) m a


------------------------------------------------------------------------------
data AppState t = AppState
    { bsKeyDown :: Event t KeyEvent
    , bsKeyPress :: Event t KeyEvent
    }


------------------------------------------------------------------------------
getRoot :: HTMLDocument -> String -> IO HTMLElement
getRoot doc appRootId = waitUntilJust $ liftM (fmap castToHTMLElement) $
                getElementById doc appRootId


------------------------------------------------------------------------------
-- appMain :: String -> (forall t m. MonadWidget t m => App t m ()) -> IO ()
appMain appRootId app = runWebGUI $ \webView -> do
    doc <- waitUntilJust $ liftM (fmap castToHTMLDocument) $
      webViewGetDomDocument webView
    root <- getRoot doc appRootId
    body <- waitUntilJust $ getBody doc
    attachWidget root webView $ do
      let eventTargetAbsorbsKeys = do
            Just t <- liftM (fmap castToHTMLElement) eventTarget
            n <- liftIO $ getTagName t
            return $ n `elem` [Just "INPUT", Just "SELECT", Just "TEXTAREA"]
      liftIO $ (`on` keyDown) body $ do
        ke <- getKeyEvent
        absorbs <- eventTargetAbsorbsKeys
        when (ke == (key $ chr 8) && not absorbs) preventDefault
      let wrapKeypress connectFunc = wrapDomEventMaybe body connectFunc $ do
            ke <- getKeyEvent
            absorbs <- eventTargetAbsorbsKeys
            return $ if absorbs && ke /= (key $ chr 27) -- Let the escape through
                     then Nothing
                     else Just ke
      kd <- wrapKeypress (`on` keyDown)
      kp <- wrapKeypress (`on` keyPress)
      runReaderT app $ AppState kd kp
