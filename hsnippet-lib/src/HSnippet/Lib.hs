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
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventM (preventDefault, eventTarget)
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement
import           Reflex.Dom hiding (getKeyEvent)
import           Reflex.Dom.Contrib.KeyEvent
------------------------------------------------------------------------------


type App t m a = ReaderT (AppState t) m a


------------------------------------------------------------------------------
data AppState t = AppState
    { bsKeyDown :: Event t KeyEvent
    , bsKeyPress :: Event t KeyEvent
    }


------------------------------------------------------------------------------
waitUntilJust :: IO (Maybe a) -> IO a
waitUntilJust a = do
    mx <- a
    case mx of
      Just x -> return x
      Nothing -> do
        threadDelay 10000
        waitUntilJust a


------------------------------------------------------------------------------
--appMain :: MonadWidget t m => String -> App t m () -> IO ()
appMain app = runWebGUI $ \webView -> do
    doc <- waitUntilJust $ liftM (fmap castToHTMLDocument) $
      webViewGetDomDocument webView
    let btag = "snippet-output"
    root <- waitUntilJust $ liftM (fmap castToHTMLElement) $
      documentGetElementById doc btag
    body <- waitUntilJust $ documentGetBody doc
    attachWidget root webView $ do
      let eventTargetAbsorbsKeys = do
            Just t <- liftM (fmap castToHTMLElement) eventTarget
            n <- liftIO $ elementGetTagName t
            return $ n `elem` ["INPUT", "SELECT", "TEXTAREA"]
      liftIO $ elementOnkeydown body $ do
        ke <- getKeyEvent
        absorbs <- eventTargetAbsorbsKeys
        when (ke == (key $ chr 8) && not absorbs) preventDefault
      let wrapKeypress connectFunc = wrapDomEventMaybe body connectFunc $ do
            ke <- getKeyEvent
            absorbs <- eventTargetAbsorbsKeys
            return $ if absorbs && ke /= (key $ chr 27) -- Let the escape through
                     then Nothing
                     else Just ke
      keyDown <- wrapKeypress elementOnkeydown
      keyPress <- wrapKeypress elementOnkeypress
      runReaderT app $ AppState keyDown keyPress
