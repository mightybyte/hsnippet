{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module ACE where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Trans
import           Data.Dependent.Sum (DSum (..))
import           Data.Monoid
import           GHCJS.DOM.Types hiding (Event, Text)
#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign.Callback
import           GHCJS.Types
#endif
import           Reflex
import           Reflex.Host.Class
import           Reflex.Dom
------------------------------------------------------------------------------


newtype AceRef = AceRef { unAceRef :: JSVal }

data ACE t = ACE
    { aceRef :: Dynamic t (Maybe AceRef)
    , aceValue :: Dynamic t String
    }

------------------------------------------------------------------------------
startACE :: String -> IO AceRef
#ifdef ghcjs_HOST_OS
startACE = js_startACE . toJSString

foreign import javascript unsafe
  "(function(){ var a = ace['edit']($1); a.session.setMode(\"ace/mode/haskell\"); return a; })()"
  js_startACE :: JSString -> IO AceRef
#else
startACE = error "startACE: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
moveCursorToPosition :: AceRef -> (Int, Int) -> IO ()
#ifdef ghcjs_HOST_OS
moveCursorToPosition a (r,c) = js_moveCursorToPosition a r c

foreign import javascript unsafe
  "(function(){ $1['gotoLine']($2, $3, true); })()"
  js_moveCursorToPosition :: AceRef -> Int -> Int -> IO ()
#else
moveursorToPosition = error "moveCursorToPosition: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
aceGetValue :: AceRef -> IO String
#ifdef ghcjs_HOST_OS
aceGetValue a = fromJSString <$> js_aceGetValue a

foreign import javascript unsafe
  "(function(){ return $1['getValue'](); })()"
  js_aceGetValue :: AceRef -> IO JSString
#else
aceGetValue = error "aceGetValue: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
setValueACE :: AceRef -> String -> IO ()
#ifdef ghcjs_HOST_OS
setValueACE a = js_aceSetValue a . toJSString

foreign import javascript unsafe
  "(function(){ $1['setValue']($2, -1); })()"
  js_aceSetValue :: AceRef -> JSString -> IO ()
#else
setValueACE = error "setValueACE: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
setupValueListener :: MonadWidget t m => AceRef -> m (Event t String)
#ifdef ghcjs_HOST_OS
setupValueListener ace = do
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    e <- newEventWithTrigger $ \et -> do
          cb <- asyncCallback1 $ \_ -> liftIO $ do
              v <- aceGetValue ace
              postGui $ runWithActions [et :=> Identity v]
          js_setupValueListener ace cb
          return (return ())
          -- TODO Probably need some kind of unsubscribe mechanism
          --return $ liftIO unsubscribe
    return $! e

foreign import javascript unsafe
  "(function(){ $1['on'](\"change\", $2); })()"
  js_setupValueListener :: AceRef -> Callback (JSVal -> IO ()) -> IO ()
#else
setupValueListener = error "setupValueListener: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
aceWidget :: MonadWidget t m => String -> m (ACE t)
aceWidget initContents = do
    let elemId = "editor"
    elAttr "pre" ("id" =: elemId <> "class" =: "ui segment") $ text initContents

    pb <- getPostBuild
    aceUpdates <- performEvent (liftIO (startACE "editor") <$ pb)
    res <- widgetHold (return never) $ setupValueListener <$> aceUpdates
    aceDyn <- holdDyn Nothing $ Just <$> aceUpdates
    updatesDyn <- holdDyn initContents $ switchPromptlyDyn res

    return $ ACE aceDyn updatesDyn


------------------------------------------------------------------------------
aceSetValue :: MonadWidget t m => ACE t -> Event t String -> m ()
aceSetValue ace val =
    performEvent_ $ attachDynWith f (aceRef ace) val
  where
    f Nothing _ = return ()
    f (Just ref) pos = liftIO $ setValueACE ref pos


------------------------------------------------------------------------------
aceMoveCursor :: MonadWidget t m => ACE t -> Event t (Int,Int) -> m ()
aceMoveCursor ace posE =
    performEvent_ $ attachDynWith f (aceRef ace) posE
  where
    f Nothing _ = return ()
    f (Just ref) pos = liftIO $ moveCursorToPosition ref pos

