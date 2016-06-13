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

module SemanticUI where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Trans
import           Data.Dependent.Sum (DSum (..))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           GHCJS.DOM.Types hiding (Event, Text)
#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign.Callback
import           GHCJS.Foreign
import           GHCJS.Marshal.Internal
import           GHCJS.Types
#endif
import           Reflex
import           Reflex.Host.Class
import           Reflex.Dom
------------------------------------------------------------------------------


------------------------------------------------------------------------------
activateSemUiDropdown :: String -> IO ()
#ifdef ghcjs_HOST_OS
activateSemUiDropdown = js_activateSemUiDropdown . toJSString

foreign import javascript unsafe
  "$($1).dropdown({fullTextSearch: true});"
  js_activateSemUiDropdown :: JSString -> IO ()
#else
activateSemUiDropdown =
  error "activateSemUiDropdown: can only be used with GHCJS"
#endif


data DropdownMulti t a = DropdownMulti
    { _dm_value :: Dynamic t a
    }

data DropdownMultiConfig a = DropdownMultiConfig
    { _dmc_initialValue :: a
    , _dmc_fullTextSearch :: Bool
    , _dmc_elementId :: String
    }

------------------------------------------------------------------------------
#ifdef ghcjs_HOST_OS
activateSemUiDropdownMulti dmc = do
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    e <- newEventWithTrigger $ \et -> do
           cb <- asyncCallback3 $ \_ t _ -> liftIO $ do
               let v = read $ fromJSString $ pFromJSVal t
               postGui $ runWithActions [et :=> Identity v]
           js_activateSemUiDropdownMulti
             (toJSString $ _dmc_elementId dmc) cb
             (toJSBool $ _dmc_fullTextSearch dmc)
           return (return ())
           -- TODO Probably need some kind of unsubscribe mechanism
           --return $ liftIO unsubscribe
    val <- holdDyn (_dmc_initialValue dmc) e
    return $! DropdownMulti val

foreign import javascript unsafe
  "(function(){ $($1).dropdown({onChange: $2, fullTextSearch: $3}); })()"
  js_activateSemUiDropdownMulti
    :: JSString
    -> Callback (JSVal -> JSVal -> JSVal -> IO ())
    -> JSVal
    -> IO ()
#else
activateSemUiDropdownMulti =
  error "activateSemUiDropdownMulti: can only be used with GHCJS"
#endif


-- Multi-select sem-ui dropdown is not working properly yet.  Not sure how
-- to get the current value.
--
-- $('#dropdownId').dropdown('get value')
-- $('#dropdownId').onChange(value, text, $choice)

-- | Wrapper around the reflex-dom dropdown that calls the sem-ui dropdown
-- function after the element is built.
semUiDropdownMulti
    :: (Ord a, Read a, Show a, MonadWidget t m)
    => String
       -- ^ Element id.  Ideally this should be randomly generated instead
       -- of passed in as an argument, but for now this approach is easier.
    -> a
       -- ^ Initial value
    -> Dynamic t (Map a String)
    -> Map String [Char]
    -> m (Dynamic t String)
semUiDropdownMulti elId iv vals attrs = do
    let f vs = semUiDropdownMulti' elId iv vs attrs
    res <- dyn =<< mapDyn f (traceDynWith (((elId ++ " values changed ") ++) . show . length) vals)
    joinDyn <$> holdDyn (constDyn $ show iv) res

-- | Wrapper around the reflex-dom dropdown that calls the sem-ui dropdown
-- function after the element is built.
semUiDropdownMulti'
    :: (Ord a, Read a, Show a, MonadWidget t m)
    => String
       -- ^ Element id.  Ideally this should be randomly generated instead
       -- of passed in as an argument, but for now this approach is easier.
    -> a
       -- ^ Initial value
    -> Map a String
    -> Map String [Char]
    -> m (Dynamic t String)
semUiDropdownMulti' elId iv vals attrs = do
    d <- dropdown (show iv) (constDyn $ M.mapKeys show vals) $ def &
      attributes .~ (constDyn $ attrs <> ("id" =: elId))
    pb <- getPostBuild
    performEvent_ (liftIO (activateSemUiDropdown ('#':elId)) <$ pb)
    return $ value d
