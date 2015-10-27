{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module HSnippet where

------------------------------------------------------------------------------
import           Reflex.Dom hiding (getKeyEvent)
import           Reflex.Dom.Contrib.KeyEvent
------------------------------------------------------------------------------

data AppState t = AppState
    { bsKeyDown :: Event t KeyEvent
    , bsKeyPress :: Event t KeyEvent
    }

runApp :: MonadWidget t m => m ()
runApp = text "Hello World"
