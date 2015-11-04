{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
import           Reflex.Dom.Contrib.Widgets.EditInPlace
------------------------------------------------------------------------------
import           HSnippet.Lib
------------------------------------------------------------------------------


------------------------------------------------------------------------------
