{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
import           Reflex.Dom.Contrib.Widgets.EditInPlace
import           Reflex.Dom.Contrib.Xhr
------------------------------------------------------------------------------
import           HSnippet.Lib
------------------------------------------------------------------------------


main :: IO ()
main = appMain app
------------------------------------------------------------------------------
