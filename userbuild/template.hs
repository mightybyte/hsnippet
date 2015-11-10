{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Colour.Palette.BrewerSet
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Diagrams.Backend.Reflex
import qualified Diagrams.Prelude as D
import           GHC.Generics
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.KeyEvent
import           Reflex.Dom.Contrib.Pagination
import           Reflex.Dom.Contrib.Utils
import           Reflex.Dom.Contrib.Widgets.EditInPlace
import           Reflex.Dom.Contrib.Widgets.Svg
import           Reflex.Dom.Contrib.Xhr
------------------------------------------------------------------------------
import           HSnippet.Lib
------------------------------------------------------------------------------


importAccessDenied :: Bool
importAccessDenied = True
------------------------------------------------------------------------------

