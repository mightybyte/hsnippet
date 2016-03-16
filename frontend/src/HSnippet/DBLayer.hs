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
import           HSnippet.Tabs
import           HSnippet.Shared.Types.BuildResults
import           HSnippet.Shared.WsApi
import           HSnippet.XmlHttpRequest
------------------------------------------------------------------------------

data DBLayer t = DBLayer
    { dbPackages :: Dynamic t [Package]
    }
