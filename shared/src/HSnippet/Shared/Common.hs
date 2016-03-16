{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module HSnippet.Shared.Common
  ( keyToInt
  , keyToIntegral
  , intToKey
  , integralToKey
  , ghConfig
  ) where

------------------------------------------------------------------------------
#ifndef __GHCJS__
import           Database.Groundhog.Core
import           Database.Groundhog.Postgresql
import           Database.Groundhog.TH
#endif
------------------------------------------------------------------------------


#ifdef __GHCJS__

type family DefaultKey a :: *

#else

ghConfig :: CodegenConfig
ghConfig = defaultCodegenConfig
    { namingStyle = lowerCaseSuffixNamingStyle }

pg :: proxy Postgresql
pg = undefined


-------------------------------------------------------------------------------
keyToInt
    :: (PrimitivePersistField (Key a b))
    => Key a b
    -> Int
keyToInt = keyToIntegral


-------------------------------------------------------------------------------
-- | Convert 'Key' to any integral type.
keyToIntegral
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => Key a b
    -> i
keyToIntegral =
    fromPrimitivePersistValue pg . toPrimitivePersistValue pg


-------------------------------------------------------------------------------
-- | Type specialized input for type inference convenience.
intToKey
    :: (PrimitivePersistField (Key a b))
    => Int
    -> Key a b
intToKey = integralToKey


-------------------------------------------------------------------------------
-- | Convert any integral type to 'Key'
integralToKey
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => i
    -> Key a b
integralToKey =
    fromPrimitivePersistValue pg . toPrimitivePersistValue pg

#endif
