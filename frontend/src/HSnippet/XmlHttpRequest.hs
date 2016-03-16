{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module HSnippet.XmlHttpRequest where

------------------------------------------------------------------------------
import           Data.Aeson
import           Reflex.Dom hiding (getKeyEvent)
import           Reflex.Dom.Contrib.Xhr
------------------------------------------------------------------------------
import           HSnippet.Shared.Types.BuildResults
------------------------------------------------------------------------------

buildCode
    :: MonadWidget t m
    => Event t String
    -> m (Event t (Maybe BuildResults))
buildCode code = do
    let mkObj a = object ["snippet" .= a]
    fmap snd <$> performJsonAjax (("/run",) . mkObj <$> code)

