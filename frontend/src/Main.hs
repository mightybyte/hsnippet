{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

------------------------------------------------------------------------------
import           HSnippet
import           HSnippet.Lib
------------------------------------------------------------------------------


------------------------------------------------------------------------------
main :: IO ()
main = appMain "hsnippet-app" runApp
