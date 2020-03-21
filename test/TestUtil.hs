{-# LANGUAGE FlexibleContexts #-}
module TestUtil
    ( unitTest
    )
where

import           Hedgehog                       ( Property
                                                , PropertyT
                                                )
import qualified Hedgehog
import           Hedgehog.Internal.Source       ( HasCallStack )

unitTest :: HasCallStack => PropertyT IO () -> Property
unitTest = Hedgehog.withTests 1 . Hedgehog.property
