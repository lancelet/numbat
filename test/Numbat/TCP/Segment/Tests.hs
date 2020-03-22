{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
module Numbat.TCP.Segment.Tests
    ( tests
    )
where

import           Hedgehog                       ( MonadGen
                                                , Property
                                                , forAll
                                                , property
                                                , (===)
                                                )
import qualified Hedgehog.Gen                  as Gen
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.Hedgehog            ( testProperty )
import           TestUtil                       ( unitTest )

import           Data.Word                      ( Word16 )

import           Numbat.TCP.Segment             ( ControlBit(Off, On)
                                                , ControlBits
                                                    ( ControlBits
                                                    , controlBitsACK
                                                    , controlBitsCRW
                                                    , controlBitsECE
                                                    , controlBitsFIN
                                                    , controlBitsNS
                                                    , controlBitsPSH
                                                    , controlBitsRST
                                                    , controlBitsSYN
                                                    , controlBitsURG
                                                    )
                                                , decodeControlBits
                                                , encodeControlBits
                                                , zeroControlBits
                                                )

tests :: TestTree
tests = testGroup
    "Numbat.TCP.Segment.Tests"
    [ testProperty "unit: encodeControlBits" unit_encodeControlBits
    , testProperty "prop: ControlBits encode/decode round-trip"
                   prop_tripControlBits
    ]

---- Unit Tests

unit_encodeControlBits :: Property
unit_encodeControlBits = unitTest $ do
    let z   = zeroControlBits
        ecb = encodeControlBits
    ecb z === (0 :: Word16)
    ecb z { controlBitsFIN = On } === (0b0000_0000_0000_0001 :: Word16)
    ecb z { controlBitsSYN = On } === (0b0000_0000_0000_0010 :: Word16)
    ecb z { controlBitsRST = On } === (0b0000_0000_0000_0100 :: Word16)
    ecb z { controlBitsPSH = On } === (0b0000_0000_0000_1000 :: Word16)
    ecb z { controlBitsACK = On } === (0b0000_0000_0001_0000 :: Word16)
    ecb z { controlBitsURG = On } === (0b0000_0000_0010_0000 :: Word16)
    ecb z { controlBitsECE = On } === (0b0000_0000_0100_0000 :: Word16)
    ecb z { controlBitsCRW = On } === (0b0000_0000_1000_0000 :: Word16)
    ecb z { controlBitsNS = On } === (0b0000_0001_0000_0000 :: Word16)

---- Properties

prop_tripControlBits :: Property
prop_tripControlBits = property $ do
    controlBits <- forAll genControlBits
    (decodeControlBits . encodeControlBits) controlBits === controlBits

---- Generators

genControlBits :: MonadGen m => m ControlBits
genControlBits =
    ControlBits
        <$> genControlBit
        <*> genControlBit
        <*> genControlBit
        <*> genControlBit
        <*> genControlBit
        <*> genControlBit
        <*> genControlBit
        <*> genControlBit
        <*> genControlBit

genControlBit :: MonadGen m => m ControlBit
genControlBit = boolToBit <$> Gen.bool
  where
    boolToBit :: Bool -> ControlBit
    boolToBit = \case
        True  -> On
        False -> Off
