{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           TestUtil                       ( hx
                                                , unitTest
                                                )

import qualified Data.Binary.Get               as Get
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Word                      ( Word16 )

import           Numbat.Nibble                  ( wordLowBitsToNibble )
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
                                                , Header
                                                    ( headerAcknowledgement
                                                    , headerChecksum
                                                    , headerControlBits
                                                    , headerDataOffset
                                                    , headerDestinationPort
                                                    , headerSequenceNumber
                                                    , headerSourcePort
                                                    , headerUrgentPointer
                                                    , headerWindow
                                                    )
                                                , decodeControlBits
                                                , encodeControlBits
                                                , getHeader
                                                , zeroControlBits
                                                )

tests :: TestTree
tests = testGroup
    "Numbat.TCP.Segment.Tests"
    [ testProperty "unit: encodeControlBits"     unit_encodeControlBits
    , testProperty "unit: getHeader - example 1" unit_getHeaderExample1
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

unit_getHeaderExample1 :: Property
unit_getHeaderExample1 = unitTest $ do
    -- an example from a WireShark dump:
    let hdrBytes :: ByteString =
            [hx| 01 bb e4 0a 2c 32 06 98 ca ed 8a 6d 80 10 00 1f |]
                <> [hx| 91 44 00 00 01 01 08 0a 62 c4 2f d4 2c 08 39 19 |]
        hdr :: Header = Get.runGet getHeader (LBS.fromStrict hdrBytes)
    headerSourcePort hdr === 443
    headerDestinationPort hdr === 58378
    headerSequenceNumber hdr === 741475992
    headerAcknowledgement hdr === 3404565101
    headerDataOffset hdr === wordLowBitsToNibble 8
    headerControlBits hdr === zeroControlBits { controlBitsACK = On }
    headerWindow hdr === 31
    headerChecksum hdr === 0x9144
    headerUrgentPointer hdr === 0

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
