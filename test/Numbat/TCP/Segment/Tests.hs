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
import qualified Hedgehog.Range                as Range
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
import qualified Data.Set                      as Set
import           Data.Word                      ( Word16 )
import           Optics                         ( (^.) )

import qualified Numbat.Nibble                 as Nibble
import           Numbat.TCP.Segment             ( ControlBit
                                                , ControlBits
                                                , Header
                                                )
import qualified Numbat.TCP.Segment            as Segment

tests :: TestTree
tests = testGroup
    "Numbat.TCP.Segment.Tests"
    [ testProperty "unit: controlBitsToWord16"   unit_controlBitsToWord16
    , testProperty "unit: getHeader - example 1" unit_getHeaderExample1
    , testProperty "prop: controlBits/Word16 round-trip"
                   prop_controlBitsRoundTrip
    ]

---- Unit Tests

unit_controlBitsToWord16 :: Property
unit_controlBitsToWord16 = unitTest $ do
    let oneControlBit :: ControlBit -> Word16
        oneControlBit controlBit = Segment.controlBitsToWord16
            (Segment.ControlBits $ Set.fromList $ [controlBit])
    oneControlBit Segment.FIN === 0b0000_0000_0000_0001
    oneControlBit Segment.SYN === 0b0000_0000_0000_0010
    oneControlBit Segment.RST === 0b0000_0000_0000_0100
    oneControlBit Segment.PSH === 0b0000_0000_0000_1000
    oneControlBit Segment.ACK === 0b0000_0000_0001_0000
    oneControlBit Segment.URG === 0b0000_0000_0010_0000
    oneControlBit Segment.ECE === 0b0000_0000_0100_0000
    oneControlBit Segment.CRW === 0b0000_0000_1000_0000
    oneControlBit Segment.NS === 0b0000_0001_0000_0000

unit_getHeaderExample1 :: Property
unit_getHeaderExample1 = unitTest $ do
    -- an example from a WireShark dump:
    let hdrBytes :: ByteString =
            [hx| 01 bb e4 0a 2c 32 06 98 ca ed 8a 6d 80 10 00 1f |]
                <> [hx| 91 44 00 00 01 01 08 0a 62 c4 2f d4 2c 08 39 19 |]
        hdr :: Header = Get.runGet Segment.getHeader (LBS.fromStrict hdrBytes)
    Segment.headerSourcePort hdr === 443
    Segment.headerDestinationPort hdr === 58378
    Segment.headerSequenceNumber hdr === 741475992
    Segment.headerAcknowledgement hdr === 3404565101
    Segment.headerDataControlBits hdr
        ^.  Segment._dataOffset
        === Nibble.wordLowBitsToNibble 8
    -- Segment.headerControlBits hdr === zeroControlBits { controlBitsACK = On }
    Segment.headerWindow hdr === 31
    Segment.headerChecksum hdr === 0x9144
    Segment.headerUrgentPointer hdr === 0

---- Properties

prop_controlBitsRoundTrip :: Property
prop_controlBitsRoundTrip = property $ do
    controlBits <- forAll genControlBits
    let w16 = Segment.controlBitsToWord16 controlBits
    Segment.word16ToControlBits w16 === controlBits

---- Generators

genControlBits :: (MonadGen m) => m ControlBits
genControlBits =
    Segment.ControlBits <$> Gen.set (Range.linear 0 9) genControlBit

genControlBit :: (MonadGen m) => m ControlBit
genControlBit = Gen.enum minBound maxBound
