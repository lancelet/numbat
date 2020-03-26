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
import           TestUtil                       ( fromRight
                                                , hx
                                                , unitTest
                                                )

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import qualified Data.Serialize.Get            as Get
import qualified Data.Set                      as Set
import           Optics                         ( set' )

import qualified Numbat.Nibble                 as Nibble
import           Numbat.TCP.Segment             ( ControlBit
                                                , ControlBits
                                                , DataControlBits
                                                , Header
                                                )
import qualified Numbat.TCP.Segment            as Segment

tests :: TestTree
tests = testGroup
    "Numbat.TCP.Segment.Tests"
    [ testProperty "unit: controlBitsToWord16 - single bits"
                   unit_controlBitsToWord16
    , testProperty "unit: getHeader - example 1" unit_getHeaderExample1
    , testProperty "prop: controlBits/Word16 round-trip"
                   prop_controlBitsRoundTrip
    ]

---- Unit Tests

unit_controlBitsToWord16 :: Property
unit_controlBitsToWord16 = unitTest $ do
    let oneControlBit :: ControlBit -> DataControlBits
        oneControlBit controlBit = Segment.controlBitsToDataControlBits
            (Segment.ControlBits $ Set.fromList [controlBit])
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
    header :: Header <- fromRight $ Get.runGet Segment.getHeader hdrBytes
    let
        expected :: Header = Segment.Header
            { Segment.headerSourcePort      = 443
            , Segment.headerDestinationPort = 58378
            , Segment.headerSequenceNumber  = 741475992
            , Segment.headerAcknowledgement = 3404565101
            , Segment.headerDataControlBits =
                set' Segment._dataOffset (Nibble.wordLowBitsToNibble 8)
                . set' Segment._ACK True
                $ Segment.DataControlBits 0
            , Segment.headerWindow          = 31
            , Segment.headerChecksum        = 0x9144
            , Segment.headerUrgentPointer   = 0
            , Segment.headerRawOptions      = Segment.RawOptions
                [ Segment.RawOption { Segment.rawOptionKind  = 0x01
                                    , Segment.rawOptionValue = ByteString.empty
                                    }
                , Segment.RawOption { Segment.rawOptionKind  = 0x01
                                    , Segment.rawOptionValue = ByteString.empty
                                    }
                , Segment.RawOption
                    { Segment.rawOptionKind  = 0x08
                    , Segment.rawOptionValue = [hx| 62 c4 2f d4 2c 08 39 19 |]
                    }
                ]
            }
    header === expected

---- Properties

prop_controlBitsRoundTrip :: Property
prop_controlBitsRoundTrip = property $ do
    controlBits <- forAll genControlBits
    let dcb :: DataControlBits =
            Segment.controlBitsToDataControlBits controlBits
    Segment.dataControlBitsToControlBits dcb === controlBits

---- Generators

genControlBits :: (MonadGen m) => m ControlBits
genControlBits =
    Segment.ControlBits <$> Gen.set (Range.linear 0 9) genControlBit

genControlBit :: (MonadGen m) => m ControlBit
genControlBit = Gen.enum minBound maxBound
