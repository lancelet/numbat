{-|
-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Numbat.TCP.Segment where

import           Control.Applicative            ( many )
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import qualified Data.Bits                     as Bits
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import           Data.Foldable                  ( foldl' )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( catMaybes )
import           Data.Serialize.Get             ( Get )
import qualified Data.Serialize.Get            as Get
import           Data.Serialize.Put             ( Put )
import qualified Data.Serialize.Put            as Put
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Word                      ( Word16
                                                , Word32
                                                , Word8
                                                )
import           Optics                         ( (^.) )
import qualified Optics                        as Optics
import           Optics.Lens                    ( Lens' )
import qualified Optics.Lens                   as Lens

import           Numbat.Nibble                  ( Nibble )
import qualified Numbat.Nibble                 as Nibble

data Header
    = Header
      { headerSourcePort      :: Word16
      , headerDestinationPort :: Word16
      , headerSequenceNumber  :: Word32
      , headerAcknowledgement :: Word32
      , headerDataControlBits :: DataControlBits
      , headerWindow          :: Word16
      , headerChecksum        :: Word16
      , headerUrgentPointer   :: Word16
      , headerRawOptions      :: RawOptions
      }

putHeader :: Header -> Put
putHeader hdr = do
    Put.putWord16be . headerSourcePort $ hdr
    Put.putWord16be . headerDestinationPort $ hdr
    Put.putWord32be . headerSequenceNumber $ hdr
    Put.putWord32be . headerAcknowledgement $ hdr
    Put.putWord16be . unDataControlBits . headerDataControlBits $ hdr
    Put.putWord16be . headerWindow $ hdr
    Put.putWord16be . headerChecksum $ hdr
    Put.putWord16be . headerUrgentPointer $ hdr
    putRawOptions . headerRawOptions $ hdr

getHeader :: Get Header
getHeader = Get.label "TCP Header" $ do
    sourcePort      <- Get.getWord16be
    destinationPort <- Get.getWord16be
    sequenceNumber  <- Get.getWord32be
    acknowledgement <- Get.getWord32be
    dataControlBits <- DataControlBits <$> Get.getWord16be
    window          <- Get.getWord16be
    checksum        <- Get.getWord16be
    urgentPointer   <- Get.getWord16be
    let dataOffset :: Int =
            fromIntegral
                $  Nibble.nibbleToWordLowBits
                $  dataControlBits
                ^. _dataOffset
        optionsLen :: Int = (dataOffset - 5) * 4
    rawOptions <- if optionsLen > 0
        then Get.isolate optionsLen getRawOptions
        else pure emptyRawOptions
    pure Header { headerSourcePort      = sourcePort
                , headerDestinationPort = destinationPort
                , headerSequenceNumber  = sequenceNumber
                , headerAcknowledgement = acknowledgement
                , headerDataControlBits = dataControlBits
                , headerWindow          = window
                , headerChecksum        = checksum
                , headerUrgentPointer   = urgentPointer
                , headerRawOptions      = rawOptions
                }

newtype DataControlBits = DataControlBits { unDataControlBits :: Word16 }
  deriving (Eq, Show, Ord, Num)

_FIN :: Lens' DataControlBits Bool
_FIN = Lens.lens (getW16Bit 0) (setW16Bit 0)

_SYN :: Lens' DataControlBits Bool
_SYN = Lens.lens (getW16Bit 1) (setW16Bit 1)

_RST :: Lens' DataControlBits Bool
_RST = Lens.lens (getW16Bit 2) (setW16Bit 2)

_PSH :: Lens' DataControlBits Bool
_PSH = Lens.lens (getW16Bit 3) (setW16Bit 3)

_ACK :: Lens' DataControlBits Bool
_ACK = Lens.lens (getW16Bit 4) (setW16Bit 4)

_URG :: Lens' DataControlBits Bool
_URG = Lens.lens (getW16Bit 5) (setW16Bit 5)

_ECE :: Lens' DataControlBits Bool
_ECE = Lens.lens (getW16Bit 6) (setW16Bit 6)

_CRW :: Lens' DataControlBits Bool
_CRW = Lens.lens (getW16Bit 7) (setW16Bit 7)

_NS :: Lens' DataControlBits Bool
_NS = Lens.lens (getW16Bit 8) (setW16Bit 8)

_dataOffset :: Lens' DataControlBits Nibble
_dataOffset = Lens.lens get set
  where
    get :: DataControlBits -> Nibble
    get =
        Nibble.wordLowBitsToNibble
            . fromIntegral
            . flip Bits.shiftR 12
            . unDataControlBits

    set :: DataControlBits -> Nibble -> DataControlBits
    set b n = DataControlBits $ (w .&. 0b0000_1111_1111_1111) .|. Bits.shiftL
        l
        12
      where
        w :: Word16
        w = unDataControlBits b

        l :: Word16
        l = fromIntegral $ Nibble.nibbleToWordLowBits n

getW16Bit :: Int -> DataControlBits -> Bool
getW16Bit i w = Bits.testBit (unDataControlBits w) i

setW16Bit :: Int -> DataControlBits -> Bool -> DataControlBits
setW16Bit i w' b = DataControlBits
    $ if b then Bits.setBit w i else Bits.clearBit w i
  where
    w :: Word16
    w = unDataControlBits w'

newtype RawOptions = RawOptions { unRawOptions :: [RawOption] }

data RawOption
  = RawOption
    { rawOptionKind  :: Word8
    , rawOptionValue :: ByteString
    }

emptyRawOptions :: RawOptions
emptyRawOptions = RawOptions []

getRawOptions :: Get RawOptions
getRawOptions = RawOptions <$> many getRawOption

getRawOption :: Get RawOption
getRawOption = Get.label "Raw TCP option" $ do
    kindByte :: Word8 <- Get.getWord8
    case kindByte of
        0x00 -> pure $ RawOption 0x00 ByteString.empty
        0x01 -> pure $ RawOption 0x01 ByteString.empty
        kind -> do
            len <- Get.getWord8
            let nBytes :: Int = fromIntegral (len - 2)
            Get.isolate nBytes $ RawOption kind <$> Get.getByteString nBytes

putRawOptions :: RawOptions -> Put
putRawOptions options = sequence_ $ putRawOption <$> unRawOptions options

putRawOption :: RawOption -> Put
putRawOption rawOption = case rawOptionKind rawOption of
    0x00 -> Put.putWord8 0x00
    0x01 -> Put.putWord8 0x01
    kind -> do
        let value :: ByteString = rawOptionValue rawOption
            len :: Word8        = fromIntegral (ByteString.length value)
        Put.putWord8 kind
        Put.putWord8 len
        Put.putByteString value

data ControlBit
  = FIN
  | SYN
  | RST
  | PSH
  | ACK
  | URG
  | ECE
  | CRW
  | NS
  deriving (Eq, Ord, Enum, Bounded, Show)

newtype ControlBits = ControlBits { unControlBits :: Set ControlBit }
  deriving (Eq, Show)

controlBitsToDataControlBits :: ControlBits -> DataControlBits
controlBitsToDataControlBits controlBits = foldl' (\x f -> f x)
                                                  (0 :: DataControlBits)
                                                  setFns
  where
    setFns :: [DataControlBits -> DataControlBits]
    setFns = Set.toList (unControlBits controlBits) <&> setFn

    setFn :: ControlBit -> (DataControlBits -> DataControlBits)
    setFn = \case
        FIN -> Optics.set' _FIN True
        SYN -> Optics.set' _SYN True
        RST -> Optics.set' _RST True
        PSH -> Optics.set' _PSH True
        ACK -> Optics.set' _ACK True
        URG -> Optics.set' _URG True
        ECE -> Optics.set' _ECE True
        CRW -> Optics.set' _CRW True
        NS  -> Optics.set' _NS True

dataControlBitsToControlBits :: DataControlBits -> ControlBits
dataControlBitsToControlBits dcb = ControlBits $ Set.fromList $ catMaybes bits
  where
    bits :: [Maybe ControlBit]
    bits =
        [ getBit FIN _FIN
        , getBit SYN _SYN
        , getBit RST _RST
        , getBit PSH _PSH
        , getBit ACK _ACK
        , getBit URG _URG
        , getBit ECE _ECE
        , getBit CRW _CRW
        , getBit NS  _NS
        ]

    getBit :: ControlBit -> Lens' DataControlBits Bool -> Maybe ControlBit
    getBit controlBit lens = if dcb ^. lens then Just controlBit else Nothing
