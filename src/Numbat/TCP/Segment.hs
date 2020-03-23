{-|
-}
{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms    #-}
module Numbat.TCP.Segment where

import           Data.Binary.Get                ( Get )
import qualified Data.Binary.Get               as Get
import           Data.Binary.Put                ( Put )
import qualified Data.Binary.Put               as Put
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import qualified Data.Bits                     as Bits
import           Data.Foldable                  ( foldl' )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( catMaybes )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Word                      ( Word16
                                                , Word32
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
      , headerDataControlBits :: Word16
      , headerWindow          :: Word16
      , headerChecksum        :: Word16
      , headerUrgentPointer   :: Word16
      }

putHeader :: Header -> Put
putHeader hdr = do
    Put.putWord16be . headerSourcePort $ hdr
    Put.putWord16be . headerDestinationPort $ hdr
    Put.putWord32be . headerSequenceNumber $ hdr
    Put.putWord32be . headerAcknowledgement $ hdr
    Put.putWord16be . headerDataControlBits $ hdr
    Put.putWord16be . headerWindow $ hdr
    Put.putWord16be . headerChecksum $ hdr
    Put.putWord16be . headerUrgentPointer $ hdr

getHeader :: Get Header
getHeader = do
    sourcePort      <- Get.getWord16be
    destinationPort <- Get.getWord16be
    sequenceNumber  <- Get.getWord32be
    acknowledgement <- Get.getWord32be
    dataControlBits <- Get.getWord16be
    window          <- Get.getWord16be
    checksum        <- Get.getWord16be
    urgentPointer   <- Get.getWord16be
    pure Header { headerSourcePort      = sourcePort
                , headerDestinationPort = destinationPort
                , headerSequenceNumber  = sequenceNumber
                , headerAcknowledgement = acknowledgement
                , headerDataControlBits = dataControlBits
                , headerWindow          = window
                , headerChecksum        = checksum
                , headerUrgentPointer   = urgentPointer
                }

_FIN :: Lens' Word16 Bool
_FIN = Lens.lens (getW16Bit 0) (setW16Bit 0)

_SYN :: Lens' Word16 Bool
_SYN = Lens.lens (getW16Bit 1) (setW16Bit 1)

_RST :: Lens' Word16 Bool
_RST = Lens.lens (getW16Bit 2) (setW16Bit 2)

_PSH :: Lens' Word16 Bool
_PSH = Lens.lens (getW16Bit 3) (setW16Bit 3)

_ACK :: Lens' Word16 Bool
_ACK = Lens.lens (getW16Bit 4) (setW16Bit 4)

_URG :: Lens' Word16 Bool
_URG = Lens.lens (getW16Bit 5) (setW16Bit 5)

_ECE :: Lens' Word16 Bool
_ECE = Lens.lens (getW16Bit 6) (setW16Bit 6)

_CRW :: Lens' Word16 Bool
_CRW = Lens.lens (getW16Bit 7) (setW16Bit 7)

_NS :: Lens' Word16 Bool
_NS = Lens.lens (getW16Bit 8) (setW16Bit 8)

_dataOffset :: Lens' Word16 Nibble
_dataOffset = Lens.lens get set
  where
    get :: Word16 -> Nibble
    get = Nibble.wordLowBitsToNibble . fromIntegral . flip Bits.shiftR 12

    set :: Word16 -> Nibble -> Word16
    set w n = w' .|. Bits.shiftL l 12
      where
        l :: Word16
        l = fromIntegral $ Nibble.nibbleToWordLowBits n

        w' :: Word16
        w' = w .&. 0b0000_1111_1111_1111

getW16Bit :: Int -> Word16 -> Bool
getW16Bit i w = Bits.testBit w i

setW16Bit :: Int -> Word16 -> Bool -> Word16
setW16Bit i w b = if b then Bits.setBit w i else Bits.clearBit w i

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

controlBitsToWord16 :: ControlBits -> Word16
controlBitsToWord16 controlBits = foldl' (\x f -> f x) (0 :: Word16) setFns
  where
    setFns :: [Word16 -> Word16]
    setFns = Set.toList (unControlBits controlBits) <&> setFn

    setFn :: ControlBit -> (Word16 -> Word16)
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

word16ToControlBits :: Word16 -> ControlBits
word16ToControlBits word = ControlBits $ Set.fromList $ catMaybes bits
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

    getBit :: ControlBit -> Lens' Word16 Bool -> Maybe ControlBit
    getBit controlBit lens = if word ^. lens then Just controlBit else Nothing
