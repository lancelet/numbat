{-|
-}
{-# LANGUAGE BinaryLiterals #-}
module Numbat.TCP.Segment where

import           Data.Binary.Get                ( Get )
import qualified Data.Binary.Get               as Get
import           Data.Binary.Put                ( Put )
import qualified Data.Binary.Put               as Put
import           Data.Bits                      ( setBit
                                                , shift
                                                , testBit
                                                , (.&.)
                                                , (.|.)
                                                )
import           Data.Word                      ( Word16
                                                , Word32
                                                , Word8
                                                )

data Header
    = Header
      { headerSourcePort      :: Word16
      , headerDestinationPort :: Word16
      , headerSequenceNumber  :: Word32
      , headerAcknowledgement :: Word32
      , headerDataOffset      :: DataOffset
      , headerControlBits     :: ControlBits
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
    Put.putWord16be $ mkWord7 (headerDataOffset hdr) (headerControlBits hdr)
    Put.putWord16be . headerWindow $ hdr
    Put.putWord16be . headerChecksum $ hdr
    Put.putWord16be . headerUrgentPointer $ hdr

mkWord7 :: DataOffset -> ControlBits -> Word16
mkWord7 dataOffset controlBits = encodeControlBits controlBits
    .|. shift (w8w16 (dataOffsetToWord8 dataOffset)) 12

getHeader :: Get Header
getHeader = do
    sourcePort                <- Get.getWord16be
    destinationPort           <- Get.getWord16be
    sequenceNumber            <- Get.getWord32be
    acknowledgement           <- Get.getWord32be
    (dataOffset, controlBits) <- decompWord7 <$> Get.getWord16be
    window                    <- Get.getWord16be
    checksum                  <- Get.getWord16be
    urgentPointer             <- Get.getWord16be
    pure Header { headerSourcePort      = sourcePort
                , headerDestinationPort = destinationPort
                , headerSequenceNumber  = sequenceNumber
                , headerAcknowledgement = acknowledgement
                , headerDataOffset      = dataOffset
                , headerControlBits     = controlBits
                , headerWindow          = window
                , headerChecksum        = checksum
                , headerUrgentPointer   = urgentPointer
                }

decompWord7 :: Word16 -> (DataOffset, ControlBits)
decompWord7 word7 = (dataOffset, decodeControlBits word7)
  where
    dataOffset :: DataOffset
    dataOffset = DataOffset $ w16w8 (shift word7 (-12) .&. 0b1111)

data ControlBit = On | Off deriving (Show, Eq)

data ControlBits
    = ControlBits
      { controlBitsNS  :: ControlBit
      , controlBitsCRW :: ControlBit
      , controlBitsECE :: ControlBit
      , controlBitsURG :: ControlBit
      , controlBitsACK :: ControlBit
      , controlBitsPSH :: ControlBit
      , controlBitsRST :: ControlBit
      , controlBitsSYN :: ControlBit
      , controlBitsFIN :: ControlBit
      }
      deriving (Show, Eq)

newtype DataOffset = DataOffset { unDataOffset :: Word8 } -- low 4-bits only

w8w16 :: Word8 -> Word16
w8w16 = fromIntegral

w16w8 :: Word16 -> Word8
w16w8 = fromIntegral

dataOffsetToWord8 :: DataOffset -> Word8
dataOffsetToWord8 = unDataOffset

zeroControlBits :: ControlBits
zeroControlBits = ControlBits { controlBitsNS  = Off
                              , controlBitsCRW = Off
                              , controlBitsECE = Off
                              , controlBitsURG = Off
                              , controlBitsACK = Off
                              , controlBitsPSH = Off
                              , controlBitsRST = Off
                              , controlBitsSYN = Off
                              , controlBitsFIN = Off
                              }

encodeControlBits :: ControlBits -> Word16
encodeControlBits controlBits =
    ( setControlBit 0 controlBitsFIN
        . setControlBit 1 controlBitsSYN
        . setControlBit 2 controlBitsRST
        . setControlBit 3 controlBitsPSH
        . setControlBit 4 controlBitsACK
        . setControlBit 5 controlBitsURG
        . setControlBit 6 controlBitsECE
        . setControlBit 7 controlBitsCRW
        . setControlBit 8 controlBitsNS
        )
        (0 :: Word16)
  where
    setControlBit :: Int -> (ControlBits -> ControlBit) -> Word16 -> Word16
    setControlBit index bitFn = case bitFn controlBits of
        On  -> flip setBit index
        Off -> id

decodeControlBits :: Word16 -> ControlBits
decodeControlBits word = ControlBits { controlBitsFIN = getControlBit 0
                                     , controlBitsSYN = getControlBit 1
                                     , controlBitsRST = getControlBit 2
                                     , controlBitsPSH = getControlBit 3
                                     , controlBitsACK = getControlBit 4
                                     , controlBitsURG = getControlBit 5
                                     , controlBitsECE = getControlBit 6
                                     , controlBitsCRW = getControlBit 7
                                     , controlBitsNS  = getControlBit 8
                                     }
  where
    getControlBit :: Int -> ControlBit
    getControlBit index = if testBit word index then On else Off
