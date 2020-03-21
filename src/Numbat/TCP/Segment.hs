{-|
-}
module Numbat.TCP.Segment where

import           Data.Bits                      ( setBit
                                                , testBit
                                                )
import           Data.Function                  ( (&) )
import           Data.Foldable                  ( foldl' )
import           Data.Word                      ( Word8
                                                , Word16
                                                , Word32
                                                )

data ControlBit = On | Off

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

newtype DataOffset = DataOffset { unDataOffset :: Word8 } -- low 4-bits only

data Header
    = Header
      { headerSourcePort       :: Word16
      , headerDestinationPort  :: Word16
      , headerSequenceNumber   :: Word32
      , headerAcknowledgement  :: Word32
      , headerDataOffset       :: DataOffset
      , headerControlBits      :: ControlBits
      , headerWindow           :: Word16
      , headerChecksum         :: Word16
      , headerUrgentPointer    :: Word16
      }

encodeControlBits :: ControlBits -> Word32
encodeControlBits controlBits =
    [ controlBitsFIN
        , controlBitsSYN
        , controlBitsFIN
        , controlBitsRST
        , controlBitsPSH
        , controlBitsACK
        , controlBitsURG
        , controlBitsECE
        , controlBitsCRW
        , controlBitsNS
        ]
        & zip [0 ..]
        & foldl' (flip setControlBit) (0 :: Word32)
  where
    setControlBit :: (Int, ControlBits -> ControlBit) -> Word32 -> Word32
    setControlBit (index, bitFn) = case bitFn controlBits of
        On  -> flip setBit index
        Off -> id

decodeControlBits :: Word32 -> ControlBits
decodeControlBits word = ControlBits { controlBitsSYN = getControlBit 0
                                     , controlBitsFIN = getControlBit 1
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
