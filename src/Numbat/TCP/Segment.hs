{-|
-}
module Numbat.TCP.Segment where

import           Data.Bits                      ( setBit
                                                , testBit
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.Function                  ( (&) )
import           Data.Word                      ( Word16
                                                , Word32
                                                , Word8
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
    [ controlBitsFIN
        , controlBitsSYN
        , controlBitsRST
        , controlBitsPSH
        , controlBitsACK
        , controlBitsURG
        , controlBitsECE
        , controlBitsCRW
        , controlBitsNS
        ]
        & zip [0 ..]
        & foldl' (flip setControlBit) (0 :: Word16)
  where
    setControlBit :: (Int, ControlBits -> ControlBit) -> Word16 -> Word16
    setControlBit (index, bitFn) = case bitFn controlBits of
        On  -> flip setBit index
        Off -> id

decodeControlBits :: Word16 -> ControlBits
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
