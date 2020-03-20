{-|
-}
module Numbat.TCP.Segment where

import           Data.Bits                      ( setBit )
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
encodeControlBits cb =
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
        & foldl' (flip setControlBit) (0 :: Word32)
  where
    setControlBit :: (Int, ControlBits -> ControlBit) -> Word32 -> Word32
    setControlBit (idx, bitFn) = case bitFn cb of
        On  -> flip setBit idx
        Off -> id
