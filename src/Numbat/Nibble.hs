{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE NumericUnderscores #-}
module Numbat.Nibble
    ( Nibble
    , mkNibble
    , wordLowBitsToNibble
    , nibbleToWordLowBits
    , nibbleToWordHighBits
    )
where

import           Data.Bits                      ( shift
                                                , testBit
                                                , (.&.)
                                                )
import           Data.Word                      ( Word8 )

newtype Nibble = Nibble Word8

mkNibble :: Word8 -> Maybe Nibble
mkNibble word = if word .&. highBits == 0 then Just (Nibble word) else Nothing

wordLowBitsToNibble :: Word8 -> Nibble
wordLowBitsToNibble word = Nibble (word .&. lowBits)

nibbleToWordLowBits :: Nibble -> Word8
nibbleToWordLowBits (Nibble n) = n

nibbleToWordHighBits :: Nibble -> Word8
nibbleToWordHighBits (Nibble n) = shift n 4

highBits :: Word8
highBits = 0b1111_0000

lowBits :: Word8
lowBits = 0b0000_1111

instance Eq Nibble where
    Nibble a == Nibble b = a .&. lowBits == b .&. lowBits

instance Ord Nibble where
    compare (Nibble a) (Nibble b) = compare (a .&. lowBits) (b .&. lowBits)

instance Show Nibble where
    show (Nibble a) = "0b" ++ [b1, b2, b3, b4]
      where
        b1, b2, b3, b4 :: Char
        b1 = charBit 3
        b2 = charBit 2
        b3 = charBit 1
        b4 = charBit 0

        charBit :: Int -> Char
        charBit i = if testBit a i then '1' else '0'
