{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module TestUtil
    ( unitTest
    , parseHexString
    , hx
    )
where

import           Language.Haskell.TH.Quote      ( QuasiQuoter(QuasiQuoter)
                                                , quoteDec
                                                , quoteExp
                                                , quotePat
                                                , quoteType
                                                )
import           Language.Haskell.TH.Syntax     ( Exp(AppE, VarE)
                                                , Q
                                                , lift
                                                )

import           Hedgehog                       ( Property
                                                , PropertyT
                                                )
import qualified Hedgehog
import           Hedgehog.Internal.Source       ( HasCallStack )

import           Control.Monad                  ( (>=>) )
import           Data.Bits                      ( (.|.) )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( catMaybes )
import           Data.Word                      ( Word8 )

import           Numbat.Nibble                  ( Nibble
                                                , nibbleToWordHighBits
                                                , nibbleToWordLowBits
                                                , wordLowBitsToNibble
                                                )

unitTest :: HasCallStack => PropertyT IO () -> Property
unitTest = Hedgehog.withTests 1 . Hedgehog.property

hx :: QuasiQuoter
hx = QuasiQuoter { quoteExp  = parseHexStringM >=> liftByteString
                 , quotePat  = undefined
                 , quoteDec  = undefined
                 , quoteType = undefined
                 }

liftByteString :: ByteString -> Q Exp
liftByteString bs =
    AppE (VarE 'ByteString.pack) <$> lift (ByteString.unpack bs)

parseHexStringM :: MonadFail m => String -> m ByteString
parseHexStringM hexStr = case parseHexString hexStr of
    Nothing -> fail $ "Could not parse \"" <> hexStr <> "\" as a hex string."
    Just bs -> pure bs

parseHexString :: String -> Maybe ByteString
parseHexString hexStr = nibbles hexStr >>= nibblesToWords <&> ByteString.pack
  where
    nibblesToWords :: [Nibble] -> Maybe [Word8]
    nibblesToWords ns = pairs ns <&> fmap nibblePairToWord

    nibblePairToWord :: (Nibble, Nibble) -> Word8
    nibblePairToWord (h, l) = nibbleToWordHighBits h .|. nibbleToWordLowBits l

    nibbles :: String -> Maybe [Nibble]
    nibbles s = catMaybes <$> sequence (parseChar <$> s)

    parseChar :: Char -> Maybe (Maybe Nibble)
    parseChar c = if c == ' ' then Just Nothing else Just <$> parseHexChar c

parseHexChar :: Char -> Maybe Nibble
parseHexChar c = wordLowBitsToNibble <$> case c of
    '0' -> Just 0x0
    '1' -> Just 0x1
    '2' -> Just 0x2
    '3' -> Just 0x3
    '4' -> Just 0x4
    '5' -> Just 0x5
    '6' -> Just 0x6
    '7' -> Just 0x7
    '8' -> Just 0x8
    '9' -> Just 0x9
    'a' -> Just 0xA
    'A' -> Just 0xA
    'b' -> Just 0xB
    'B' -> Just 0xB
    'c' -> Just 0xC
    'C' -> Just 0xC
    'd' -> Just 0xD
    'D' -> Just 0xD
    'e' -> Just 0xE
    'E' -> Just 0xE
    'f' -> Just 0xF
    'F' -> Just 0xF
    _   -> Nothing

pairs :: [a] -> Maybe [(a, a)]
pairs []           = Just []
pairs (a : b : xs) = fmap ((:) (a, b)) (pairs xs)
pairs [_         ] = Nothing
