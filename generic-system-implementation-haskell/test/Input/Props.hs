{-# LANGUAGE OverloadedStrings #-}

module Input.Props (props, toBSInput) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Internal.Input as I
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

props :: [TestTree]
props =
  [ testProperty "Valid numeric values are parsed correctly" prop_parseValidNumber
  ]

prop_parseValidNumber :: Property
prop_parseValidNumber = property $ do
  num <- forAll genNumber
  if num >= 0 && num <= 999999999
    then I.parseMaybeInput (toBSInput num) === Just (I.Number num)
    else I.parseMaybeInput (toBSInput num) === Nothing

genNumber :: Gen Int
genNumber = Gen.int Range.linearBounded

toBSInput :: Int -> BS.ByteString
toBSInput = flip BS.append "\n" . justifyRight 9 48 . C.pack . show
  where
    justifyRight :: Int -> Word8 -> BS.ByteString -> BS.ByteString
    justifyRight k c t
      | BS.length t >= k = t
      | otherwise = BS.replicate (k - BS.length t) c `BS.append` t