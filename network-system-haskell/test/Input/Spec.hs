{-# LANGUAGE OverloadedStrings #-}

module Input.Spec (specs) where

import qualified Internal.Input as I
import Test.Hspec (SpecWith, describe, it, shouldBe)

specs :: [SpecWith ()]
specs =
  [ spec_parseTerminationSequence,
    spec_parseInvalidInputs
  ]

spec_parseTerminationSequence :: SpecWith ()
spec_parseTerminationSequence = describe "Parse termination input values" $ do
  it "Parses the 'terminate' input" $ do
    I.parseMaybeInput "terminate\n" `shouldBe` Just I.Terminate
    I.parseMaybeInput "terminate\n\r" `shouldBe` Just I.Terminate
    I.parseMaybeInput "terminate\r\n" `shouldBe` Just I.Terminate
    I.parseMaybeInput "terminate\r" `shouldBe` Just I.Terminate

spec_parseInvalidInputs :: SpecWith ()
spec_parseInvalidInputs = describe "Invalid inputs should not be parsed" $ do
  it "Fails with 'terminate' and spaces intercalated" $ do
    I.parseMaybeInput "terminate" `shouldBe` Nothing
    I.parseMaybeInput " terminate" `shouldBe` Nothing
    I.parseMaybeInput "terminate \n" `shouldBe` Nothing
    I.parseMaybeInput " terminate " `shouldBe` Nothing
    I.parseMaybeInput " terminate\r" `shouldBe` Nothing
    I.parseMaybeInput "terminate \r" `shouldBe` Nothing
    I.parseMaybeInput "term inate " `shouldBe` Nothing
    I.parseMaybeInput " terminate " `shouldBe` Nothing
  it "Fails with random bytestrings" $ do
    I.parseMaybeInput "" `shouldBe` Nothing
    I.parseMaybeInput " " `shouldBe` Nothing
    I.parseMaybeInput " \n" `shouldBe` Nothing
    I.parseMaybeInput "\r" `shouldBe` Nothing
    I.parseMaybeInput " \r\n" `shouldBe` Nothing
    I.parseMaybeInput "12351" `shouldBe` Nothing
    I.parseMaybeInput "term\n\r" `shouldBe` Nothing
    I.parseMaybeInput "termination\n" `shouldBe` Nothing
    I.parseMaybeInput "hunter2\n" `shouldBe` Nothing
    I.parseMaybeInput "ñññ" `shouldBe` Nothing
    I.parseMaybeInput "hello123456789\n" `shouldBe` Nothing
    I.parseMaybeInput "helloterminate\r" `shouldBe` Nothing
