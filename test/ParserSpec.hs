module ParserSpec where

import Test.Hspec
import Text.Megaparsec
import Parser

spec :: Spec
spec =
  describe "parses stuff" $ do
    it "parses one word" $ do
      runParser parseWord "" "<foo>" `shouldBe` Right "foo"
    it "parses one word with a dash" $ do
      runParser parseWord "" "<foo-bar>" `shouldBe` Right "foo-bar"
    it "parses multiple words" $ do
      runParser parseWords "" "<foo> <bar>" `shouldBe` Right ["foo","bar"]
      --adress
      --integer
    it "Parse Literal Integer" $ do
      runParser parseLitInt "" "15" `shouldBe` Right (LitInt 15)
    it "Parse Float" $ do
      runParser parseFloat "" "15.0" `shouldBe` Right 15.0
    it "Parse a hashtag" $ do
      runParser hashtag "" "#" `shouldBe` Right "#"
     -- Instructions
    -- it "Parse an Instruction" $ do
    --   runParser parseInstruction "" "add" `shouldBe` Right "add"
    -- it "Parse an undefined Instruction" $ do
    --   runParser parseInstruction "" "test" `shouldBe` Right "instruction test is not reserverd"
    -- it "Parse Nop Instruction" $ do
    --   runParser skipInstruction "" "Nop" `shouldBe` Right Nop
      --Fix label parser to amtch specification
    it "Parses an label" $ do
      runParser parseLabel "" "if2" `shouldBe` Right "if2"
    it "new: Parse a memory Adress (Int)" $ do
      runParser parseMemoryAdress "" "[15]" `shouldBe` Right (LitAdress (LitInt 15))
    it "new: Parse a memory Adress (String)" $ do
      runParser parseRegisterReference "" "[test]" `shouldBe` Right (LitAdress (LitString "test"))
    it "new: Parse a memory Adress (Int)" $ do
      runParser parseMemoryAdressOrReference "" "[15]" `shouldBe` Right (LitAdress (LitInt 15))
    it "new: Parse a memory Adress (String)" $ do
      runParser parseMemoryAdressOrReference "" "[test]" `shouldBe` Right (LitAdress (LitString "test"))