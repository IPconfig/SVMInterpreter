module ParserSpec where

import Test.Hspec
import Text.Megaparsec
import Parser

spec :: Spec
spec =
  describe "The building blocks of the parser" $ do
    it "Parse a integer" $ do
      runParser parseInteger "" "15" `shouldBe` Right 15
    it "Parse a double" $ do
      runParser parseDouble "" "15.12345" `shouldBe` Right 15.12345
    it "Parse one word" $ do
      runParser parseWord "" "sampleWord" `shouldBe` Right "sampleWord"
    it "Parse multiple words" $ do
      runParser parseWords "" "sampleWord sampleWord2" `shouldBe` Right ["sampleWord", "sampleWord2"]
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
    it "Parse a label" $ do
      runParser parseLabel "" "if2" `shouldBe` Right "if2"
--  describe "Literals" $ do
    it "new: Parse a memory Adress (Int)" $ do
      runParser parseMemoryAdress "" "[15]" `shouldBe` Right (LitAdress (LitInt 15))
    it "new: Parse a memory Adress (String)" $ do
      runParser parseRegisterReference "" "[test]" `shouldBe` Right (LitAdress (LitString "test"))
    it "new: Parse a memory Adress (Int)" $ do
      runParser parseMemoryAdressOrReference "" "[15]" `shouldBe` Right (LitAdress (LitInt 15))
    it "new: Parse a memory Adress (String)" $ do
      runParser parseMemoryAdressOrReference "" "[test]" `shouldBe` Right (LitAdress (LitString "test"))