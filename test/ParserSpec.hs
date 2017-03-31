module ParserSpec where

import Test.Hspec
import Text.Megaparsec
import Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "Parses stuff" $ do
    describe "with general building blocks" $ do
      it "returns an integer" $ do
        runParser parseInteger "" "15" `shouldBe` Right 15
      it "returns a double" $ do
        runParser parseDouble "" "15.12345" `shouldBe` Right 15.12345
      it "returns one word" $ do
        runParser parseWord "" "sampleWord" `shouldBe` Right "sampleWord"
      it "returns multiple words" $ do
        runParser parseWords "" "sampleWord sampleWord2" `shouldBe` Right ["sampleWord", "sampleWord2"]
      it "returns a hashtag" $ do
        runParser parseHashtag "" "#" `shouldBe` Right "#"
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

    describe "with literals" $ do
      it "returns a literal int" $ do
        runParser parseLitInt "" "15" `shouldBe` Right (LitInt 15)
      it "returns a literal float/double" $ do
        runParser parseLitFloat "" "15.5" `shouldBe` Right (LitFloat 15.5)
      it "returns a memory adress" $ do
        runParser parseMemoryAdress "" "[15]" `shouldBe` Right (LitAdress (LitInt 15))
      it "retuns a Register Reference" $ do
        runParser parseRegisterReference "" "[test]" `shouldBe` Right (LitAdress (LitString "test"))
      it "new: Parse a memory Adress (Int)" $ do
        runParser parseMemoryAdressOrReference "" "[15]" `shouldBe` Right (LitAdress (LitInt 15))
      it "new: Parse a memory Adress (String)" $ do
        runParser parseMemoryAdressOrReference "" "[test]" `shouldBe` Right (LitAdress (LitString "test"))