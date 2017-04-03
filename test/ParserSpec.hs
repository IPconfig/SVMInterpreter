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
      it "returns a label" $ do
        runParser parseLabel "" "#hello_world" `shouldBe` Right "#hello_world"
      it "returns a register" $ do
        runParser parseRegister "" "reg1" `shouldBe` Right Reg1

        
      -- Instructions
      -- it "Parse an Instruction" $ do
      --   runParser parseInstruction "" "add" `shouldBe` Right "add"
      -- it "Parse an undefined Instruction" $ do
      --   runParser parseInstruction "" "test" `shouldBe` Right "instruction test is not reserverd"
      -- it "Parse Nop Instruction" $ do
      --   runParser skipInstruction "" "Nop" `shouldBe` Right Nop
        --Fix label parser to amtch specification


    describe "with literals" $ do
      it "returns a literal int" $ do
        runParser parseLitInt "" "15" `shouldBe` Right (LitInt 15)
      it "returns a literal float/double" $ do
        runParser parseLitFloat "" "15.5" `shouldBe` Right (LitFloat 15.5)
      it "returns a literal string" $ do
        runParser parseLitString "" "sampleWord" `shouldBe` Right (LitString "sampleWord")
      it "returns a memory adress" $ do
        runParser parseMemoryAdress "" "[15]" `shouldBe` Right (LitAdress (LitInt 15))
      it "retuns a Register Reference" $ do
        runParser parseRegisterReference "" "[reg1]" `shouldBe` Right (LitAdress (LitString "reg1"))
      it "returns a literal Register" $ do
          runParser parseLitRegister "" "reg1" `shouldBe` Right (LitRegister Reg1)
      it "new: Parse a memory Adress (Int)" $ do
        runParser parseMemoryAdressOrReference "" "[15]" `shouldBe` Right (LitAdress (LitInt 15))
      it "new: Parse a memory Adress (String)" $ do
        runParser parseMemoryAdressOrReference "" "[test]" `shouldBe` Right (LitAdress (LitString "test"))