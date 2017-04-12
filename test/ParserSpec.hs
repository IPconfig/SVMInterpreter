module ParserSpec where

import Test.Hspec
import Text.Megaparsec
import ADT
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
      it "returns an identifier" $ do
        runParser parseId "" "h98_0e" `shouldBe` Right "h98_0e"
      it "returns a label" $ do
        runParser parseLabel "" "#h" `shouldBe` Right "#h"
      it "returns a register" $ do
        runParser parseRegister "" "reg1" `shouldBe` Right Reg1
    describe "with literals" $ do
      it "returns a literal Integer" $ do
        runParser parseLitInt "" "15" `shouldBe` Right (LitInt 15)
      it "returns a literal float/double" $ do
        runParser parseLitFloat "" "15.5" `shouldBe` Right (LitFloat 15.5)
      it "returns a literal string" $ do
        runParser parseLitString "" "sampleWord" `shouldBe` Right (LitString "sampleWord")
      it "returns a memory adress" $ do
        runParser parseMemoryAdressOrReference "" "[15]" `shouldBe` Right (LitAdress (LitInt 15))
      it "retuns a Register Reference" $ do
        runParser parseMemoryAdressOrReference "" "[reg1]" `shouldBe` Right (LitAdress (LitRegister Reg1))
      it "return a literal Register" $ do
          runParser parseLitRegister "" "reg1" `shouldBe` Right (LitRegister Reg1)
      it "returns a negative literal Integer" $ do
        negate' (LitInt 5) `shouldBe` (LitInt (-5))
      it "returns a negative literal Double" $ do
        negate' (LitFloat 5.5) `shouldBe` (LitFloat (-5.5))
    describe "with literalParser" $ do
      it "returns a negative literal Integer" $ do
        runParser parseLiteral "" "-15" `shouldBe` Right (LitInt (-15))
      it "returns a negative literal float/double" $ do
        runParser parseLiteral "" "-15.5" `shouldBe` Right (LitFloat (-15.5))
      it "returns a literal string" $ do
        runParser parseLiteral "" "sampleWord" `shouldBe` Right (LitString "sampleWord")
      it "returns a memory adress" $ do
        runParser parseLiteral "" "[15]" `shouldBe` Right (LitAdress (LitInt 15))
      it "retuns a Register Reference" $ do
        runParser parseLiteral "" "[reg1]" `shouldBe` Right (LitAdress (LitRegister Reg1))
      it "return a literal Register" $ do
          runParser parseLiteral "" "reg1" `shouldBe` Right (LitRegister Reg1)
    describe "with instructions" $ do
      it "returns instruction (Nop)" $ do
        runParser instruction' "" "nop" `shouldBe` Right (Nop)
      it "returns instruction (Mov)" $ do
        runParser instruction' "" "mov 10 10" `shouldBe` Right (Mov (LitInt 10) (LitInt 10))
      it "returns instruction (And)" $ do
        runParser instruction' "" "and reg1 10" `shouldBe` Right (And (Reg1) (LitInt 10))
      it "returns instruction (Or)" $ do
        runParser instruction' "" "or reg1 10" `shouldBe` Right (Or (Reg1) (LitInt 10))
      it "returns instruction (Not)" $ do
        runParser instruction' "" "not reg1" `shouldBe` Right (Not (Reg1))
      it "returns instruction (Mod)" $ do
        runParser instruction' "" "mod reg1 10" `shouldBe` Right (Mod (Reg1) (LitInt 10))
      it "returns instruction (Add)" $ do
        runParser instruction' "" "add reg1 10" `shouldBe` Right (Add (Reg1) (LitInt 10))
      it "returns instruction (Sub)" $ do
        runParser instruction' "" "sub reg1 10" `shouldBe` Right (Sub (Reg1) (LitInt 10))
      it "returns instruction (Mul)" $ do
        runParser instruction' "" "mul reg1 10" `shouldBe` Right (Mul (Reg1) (LitInt 10))
      it "returns instruction (Div)" $ do
        runParser instruction' "" "div reg1 10" `shouldBe` Right (Div (Reg1) (LitInt 10))
      it "returns instruction (Cmp)" $ do
        runParser instruction' "" "cmp reg1 10" `shouldBe` Right (Cmp (Reg1) (LitInt 10))
      it "returns instruction (Jmp)" $ do
        runParser instruction' "" "jmp hello_world" `shouldBe` Right (Jmp "hello_world")
      it "returns instruction (Jc)" $ do
        runParser instruction' "" "jc hello_world reg4" `shouldBe` Right (Jc "hello_world" Reg4)
      it "returns instruction (Jeq)" $ do
        runParser instruction' "" "jeq hello_world reg3" `shouldBe` Right (Jeq "hello_world" Reg3)
      it "returns instruction (Label)" $ do
        runParser instruction' "" "#hello_world" `shouldBe` Right (LabelI "#hello_world")
      it "returns a Program[Instruction]" $ do
        runParser instructionProgram "" "mov 10 5 \n and reg1 4" `shouldBe` Right (Program [Mov (LitInt 10) (LitInt 5),And Reg1 (LitInt 4)])