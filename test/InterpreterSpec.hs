module InterpreterSpec where

import Test.Hspec
import Interpreter
import ADT
import Core

testSVMState :: SVMState
testSVMState = SVMState
    { memory = [INT 0, DOUBLE 1.0, STRING "3", INT 4]
    , register1 = INT 1
    , register2 = (DOUBLE 2.0)
    , register3 = STRING "reg3"
    , register4 = INT 0
    , programCounter = 0
    } 


-- SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}

spec :: Spec
spec = 
  describe "Interpret stuff" $ do
    it "evaluate Nop" $ do
      eval testSVMState Nop `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 1}
    describe "evaluate Mov" $ do
      it "with memory adress" $ do
        eval testSVMState (Mov (LitAdress (LitInt 2)) (LitString "test")) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "test",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with register reference" $ do
        eval testSVMState (Mov (LitAdress (LitRegister Reg4)) (LitString "test")) `shouldBe` SVMState {memory = [STRING "test", DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with register" $ do
        eval testSVMState (Mov (LitAdress (LitRegister Reg1)) (LitString "test") ) `shouldBe` SVMState {memory = [INT 0, STRING "test",STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    it "evaluate And" $ do
      eval testSVMState (And Reg4 (LitInt 5)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 1, programCounter = 0}
    it "evaluate Or" $ do
      eval testSVMState (Or Reg4 (LitInt 5)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 1, programCounter = 0}
    it "evaluate Not" $ do
      eval testSVMState (Not Reg1) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT (-1), register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    it "evaluate Mod" $ do
      eval testSVMState (Mod Reg1 (LitInt 5)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    describe "evaluate Add" $ do
        it "with integers" $ do
          eval testSVMState (Add Reg1 (LitInt 5)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 6, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
        it "with doubles" $ do
          eval testSVMState (Add Reg2 (LitFloat 5.0)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 7.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
        it "with strings" $ do
          eval testSVMState (Add Reg3 (LitString "added")) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3added", register4 = INT 0, programCounter = 0}
    describe "evaluate Sub" $ do
      it "with integers" $ do
        eval testSVMState (Sub Reg1 (LitInt 5)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT (-4), register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with doubles" $ do
        eval testSVMState (Sub Reg2 (LitFloat 1.0)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 1.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    describe "evaluate Mul" $ do
      it "with integers" $ do
        eval testSVMState (Mul Reg1 (LitInt 5)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 5, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with doubles" $ do
        eval testSVMState (Mul Reg2 (LitFloat 3.0)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 6.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    describe "evaluate Div" $ do
      it "with integers" $ do
        eval testSVMState (Div Reg1 (LitInt 5)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 0, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with doubles" $ do
        eval testSVMState (Div Reg2 (LitFloat 2.0)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 1.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    describe "evaluate Cmp" $ do
      it "with x < y" $ do
        eval testSVMState (Cmp Reg1 (LitInt 5)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT (-1), register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with x == y" $ do
        eval testSVMState (Cmp Reg2 (LitFloat 2.0)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = INT 0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with x > y" $ do
        eval testSVMState (Cmp Reg2 (LitFloat 1.0)) `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = INT 1, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}