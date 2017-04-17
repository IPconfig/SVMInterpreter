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
      eval Nop testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 1}
    describe "evaluate Mov" $ do
      it "with memory adress" $ do
        eval (Mov (LitAdress (LitInt 2)) (LitString "test")) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "test",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with register reference" $ do
        eval (Mov (LitAdress (LitRegister Reg4)) (LitString "test")) testSVMState `shouldBe` SVMState {memory = [STRING "test", DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with register" $ do
        eval (Mov (LitAdress (LitRegister Reg1)) (LitString "test") ) testSVMState `shouldBe` SVMState {memory = [INT 0, STRING "test",STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    it "evaluate And" $ do
      eval (And Reg4 (LitInt 5)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 1, programCounter = 0}
    it "evaluate Or" $ do
      eval (Or Reg4 (LitInt 5)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 1, programCounter = 0}
    it "evaluate Not" $ do
      eval (Not Reg1) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT (-1), register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    it "evaluate Mod" $ do
      eval (Mod Reg1 (LitInt 5)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    describe "evaluate Add" $ do
        it "with integers" $ do
          eval (Add Reg1 (LitInt 5)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 6, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
        it "with doubles" $ do
          eval (Add Reg2 (LitFloat 5.0)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 7.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
        it "with strings" $ do
          eval (Add Reg3 (LitString "added")) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3added", register4 = INT 0, programCounter = 0}
    describe "evaluate Sub" $ do
      it "with integers" $ do
        eval (Sub Reg1 (LitInt 5)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT (-4), register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with doubles" $ do
        eval (Sub Reg2 (LitFloat 1.0)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 1.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    describe "evaluate Mul" $ do
      it "with integers" $ do
        eval (Mul Reg1 (LitInt 5)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 5, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with doubles" $ do
        eval (Mul Reg2 (LitFloat 3.0)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 6.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    describe "evaluate Div" $ do
      it "with integers" $ do
        eval (Div Reg1 (LitInt 5)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 0, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with doubles" $ do
        eval (Div Reg2 (LitFloat 2.0)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 1.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
    describe "evaluate Cmp" $ do
      it "with x < y" $ do
        eval (Cmp Reg1 (LitInt 5)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT (-1), register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with x == y" $ do
        eval (Cmp Reg2 (LitFloat 2.0)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = INT 0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "with x > y" $ do
        eval (Cmp Reg2 (LitFloat 1.0)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = INT 1, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}