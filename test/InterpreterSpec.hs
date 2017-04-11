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
      