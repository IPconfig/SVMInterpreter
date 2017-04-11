module CoreSpec where

import Test.Hspec
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

spec :: Spec
spec = 
  describe "SVM Components" $ do
    describe "with general building blocks" $ do
      it "create memory made of values" $ do
        createMemory 10 `shouldBe` [INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0]
      it "return value from memory adress" $ do
        readMemory 2 [INT 0, DOUBLE 0.0, STRING "String"] `shouldBe` (STRING "String")
      it "create an empty SVM State" $ do
        emptySVMState `shouldBe` SVMState {memory = [INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0], register1 = INT 0, register2 = INT 0, register3 = INT 0, register4 = INT 0, programCounter = 0}
      it "update SVM program counter" $ do
        updateProgramCounter emptySVMState `shouldBe` SVMState {memory = [INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0,INT 0], register1 = INT 0, register2 = INT 0, register3 = INT 0, register4 = INT 0, programCounter = 1}

    -- | SVM Memory
    describe "with SVM Memory" $ do
      it "set value to SVM memory" $ do
        setMemory 2 (DOUBLE 8.5) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,DOUBLE 8.5,INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "return value from SVM memory" $ do
        getMemory 0 testSVMState `shouldBe` (INT 0)
      it "set Literal Int to SVM memory" $ do
        setMemWithAnyArg 0 (LitInt 9) testSVMState `shouldBe` SVMState {memory = [INT 9, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "set Literal Double to SVM memory" $ do
        setMemWithAnyArg 0 (LitFloat 9.9) testSVMState `shouldBe` SVMState {memory = [DOUBLE 9.9, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "set Literal String to SVM memory" $ do
        setMemWithAnyArg 0 (LitString "hello") testSVMState `shouldBe` SVMState {memory = [STRING "hello", DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "set Literal Adress (Int) to SVM memory" $ do
        setMemWithAnyArg 0 (LitAdress (LitInt 2)) testSVMState `shouldBe` SVMState {memory = [STRING "3", DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "set Literal Adress (Register) to SVM memory" $ do
        setMemWithAnyArg 0 (LitAdress (LitRegister Reg1)) testSVMState `shouldBe` SVMState {memory = [DOUBLE 1.0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "set Literal register to SVM memory" $ do
        setMemWithAnyArg 0 (LitRegister Reg3) testSVMState `shouldBe` SVMState {memory = [STRING "reg3", DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}

    -- | SVM Registers  
    describe "with SVM Registers" $ do
      it "set a SVM Register to a value" $ do
        setRegister Reg1 (STRING("test")) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = STRING "test", register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "return value from a SVM Register" $ do
        getRegister Reg3 testSVMState `shouldBe` (STRING "reg3")
      it "return a memory adress from SVM Register" $ do
        getAdressFromRegister Reg1 testSVMState `shouldBe` 1 
      it "Set a memory adress from SVM Register" $ do
        trySetMemFromReg Reg4 (LitString "test") testSVMState `shouldBe` SVMState {memory = [STRING "test", DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "set Literal Int to SVM Register" $ do
        setRegWithAnyArg Reg1 (LitInt 9) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 9, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "set Literal Double to SVM Register" $ do
        setRegWithAnyArg Reg4 (LitFloat 9.9) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = DOUBLE 9.9, programCounter = 0}
      it "set Literal String to SVM Register" $ do
        setRegWithAnyArg Reg1 (LitString "hello") testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = STRING "hello", register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "set Literal Adress (Int) to SVM Register" $ do
        setRegWithAnyArg Reg1 (LitAdress (LitInt 2)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = STRING "3", register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = INT 0, programCounter = 0}
      it "set Literal Adress (Register) to SVM Register" $ do
        setRegWithAnyArg Reg4 (LitAdress (LitRegister Reg1)) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = DOUBLE 1.0, programCounter = 0}
      it "set Literal register to SVM Register" $ do
        setRegWithAnyArg Reg4 (LitRegister Reg3) testSVMState `shouldBe` SVMState {memory = [INT 0, DOUBLE 1.0,STRING "3",INT 4], register1 = INT 1, register2 = DOUBLE 2.0, register3 = STRING "reg3", register4 = STRING "reg3", programCounter = 0}