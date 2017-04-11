module Interpreter where
import Core
import ADT

eval :: Instruction -> SVMState -> SVMState
eval instruction svm = case instruction of
  Nop -> updateProgramCounter svm
  Mov left right -> case left of -- Mov (LitAdress(LitInt 10)) (LitInt 5)
    (LitAdress (LitInt x)) -> setMemWithAnyArg x right svm
    (LitAdress (LitRegister x)) -> trySetMemFromReg x right svm
    (LitRegister x) -> setRegWithAnyArg x right svm
    _ -> error "Invalid MOV structure"