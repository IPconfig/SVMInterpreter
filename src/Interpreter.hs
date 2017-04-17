module Interpreter where
import Core
import ADT

eval :: Instruction -> SVMState -> SVMState
eval instruction svm = case instruction of
  Nop -> updateProgramCounter svm
  Mov arg1 arg2 -> case arg1 of -- Mov (LitAdress(LitInt 10)) (LitInt 5)
    (LitAdress (LitInt x)) -> setMemWithAnyArg x arg2 svm
    (LitAdress (LitRegister x)) -> trySetMemFromReg x arg2 svm
    (LitRegister x) -> setRegWithAnyArg x arg2 svm
    _ -> error "Invalid MOV structure"
  Not reg -> case getRegister reg svm of
    (INT x) ->  if x >= 0 then setRegister reg (INT (-1)) svm else setRegister reg (INT 0) svm
    _ -> error "NOT accepts only integer values"