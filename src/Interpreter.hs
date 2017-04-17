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
  And arg1 arg2 -> case getBinaryValues arg1 arg2 svm of
    ((INT x), (INT y)) -> if x >= 0 && y>= 0 then setRegister arg1 (INT 1) svm else setRegister arg1 (INT (-1)) svm
    _ -> error "AND can be applied only to integer values"
  Or arg1 arg2 -> case getBinaryValues arg1 arg2 svm of
    ((INT x), (INT y)) -> if x < 0 && y < 0 then setRegister arg1 (INT (-1)) svm else setRegister arg1 (INT 1) svm
    _ -> error "OR can be applied only to integer values"
  Not reg -> case getRegister reg svm of
    (INT x) ->  if x >= 0 then setRegister reg (INT (-1)) svm else setRegister reg (INT 0) svm
    _ -> error "NOT accepts only integer values in register"
  Mod arg1 arg2 -> case getBinaryValues arg1 arg2 svm of
    ((INT x), (INT y)) -> setRegister arg1 (INT (x `mod` y)) svm
    _ -> error "MOD can be applied only to integer values"
  Add arg1 arg2 -> case getBinaryValues arg1 arg2 svm of
    ((INT x), (INT y)) -> setRegister arg1 (INT (x + y)) svm
    ((DOUBLE x), (DOUBLE y)) -> setRegister arg1 (DOUBLE (x + y)) svm
    ((STRING x), (STRING y)) -> setRegister arg1 (STRING (x ++ y)) svm
    _ -> error "ADD accepts only integer, double or strings as arguments"
  Sub arg1 arg2 -> case getBinaryValues arg1 arg2 svm of
    ((INT x), (INT y)) -> setRegister arg1 (INT (x - y)) svm
    ((DOUBLE x), (DOUBLE y)) -> setRegister arg1 (DOUBLE (x - y)) svm
    _ -> error "SUB accepts only integer or double as arguments"
  Mul arg1 arg2 -> case getBinaryValues arg1 arg2 svm of
    ((INT x), (INT y)) -> setRegister arg1 (INT (x * y)) svm
    ((DOUBLE x), (DOUBLE y)) -> setRegister arg1 (DOUBLE (x * y)) svm
    _ -> error "MUL accepts only integer or double as arguments"
  Div arg1 arg2 -> case getBinaryValues arg1 arg2 svm of
    ((INT x), (INT y)) -> setRegister arg1 (INT (x `div` y)) svm -- `div` can only be applied to integral numbers
    ((DOUBLE x), (DOUBLE y)) -> setRegister arg1 (DOUBLE (x / y)) svm -- (/) only can be applied to fractional numbers
    _ -> error "DIV accepts only integer or double as arguments"
  Cmp arg1 arg2 -> case getBinaryValues arg1 arg2 svm of
    ((INT x), (INT y)) -> compareFunc x y -- `div` can only be applied to integral numbers
    ((DOUBLE x), (DOUBLE y)) -> compareFunc x y -- (/) only can be applied to fractional numbers
    _ -> error "Cmp accepts only integer or double as arguments" 
    where 
      compareFunc x y 
        | x < y     = setRegister arg1 (INT (-1)) svm
        | x == y    = setRegister arg1 (INT 0) svm
        | otherwise = setRegister arg1 (INT 1) svm
  Label label-> svm -- do Nothing
  Jmp label -> svm
  Jc label register -> svm
  Jeq label register -> svm