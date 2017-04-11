module Core where
import ADT

data Value = INT Integer | DOUBLE Double | STRING String deriving (Eq)
instance Show Value where
  show (INT x) = show x
  show (DOUBLE x) = show x
  show (STRING x) = show x

type Memory = [Value]

data SVMState = SVMState { memory :: [Value]
                         , register1   :: Value
                         , register2   :: Value
                         , register3   :: Value
                         , register4   :: Value
                         , programCounter :: Int
--                         , Labels :: Map<string,int>
--                         , Stack  ::  Map<string,List<Value>> 
                         } deriving (Eq, Show)

emptySVMState :: SVMState
emptySVMState = SVMState
    { memory = createMemory 10
    , register1 = INT 0
    , register2 = STRING "Test"
    , register3 = DOUBLE 0.0
    , register4 = INT 0
    , programCounter = 0
    } 

-- usage:: setRegister Reg1 (STRING("set Reg 1 to this text")) emptySVMState
setRegister :: Register -> Value -> SVMState -> SVMState
setRegister reg value svm = case reg of
    Reg1 -> svm { register1 = value }
    Reg2 -> svm { register2 = value }
    Reg3 -> svm { register3 = value }
    Reg4 -> svm { register4 = value }

-- usage: getRegister Reg3 emptySVMState
getRegister :: Register -> SVMState -> Value
getRegister reg svm = case reg of
  Reg1 -> register1 svm
  Reg2 -> register2 svm
  Reg3 -> register3 svm
  Reg4 -> register4 svm

createMemory :: Int -> Memory
createMemory n = replicate n (INT 0)

readMemory :: Integer -> Memory -> Value
readMemory _ [] = error "empty memory. This should never happen" -- runtime exception
readMemory y (x:xs) | y <= 0 = x -- Start index at 0
                             | otherwise = readMemory (y-1) xs

-- let a = setMemory' 2 (INT 89) emptySVMState
-- getMemory 2 a
getMemory :: Integer -> SVMState -> Value
getMemory adress SVMState {memory = mem} 
  =  readMemory adress mem

setMemory :: Int -> Value -> Memory -> Memory
setMemory adress value mem
  | adress < length mem = case splitAt adress mem of
                            (front, back) -> front ++ value : back
  | otherwise = error "memory adress out of bounds" -- runtime exception

-- | usage getAdressFromRegister Reg1 emptySVMState
getAdressFromRegister :: Register -> SVMState -> Integer
getAdressFromRegister reg svm = case getRegister reg svm of
  (INT x) -> x
  _ -> error "The register does not contain an Integer"

-- usage:: setMemory' 2 (INT 2) emptySVMState
setMemory' :: Int -> Value -> SVMState -> SVMState
setMemory' adress value SVMState{memory = mem
  , register1 = sreg1
  , register2 = sreg2
  , register3 = sreg3
  , register4 = sreg4
  , programCounter = spc }
  | adress < length mem = case splitAt adress mem of
                                 (front, back) -> SVMState { memory = front ++ value : back
                                 , register1 = sreg1
                                 , register2 = sreg2
                                 , register3 = sreg3
                                 , register4 = sreg4
                                 , programCounter = spc }
   | otherwise = error "memory adress out of bounds" -- runtime exception
                                  

printMemory :: Memory -> String
printMemory = undefined

-- usage: setMemWithAnyArg 2 (LitInt 99) emptySVMState
setMemWithAnyArg :: Int -> Literal -> SVMState -> SVMState
setMemWithAnyArg adress lit svm = case lit of
  (LitInt x) -> setMemory' adress (INT x) svm
  (LitFloat x) -> setMemory' adress (DOUBLE x) svm
  (LitString x) -> setMemory' adress (STRING x) svm
  (LitAdress (LitInt x)) -> setMemory' adress (getMemory x svm) svm
  (LitAdress (LitRegister x)) -> setMemory' adress (getMemory(getAdressFromRegister x svm) svm) svm
  (LitRegister x) -> setMemory' adress (getRegister x svm) svm -- setMemWithAnyArg 0 (LitRegister Reg2) emptySVMState
  _ -> error "invalid right argument structure"

--setRegWithAnyArgument :: Register -> Literal -> SVM
setRegWithAnyArg :: Register -> Literal -> SVMState -> SVMState
setRegWithAnyArg reg lit svm = case lit of
  (LitInt x) -> setRegister reg (INT x) svm
  (LitFloat x) -> setRegister reg (DOUBLE x) svm
  (LitString x) -> setRegister reg (STRING x) svm
  -- (LitAdress (LitInt x))
  -- (LitAdress (LitRegister x))
  (LitRegister x) -> setRegister reg (getRegister x svm) svm -- setRegWithAnyArg Reg1 (LitRegister Reg2) emptySVMState
  _ -> error "invalid right argument structure"
