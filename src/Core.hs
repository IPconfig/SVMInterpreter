-- | Core implements all SVMState manipulations (Memory and Registers)
module Core where
import ADT

data Value = INT Integer | DOUBLE Double | STRING String deriving (Eq)
instance Show Value where
  show (INT x) = show x
  show (DOUBLE x) = show x
  show (STRING x) = show x
  
type Memory = [Value]
data SVMState = SVMState { memory :: Memory
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
    , register2 = INT 0
    , register3 = INT 0
    , register4 = INT 0
    , programCounter = 0
    } 

createMemory :: Int -> Memory
createMemory n = replicate n (INT 0)

readMemory :: Integer -> Memory -> Value
readMemory _ [] = error "empty memory. This should never happen"
readMemory y (x:xs) | y <= 0 = x -- Start index at 0
                             | otherwise = readMemory (y-1) xs

-- | MEMORY FUNCTIONS 
getMemory :: Integer -> SVMState -> Value
getMemory adress SVMState {memory = mem} 
  =  readMemory adress mem

setMemory :: Int -> Value -> SVMState -> SVMState
setMemory adress value SVMState{memory = mem
  , register1 = sreg1
  , register2 = sreg2
  , register3 = sreg3
  , register4 = sreg4
  , programCounter = spc }
  | adress < length mem = case splitAt adress mem of
                                 (front, back) -> SVMState { memory = front ++ value : (tail back)
                                 , register1 = sreg1
                                 , register2 = sreg2
                                 , register3 = sreg3
                                 , register4 = sreg4
                                 , programCounter = spc }
   | otherwise = error "memory adress out of bounds"
                                  
setMemWithAnyArg :: Int -> Literal -> SVMState -> SVMState
setMemWithAnyArg adress lit svm = case lit of
  (LitInt x) -> setMemory adress (INT x) svm
  (LitFloat x) -> setMemory adress (DOUBLE x) svm
  (LitString x) -> setMemory adress (STRING x) svm
  (LitAdress (LitInt x)) -> setMemory adress (getMemory x svm) svm
  (LitAdress (LitRegister x)) -> setMemory adress (getMemory(getAdressFromRegister x svm) svm) svm
  (LitRegister x) -> setMemory adress (getRegister x svm) svm
  _ -> error "invalid right argument structure"

printMemory :: Memory -> String
printMemory = undefined


-- | REGISTER FUNCTIONS
setRegister :: Register -> Value -> SVMState -> SVMState
setRegister reg value svm = case reg of
    Reg1 -> svm { register1 = value }
    Reg2 -> svm { register2 = value }
    Reg3 -> svm { register3 = value }
    Reg4 -> svm { register4 = value }

getRegister :: Register -> SVMState -> Value
getRegister reg svm = case reg of
  Reg1 -> register1 svm
  Reg2 -> register2 svm
  Reg3 -> register3 svm
  Reg4 -> register4 svm

getAdressFromRegister :: Register -> SVMState -> Integer
getAdressFromRegister reg svm = case getRegister reg svm of
  (INT x) -> x
  _ -> error "The register does not contain an Integer"

setRegWithAnyArg :: Register -> Literal -> SVMState -> SVMState
setRegWithAnyArg reg lit svm = case lit of
  (LitInt x) -> setRegister reg (INT x) svm
  (LitFloat x) -> setRegister reg (DOUBLE x) svm
  (LitString x) -> setRegister reg (STRING x) svm
  (LitAdress (LitInt x)) -> setRegister reg (getMemory x svm) svm -- example: move contents from [2] to Reg 1
  (LitAdress (LitRegister x)) -> setRegister reg (getMemory(getAdressFromRegister x svm) svm) svm -- move contents from [reg1] to reg4
  (LitRegister x) -> setRegister reg (getRegister x svm) svm
  _ -> error "invalid right argument structure"