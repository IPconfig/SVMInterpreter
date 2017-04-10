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
--setRegWithAnyArgument :: Register -> Literal -> SVM

-- usage:: setRegister Reg1 (STRING("set Reg 1 to this text")) emptySVMState
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

createMemory :: Int -> Memory
createMemory n = replicate n (INT 0)

readMemory :: Int -> Memory -> Value
readMemory _ [] = error "empty memory. This should never happen" --runtime exception
readMemory y (x:xs) | y <= 0 = x -- Start index at 0
                             | otherwise = readMemory (y-1) xs
                             
setMemory :: Int -> Value -> Memory -> Memory
setMemory adress value mem
  | adress < length mem = case splitAt adress mem of
                            (front, back) -> front ++ value : back
  --                          _ -> mem -- if the list doesn't have an element at index adress. Use this or the runtimeError
  | otherwise = error "memory adress out of bounds" --runtime exception

printMemory :: Memory -> String
printMemory = undefined