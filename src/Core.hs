-- | Core implements all SVMState manipulations (Memory and Registers)
{-# LANGUAGE RecordWildCards #-} -- Enable this extension so we dont have to write every field in a record, but can simply use '..'
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

printSVM :: SVMState -> IO()
printSVM SVMState {memory = mem
                  , register1 = sreg1
                  , register2 = sreg2
                  , register3 = sreg3
                  , register4 = sreg4
                  , programCounter = spc } = do
  putStrLn "MEMORY:"
  printMemory mem
  putStrLn "\n=============="
  putStrLn "REGISTERS:"
  putStrLn $ show sreg1 ++ "\t" ++ show sreg2 ++ "\t" ++ show sreg3 ++ "\t" ++ show sreg4
  putStrLn "\n=============="
  putStrLn "PROGRAMCOUNTER:" 
  putStrLn $ show spc

emptySVMState :: SVMState
emptySVMState = SVMState
    { memory = createMemory 20
    , register1 = INT 0
    , register2 = INT 0
    , register3 = INT 0
    , register4 = INT 0
    , programCounter = 0
    } 

createMemory :: Integer -> Memory
createMemory n = replicate (fromInteger n) (INT 0)

readMemory :: Integer -> Memory -> Value
readMemory _ [] = error "empty memory. This should never happen"
readMemory y (x:xs) | y <= 0 = x -- Start index at 0
                             | otherwise = readMemory (y-1) xs

-- | MEMORY FUNCTIONS 
getMemory :: Integer -> SVMState -> Value
getMemory adress SVMState {memory = mem} 
  =  readMemory adress mem

setMemory :: Integer -> Value -> SVMState -> SVMState
setMemory adress value svm@(SVMState{ memory = mem })
  | adress < (toInteger (length mem)) = case splitAt (fromInteger(adress)) mem of
                                 (front, back) -> svm { memory = front ++ value : (tail back)}
   | otherwise = error "memory adress out of bounds"
                                  
setMemWithAnyArg :: Integer -> Literal -> SVMState -> SVMState
setMemWithAnyArg adress lit svm = case lit of
  (LitInt x) -> setMemory adress (INT x) svm
  (LitFloat x) -> setMemory adress (DOUBLE x) svm
  (LitString x) -> setMemory adress (STRING x) svm
  (LitAdress (LitInt x)) -> setMemory adress (getMemory x svm) svm
  (LitAdress (LitRegister x)) -> setMemory adress (getMemory(getAdressFromRegister x svm) svm) svm
  (LitRegister x) -> setMemory adress (getRegister x svm) svm
  _ -> error "invalid right argument structure"

printMemory :: Memory -> IO ()
printMemory mem = mapM_ putStr $ format $ memoryToString where
  memoryToString = map show mem  
  format list = zip list (cycle [1 :: Integer ..10]) >>= function where -- Create cyclic indexes from 1 till 10
    function (x,k) = if k == 10 then [x,"\n"] else [x, "\t"] -- If 10th index, add "\n", else "\t"

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

trySetMemFromReg :: Register -> Literal -> SVMState -> SVMState
trySetMemFromReg reg lit svm = case getRegister reg svm of
  (INT x) -> setMemWithAnyArg x lit svm
  _ -> error "The register does not contain an Integer"

setRegWithAnyArg :: Register -> Literal -> SVMState -> SVMState
setRegWithAnyArg reg lit svm = case lit of
  (LitInt x) -> setRegister reg (INT x) svm
  (LitFloat x) -> setRegister reg (DOUBLE x) svm
  (LitString x) -> setRegister reg (STRING x) svm
  (LitAdress (LitInt x)) -> setRegister reg (getMemory x svm) svm
  (LitAdress (LitRegister x)) -> setRegister reg (getMemory(getAdressFromRegister x svm) svm) svm
  (LitRegister x) -> setRegister reg (getRegister x svm) svm
  _ -> error "invalid right argument structure"

-- | OTHER FUNCTIONS
updateProgramCounter :: SVMState -> SVMState
updateProgramCounter svm@(SVMState { .. }) = svm {programCounter = programCounter + 1 }

-- | 'getBinaryValues' returns a tuple containing the Value-datatype from a stored value in register and the Literal definition
getBinaryValues :: Register -> Literal -> SVMState -> (Value, Value)
getBinaryValues reg literal svm = (arg1, arg2)
  where arg1 = getRegister reg svm
        arg2 = case literal of
             (LitInt x) -> (INT x)
             (LitFloat x) -> (DOUBLE x)
             (LitString x) -> (STRING x)
             (LitAdress (LitInt x)) -> (getMemory x svm) -- example: move contents from [2] to Reg 1
             (LitAdress (LitRegister x)) -> (getMemory(getAdressFromRegister x svm) svm) -- move contents from [reg1] to reg4
             (LitRegister x) -> (getRegister x svm)
             _ -> error "invalid binary operator argument"