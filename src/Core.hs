module Core where
import ADT

data Value = INT Int | DOUBLE Double | STRING String deriving (Eq)
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
createMemory :: Int -> Memory
createMemory n = replicate n (INT 0)