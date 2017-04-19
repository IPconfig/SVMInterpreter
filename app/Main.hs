module Main where

import Text.Megaparsec hiding (option)
import Options.Applicative
import Data.Semigroup ((<>))
import Parser
import Interpreter
import Core

data Config = Config
    { filename     :: String
    , memorysize       :: Int
    }

main :: IO ()
main = undefined

initialized :: Config -> SVMState
initialized (Config _ memorySize) = SVMState
    { memory = replicate memorySize (INT 0)
    , register1 = INT 0
    , register2 = INT 0
    , register3 = INT 0
    , register4 = INT 0
    , programCounter = 0
    } 

configParse :: Options.Applicative.Parser Config
configParse = Config
          <$> strArgument (
          --      long "file"
                metavar "FILENAME"
             <> help "The hasm file to execute." )
          <*> option auto (
                short 'm'
             <> long "memory"
             <> metavar "M"
             <> help "How much memory the VM has during execution. Defaults to 10 banks."
             <> value 10 )

opts :: ParserInfo Config
opts = info (helper <*> configParse)
     ( fullDesc
    <> progDesc "Runs a svm file and dumps the final state"
    <> header "SVM - an assembly-like programming language for a virtual machine" )

run :: IO ()
run = parseAndEval' =<< execParser opts

parseAndEval' :: Config -> IO ()
parseAndEval' config@(Config file _) = do
  svmFile <- readFile file
  case (parse whileParser "" svmFile) of
    Left err -> putStr (parseErrorPretty err)
    Right parsedResults -> printSVM $ eval (initialized config) parsedResults


-- parseAndEval
parseAndEval :: FilePath -> IO()
parseAndEval filename' = do
  svmFile <- readFile filename'
  case (parse whileParser "" svmFile) of
    Left err -> putStr (parseErrorPretty err)
    Right parsedResults -> printSVM $ eval emptySVMState parsedResults