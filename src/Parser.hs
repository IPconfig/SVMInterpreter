{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Parser where

import Control.Monad (void)
import Text.Megaparsec -- megaparsec generates parsers and can chain them together
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’. We can optimize our parser by using Data.Text instead
import Data.Char
import qualified Text.Megaparsec.Lexer as L

-- We change every instance of float in the AST to Double since Float should be avoided (see https://wiki.haskell.org/Performance/Floating_point)
data Value = Int | Double | String deriving (Show, Eq)

-- Discriminated union for the 4 registers of the SVM
data Register = Reg1 Value
              | Reg2 Value
              | Reg3 Value
              | Reg4 Value
deriving instance Show (Register)
deriving instance Eq (Register)

-- Data structures representing the constant values of the language. 
-- Address may contain the Integer representing the memory address or the register from which the address is read.
-- data Literal = LInt Integer
--              | LFloat Float
--              | LString String
--              | LAdress Literal
--              | LRegister Register
-- deriving instance Show (Literal)
-- deriving instance Eq (Literal)

-- Instructions supported by the SVM. See the documentation for further details.            
-- We still need address and Register somewhere            
data Instruction = Nop
                   | Neg Instruction
--                   | Literal Literal
--                   | Register Register

--                 | Mov Literal Literal
--                 | And Register Literal
--                 | Or Register Literal
--                 | Not Register
--                 | Mod Register Literal
                   | Var String
                   | IntConst Integer
                   | FloatConst Float
                   | Op Binop Instruction Instruction

--                   | Add2 Register Literal
--                 | Sub Register Literal
--                 | Mul Register Literal
--                 | Div Register Literal

--                 | Cmp Register Literal
--                 | Jmp String
--                 | Jc String Register
--                 | Jeq String Register
--                 | Label String
                   | Program [Instruction]
deriving instance Show (Instruction)
deriving instance Eq (Instruction)

data Binop = Add | Sub | Mul | Div
  deriving (Eq, Ord, Show)

-- data SVM = SVM { memory :: [Value]
--                 , reg1 :: Value
--                 , reg2 :: Value
--                 , reg3 :: Value
--                 , reg4 :: Value
--  --               , ProgramCounter :: Int
--  --               , Labels :: Map(String, Int)
--  --               , Stack :: Map (String, [Value])
--   }

-- remove whitsepace
spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

-- extract a token from a stream of characters, consume all whitespaces after this token
parseLexeme :: Parser a -> Parser a
parseLexeme = L.lexeme spaceConsumer

-- symbol takes a string as argument and parses this string with whitespace after it
parseSymbol :: String -> Parser String
parseSymbol = L.symbol spaceConsumer

  -- | 'parseInteger' parses an integer
parseInteger :: Parser Integer
parseInteger = parseLexeme L.integer

-- | 'parseDouble' parses a double
parseDouble :: Parser Double -- Megaparsec uses a double internally since float should be avoided in Haskell
parseDouble = parseLexeme L.float

-- | 'parseWord' parses a word
parseWord :: Parser String 
parseWord = parseLexeme $ (some alphaNumChar)

-- | 'parseWords' parses multiple words
parseWords :: Parser [String]
parseWords = some parseWord

-- | 'parseHashtag' parses a hashtag symbol
parseHashtag :: Parser String
parseHashtag = parseSymbol "#"

parseLabel :: Parser String
parseLabel = (parseLexeme . try) (p)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (parseSymbol "(") (parseSymbol ")")


--  parseInstruction :: String -> Parser ()
--  parseInstruction word = string word *> notFollowedBy alphaNumChar *> spaceConsumer

reservedInstructions :: [String] -- list of reserved words
reservedInstructions = ["nop","mov","and","or","not","mod","add","sub","mul","div","cmp", "jmp", "jc", "jeq"]

parseInstruction :: Parser String
parseInstruction = (parseLexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedInstructions
                then return x
                else fail $ "instruction " ++ show x ++ " is not reserved"



--Parser
whileParser :: Parser Instruction
whileParser = between spaceConsumer eof instruction --remove initial whitespcace since we only remove after the tokens

instruction :: Parser Instruction
instruction = parens instruction  -- <|> instructionProgram

-- instructionProgram :: Parser Instruction
-- instructionProgram = f <$> sepBy1 instruction' spaceChar
--   -- if there's only one program return it without using ‘Program’
--   where f l = if length l == 1 then head l else Program l

-- instruction' :: Parser Instruction
-- -- instruction' = ifStmt <|> whileStmt <|> skipStmt <|> assignStmt
-- instruction' = undefined

-- ifStmt :: Parser Instruction
-- ifStmt = do
--   rword "if"
--   cond  <- bExpr
--   rword "then"
--   stmt1 <- stmt
--   rword "else"
--   stmt2 <- stmt
--   return (If cond stmt1 stmt2)

-- whileStmt :: Parser Instruction
-- whileStmt = do
--   rword "while"
--   cond <- bExpr
--   rword "do"
--   stmt1 <- stmt
--   return (While cond stmt1)

-- assignStmt :: Parser Instruction
-- assignStmt = do
--   var  <- identifier
--   void (symbol ":=")
--   expr <- aExpr
--   return (Assign var expr)

-- skipInstruction :: Parser Instruction
-- skipInstruction = Nop <$ parseInstruction "nop"



-- aExpr :: Parser Instruction
-- aExpr = makeExprParser aTerm aOperators

-- aOperators :: [[Operator Parser Instruction]]
-- aOperators =
--   [ [Prefix (Neg <$ symbol "-") ]
--   , [ InfixL (Op Mul <$ symbol "*")
--     , InfixL (Op Div <$ symbol "/") ]
--   , [ InfixL (Op Add <$ parseInstruction "add")
--     , InfixL (Op Sub <$ symbol "-") ]
--   ]

-- aTerm :: Parser Instruction
-- aTerm = parens aExpr
--     <|>  Var <$> parseLabel
--     <|>  IntConst <$> parseInteger


-- data SimpleExpr = Num Integer
--                 | Var String
--                 | Add SimpleExpr SimpleExpr
--                 | Parens SimpleExpr
--                   deriving (Eq,Show)




-- Data structures representing the constant values of the language. 
-- Address may contain the Integer representing the memory address or the register from which the address is read.
data Literal = LitInt Integer
             | LitFloat Double
             | LitString String
             | LitAdress Literal
             | LitRegister Register
deriving instance Show (Literal)
deriving instance Eq (Literal)

-- | Literal Integer
parseLitInt :: Parser Literal
parseLitInt = LitInt <$> parseInteger

-- | Literal Float/Double
parseLitFloat :: Parser Literal
parseLitFloat = LitFloat <$> parseDouble

parseLitString :: Parser Literal
parseLitString = parseLexeme $ do 
  stringChar <- some alphaNumChar
  return $ LitString $ stringChar

-- | Memory Adress
-- | Memory adresses are defined as integer numbers enclosed by square brackets
-- | FE: [100], which means “the memory location at index 100”.
parseMemoryAdress :: Parser Literal
parseMemoryAdress = parseLexeme $ do 
  _ <- char '['
  adress <- some digitChar
  _ <- char ']'
  return $ LitAdress $ LitInt $ (read adress) --convert [char] to int

-- | Register Reference
-- | they are used when a register contains an integer number representing the address of a memory location, rather than a value. They are defined by enclosing the register keyword with square brackets. 
-- | FE: [reg1] means “the content of the memory address stored in 'reg1'" -> if the register contains the value 1000, this will use the content of the memory location at 1000.
parseRegisterReference :: Parser Literal
parseRegisterReference = parseLexeme $ do
    _ <- char '['
    fc <- firstChar
    rest <- many nonFirstChar
    _ <- char ']'
    return $ LitAdress $ LitString (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

-- Parser that can try both parsers of LitAdress
parseMemoryAdressOrReference :: Parser Literal
parseMemoryAdressOrReference = try parseMemoryAdress <|> parseRegisterReference

-- | they are simply denoted with the keywords reg1, reg2, reg3, and reg4
parseLitRegister :: Parser Literal
parseLitRegister = undefined
-- LitRegister

-- | parse all literal data structures
parseLiterals :: Parser Literal
parseLiterals = choice [parseMemoryAdressOrReference 
                        , parseLitString
                        , parseLitFloat
                        , parseLitInt ]
