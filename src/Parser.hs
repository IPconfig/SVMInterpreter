{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Parser where

import ADT
import Control.Monad (void)
import Text.Megaparsec -- megaparsec generates parsers and can chain them together
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’. We can optimize our parser by using Data.Text instead
import Data.Char
import qualified Text.Megaparsec.Lexer as L





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

-- | parses a given string
rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> spaceConsumer

-- | 'parseWords' parses multiple words
parseWords :: Parser [String]
parseWords = some parseWord

-- | 'parseLabel' parses a Label.
-- Labels simply use the symbol # followed by a variable name, which can be any alphanumerical sequence of characters starting with a letter. They can also contain the symbol ’_'
parseLabel :: Parser String
parseLabel = parseLexeme $ do
  hash <- parseSymbol "#"
  letter <- some letterChar
  rest <- some (alphaNumChar <|> char '_')
  return $ hash ++ letter ++ rest

-- | 'parseRegister' parses a register value
parseRegister :: Parser Register
parseRegister = choice [Reg1 <$ string "reg1" -- (<$) parse a keyword and return a no argument constructor
                        , Reg2 <$ string "reg2"
                        , Reg3 <$ string "reg3"
                        , Reg4 <$ string "reg4"]

-- | Literal Integer
parseLitInt :: Parser Literal
parseLitInt = LitInt <$> parseInteger

-- | Literal Float/Double
parseLitFloat :: Parser Literal
parseLitFloat = LitFloat <$> parseDouble

-- | Literal String
parseLitString :: Parser Literal
parseLitString = LitString <$> parseWord

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
    reg <- parseLitRegister
    _ <- char ']'
    return $ LitAdress $ reg

-- Parser that can try both parsers of LitAdress
parseMemoryAdressOrReference :: Parser Literal
parseMemoryAdressOrReference = try parseMemoryAdress <|> parseRegisterReference

parseLitRegister :: Parser Literal
parseLitRegister = LitRegister <$> parseRegister


-- | parse all literal data structures
parseLiterals :: Parser Literal
parseLiterals = parseMemoryAdressOrReference <|> parseLitRegister <|> try parseLitFloat <|> parseLitInt <|> parseLitString

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