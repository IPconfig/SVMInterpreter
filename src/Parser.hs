-- | Parser parses a given String/File into the data structure defined in ADT
module Parser where
import ADT
import Control.Monad (void)
import Text.Megaparsec -- megaparsec generates parsers and can chain them together
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’. We can optimize our parser by using Data.Text instead
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

-- | 'parseId' parses an identitifier
parseId :: Parser String
parseId = parseLexeme $ do 
  letter <- some letterChar
  rest <- many (alphaNumChar <|> char '_')
  return $ letter ++ rest

-- | 'parseLabel' parses a Label.
-- Labels simply use the symbol # followed by a variable name, which can be any alphanumerical sequence of characters starting with a letter. They can also contain the symbol ’_'
parseLabel :: Parser String
parseLabel = parseLexeme $ do
  hash <- parseSymbol "#"
  letter <- some letterChar
  rest <- many (alphaNumChar <|> char '_')
  return $ hash ++ letter ++ rest

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (parseSymbol "(") (parseSymbol ")")

-- | 'parseRegister' parses a register value
parseRegister :: Parser Register
parseRegister = choice [Reg1 <$ string "reg1" -- (<$) parse a keyword and return a no argument constructor
                        , Reg2 <$ string "reg2"
                        , Reg3 <$ string "reg3"
                        , Reg4 <$ string "reg4"] <* spaceConsumer

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
  return $ LitAdress $ LitInt $ (read adress) --convert [char] to integer with read

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
parseLiteral :: Parser Literal
parseLiteral = makeExprParser litTerm litOperators <* spaceConsumer -- Empty operator list since we don't have operators for Literals.

litOperators :: [[Operator Parser Literal]]
litOperators =
   [ 
     [prefix "-" negate']
   ]
  where 
    prefix symbol fun = Prefix ( fun <$ parseSymbol symbol)

negate' :: Literal -> Literal
negate' input = case input of 
  (LitInt x) -> (LitInt (0 - x))
  (LitFloat x) -> (LitFloat(0.0 -x))
  _ -> error "trying to appy (-) to something other then a numeric value"


litTerm :: Parser Literal
litTerm = parens parseLiteral
  <|> try parseLitFloat --parser will eat integers and fail without try
  <|> parseLitInt
  <|> parseLitRegister
  <|> parseMemoryAdressOrReference
  <|> parseLitString

nopE :: Parser Instruction
nopE = Nop <$ rword "nop"

movE :: Parser Instruction
movE = do
  _  <- rword "mov"
  e0 <- parseLiteral
  e1 <- parseLiteral
  return (Mov e0 e1)

andE :: Parser Instruction
andE = do
  _  <- rword "and"
  e0 <- parseRegister
  e1 <- parseLiteral
  return (And e0 e1)

orE :: Parser Instruction
orE = do
  _  <- rword "or"
  e0 <- parseRegister
  e1 <- parseLiteral
  return (Or e0 e1)

notE :: Parser Instruction
notE = do
  _ <- rword "not"
  e <- parseRegister
  return (Not e)

-- arithmics 
modE :: Parser Instruction
modE = do
  _  <- rword "mod"
  e0 <- parseRegister
  e1 <- parseLiteral
  return (Mod e0 e1)

mulE :: Parser Instruction
mulE = do
  _  <- rword "mul"
  e0 <- parseRegister
  e1 <- parseLiteral
  return (Mul e0 e1)

divE :: Parser Instruction
divE = do
  _  <- rword "div"
  e0 <- parseRegister
  e1 <- parseLiteral
  return (Div e0 e1)

addE :: Parser Instruction
addE = do
  _  <- rword "add"
  e0 <- parseRegister
  e1 <- parseLiteral
  return (Add e0 e1)

subE :: Parser Instruction
subE = do
  _  <- rword "sub"
  e0 <- parseRegister
  e1 <- parseLiteral
  return (Sub e0 e1)

-- memory manipulation
cmpE :: Parser Instruction
cmpE = do
  _  <- rword "cmp"
  e0 <- parseRegister
  e1 <- parseLiteral
  return (Cmp e0 e1)

-- jump statements
jmpE :: Parser Instruction
jmpE = do
  _  <- rword "jmp"
  e0 <- parseId
  return (Jmp e0)

jcE :: Parser Instruction
jcE = do
  _  <- rword "jc"
  e0 <- parseId
  e1 <- parseRegister
  return (Jc e0 e1)

jeqE :: Parser Instruction
jeqE = do
  _  <- rword "jeq"
  e0 <- parseId
  e1 <- parseRegister
  return (Jeq e0 e1)

labelE :: Parser Instruction
labelE = do
  e <- parseLabel
  return (LabelI e)

instructionProgram :: Parser Instruction
instructionProgram = f <$> sepBy1 instruction' (many $ oneOf "\n") -- Seperate instructions by newline
  where f l = if length l == 1 then head l else Program l   -- if there's only one instruction return it without using ‘Program’

instruction' :: Parser Instruction
instruction' = nopE <|> movE 
            <|> andE <|> orE 
            <|> notE <|> modE 
            <|> mulE <|> divE 
            <|> addE <|> subE 
            <|> cmpE <|> jmpE 
            <|> jcE <|> jeqE 
            <|> labelE

instruction :: Parser Instruction
instruction = parens instruction  <|> instructionProgram

--Parser
whileParser :: Parser Instruction
whileParser = between spaceConsumer eof instruction --remove initial whitespcace since we only remove after the tokens

-- usage | parseFile "test.svm"
parseFile :: FilePath -> IO ()
parseFile filename = do
  svmCode <- readFile filename
  case (parse whileParser "" svmCode) of
    Left err -> putStr (parseErrorPretty err)
    Right results -> print results

-- mainFileParser :: String -> IO [Grammar.ProgramLine]
-- mainFileParser filename = do
--     f <- readFile filename
--     return $ map mainParser (lines f)