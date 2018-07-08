{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Parser
( module Parser
, parse
, parseTest'
) where

import           Control.Monad
import           Data.Semigroup
import           Data.String                (IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr
import Syntax

{-

  Lexer

-}

type Parser = Parsec Void Text

scn :: Parser ()
scn = L.space (void spaceChar) (L.skipLineComment "#") empty

sc :: Parser ()
sc = L.space (void $ oneOf [' ', '\t']) (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

angles :: Parser a -> Parser a
angles = between (lexeme $ char '<') (lexeme $ char '>')


-- | Top Level Parser for a PP program
pp :: Parser ParsedProgramme
pp = do
  scn
  P <$> variableAddressSection <*> parameterSection <*> constantSection <*> schemaSection <* eof

section :: Text -> Parser a -> Parser a
section sectionName sectionParser = do
    --       v--- enforce sequential section numbers?
  lexeme $ L.decimal <* (char '.')
  forM (T.words sectionName) $ \word -> (symbol word)
  scn
  sectionParser

variableAddressSection :: Parser VASection
variableAddressSection = do
  section "Variable Addresses" (VA <$> some variableAddressBlock)

variableAddressBlock :: Parser Block
variableAddressBlock = do
  letter <- parens letterChar
  symbol "Block"
  symbol (T.singleton letter)

  numCells <- parens (integer <* symbol "cells")
  scn

  Block numCells <$> some variableAddress

variableAddress :: Parser (Text, SimpleExpr)
variableAddress = do
  (var, subscripts) <- lexeme . angles $ (,) <$> (letterChar <* char '_') <*> (some letterChar)
  symbol "="
  exp <- simpleExpr (oneOf subscripts)
  scn
  return (T.pack $ var : '_' : subscripts, exp)

simpleExpr :: Parser Char -> Parser SimpleExpr
simpleExpr vars = makeExprParser (constant <|> expVar vars) opTable

constant :: Parser SimpleExpr
constant = lexeme $ SConstant <$> integer

expVar :: Parser Char -> Parser SimpleExpr
expVar vars = lexeme $ SExpVar <$> vars

opTable :: [[Operator Parser SimpleExpr]]
opTable =
  [ [ binary (symbol "*") STimes]
  , [ binary (symbol "+") SAdd
    ]
  ]

binary :: Parser Text -> (a -> a -> a) -> Operator Parser a
binary op f = InfixL $ do
  op
  return f

parameterSection :: Parser [Parameter]
parameterSection = section "Parameters" (some parameterDecl)

parameterDecl :: Parser Parameter
parameterDecl = inFin <|> characteristic

inFin :: Parser Parameter
inFin = do
  var <- simpleVar
  symbol ":"

  init <- (lexeme $ string (unVar var) <* string "_in") *> symbol "=" *> integer
  symbol ","
  fin  <- (lexeme $ string (unVar var) <* string "_fin") *> symbol "=" *> integer
  scn

  return $ InFin var init fin


-- This syntax is inferred from the book and context, no actual example of a so called 'characteristic loop'
-- has been found
characteristic :: Parser Parameter
characteristic = do
  var <- simpleVar
  symbol ":"
  init <- (lexeme $ string (unVar var) <* string "_in") *> symbol "=" *> integer
  symbol ","

  a <- variable
  op <- symbol "<" >> return Comparison
    <|> (symbol "|<|" >> return WordComparison )
    <|> (symbol ",<"  >> return ModuliComparison)
  b <- variable

  return $ Charateristic op init a b

constantSection :: Parser [SimpleExpr]
constantSection = do
  section "List of Constants and Variable Quantities" (list (constant <|> expVar letterChar)) <* scn

schemaSection :: Parser LogicalSchema
schemaSection = section "Logical Scheme"  $ do
  schemaParser

schemaParser :: Parser LogicalSchema
schemaParser = Seq <$> some (schemaTerm <* optional (symbol ";") <* scn)

schemaTerm :: Parser LogicalSchema
schemaTerm = loop <|> printExpr <|> assign <|> logicalOperator <|> opSign <|> stop

loop :: Parser LogicalSchema
loop = between (char '[') (char ']') $ do
  var <- lexeme letterChar
  schema <- schemaParser

  return $ Loop var schema

assign :: Parser LogicalSchema
assign = do
  exp <- try $ schemaExpr <* symbol "=>"
  var <- variable

  return $ Assign exp var

variable :: Parser Variable
variable = complexVar <|> simpleVar

simpleVar :: Parser Variable
simpleVar = lexeme $ Var <$> (T.singleton <$> letterChar)

complexVar :: Parser Variable
complexVar = liftM Var $ lexeme $ do
  a <- try $ letterChar <* char '_'
  sub <- some letterChar

  return . T.pack $ a : '_' : sub

printExpr :: Parser LogicalSchema
printExpr = do
   exp <- try $ schemaExpr <* (mapM_ symbol [",", "=>", "0"])

   return $ Print exp

logicalOperator :: Parser LogicalSchema
logicalOperator = lexeme $ (*>) (char 'P') $ parens $ do
  var <- variable
  symbol ","

  opSign <- lexeme $ some hexDigitChar
  symbol ";"

  branches <- some logicalBranch
  return $ LogicalOperator var (T.pack opSign) branches
  where
  logicalBranch = do
    opSign <- lexeme $ some hexDigitChar
    symbol "/"

    (,) <$> pure (T.pack opSign) <*> range

  range = do
    open <- symbol "(" <|> symbol "["
    a <- variable <|> (Var <$> symbol "-∞")
    symbol ","
    b <- variable <|> (Var <$> symbol "∞")
    close <- symbol ")" <|> symbol "]"

    case (open, a, b, close) of
      ("[", "-∞",  _ ,  _ ) -> fail "Can't have an left-inclusive interval with -∞"
      ("(", "-∞",  a , ")") -> return $ LeftImproperInterval a
      ("(", "-∞",  a , "]") -> return $ LeftImproperSemiInterval a
      ( _ ,   _ , "∞", "]") -> fail "Can't have an left-inclusive interval with -∞"
      ("[",   a , "∞", ")") -> return $ RightImproperInterval a
      ("(",   a , "∞", ")") -> return $ RightImproperSemiInterval a
      ("[",   a ,  b , "]") -> return $ Segment a b
      ("[",   a ,  b , ")") -> return $ SemiInterval a b
      ("(",   a ,  b , "]") -> return $ SemiSegment a b
      ("(",   a ,  b , ")") -> return $ Interval a b

opSign :: Parser LogicalSchema
opSign = char 'L' *> ((OpLabel . T.pack) <$> (lexeme $ some hexDigitChar))

stop :: Parser LogicalSchema
stop = symbol "stop" *> return Stop

schemaExpr :: Parser SchemaExpr
schemaExpr = makeExprParser baseTerm schemaOpTable
  where
  baseTerm = Constant <$> integer <|> (symbol "Form" *> (Form <$> variable)) <|> ExpVar <$> variable

schemaOpTable :: [[Operator Parser SchemaExpr]]
schemaOpTable =
  [ [ binary (symbol "*") Times
    , binary (symbol ":") Div
    ]
  , [ binary (symbol "+") Add
    , binary (symbol "-") Minus
    ]
  ]

list :: Parser a -> Parser [a]
list a = a `sepBy` symbol ","

