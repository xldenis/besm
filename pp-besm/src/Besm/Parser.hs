{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Besm.Parser
( module Besm.Parser
, parse
, parseTest'
, parseErrorPretty'
) where

import           Control.Monad
import qualified Data.List.NonEmpty         as NL
import           Data.Semigroup
import           Data.String                (IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Besm.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

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

binary :: Parser Text -> (a -> a -> a) -> Operator Parser a
binary op f = InfixL $ do
  op
  return f

-- | Top Level Parser for a PP program
pp :: Parser ParsedProgramme
pp = do
  scn
  P <$> variableAddressSection <*> parameterSection <*> constantSection <*> schemaSection <* eof

-- | Parses the header to a section
section :: Text -> Parser a -> Parser a
section sectionName sectionParser = do
    --       v--- enforce sequential section numbers?
  lexeme $ L.decimal <* (char '.')
  forM (T.words sectionName) $ \word -> (symbol word)
  scn
  sectionParser

{-
  The variable address block contains a list of simple equations (+, *) with up to 3 variables and an offset.

  It also contains a list of loop parameters dependent on 'higher order parameters', described by an equation
  with two variables and an offset.
-}

variableAddressSection :: Parser VASection
variableAddressSection = do
  section "Variable Addresses" (VA <$> many variableAddressBlock)

variableAddressBlock :: Parser Block
variableAddressBlock = do
  letter <- parens letterChar
  symbol "Block"
  symbol (T.singleton letter)

  numCells <- parens (integer <* symbol "cells")
  scn

  (Block numCells . NL.fromList) <$> some variableAddress

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


parameterSection :: Parser [Parameter]
parameterSection = section "Parameters" (many parameterDecl)

parameterDecl :: Parser Parameter
parameterDecl = inFin <|> characteristic


-- | A parameter consisting only of upper and lower bounds.
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
-- It describes loops in which the upper bound is determined dynamically by a logical condition
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

  return $ Charateristic var op init a b

-- The constant section contains a list of _every_ constant and undetermined variable not described in Block V
constantSection :: Parser [SimpleExpr]
constantSection = do
  section "List of Constants and Variable Quantities" (list (constant <|> expVar letterChar)) <* scn

{-
  Block K

  Block K is the 'actual' program.


-}
schemaSection :: Parser LogicalSchema
schemaSection = section "Logical Scheme"  $ do
  schemaParser


-- | Parse a list of schematic statements
schemaParser :: Parser LogicalSchema
schemaParser = (Seq . concatTuples) <$> some ((,) <$> schemaTerm <*> optional (symbol ";" >> pure Semicolon) <* scn)
  where
  concatTuples ((term, Just s ) : ls) = term : s : concatTuples ls
  concatTuples ((term, Nothing) : ls) = term : concatTuples ls
  concatTuples []                     = []

-- | Parse one schema statement
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

quantity :: Parser Quantity
quantity = V <$> variable <|> C <$> integer

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
  var <- quantity
  symbol ","

  opSign <- operatorNumber
  symbol ";"

  branches <- some logicalBranch
  return $ LogicalOperator var opSign branches
  where
  logicalBranch = do
    opSign <- operatorNumber
    symbol "/"

    (,) <$> pure opSign <*> range

  range = do
    open <- symbol "(" <|> symbol "["
    a <- quantity <|> (V . Var <$> symbol "-∞")
    symbol ","
    b <- quantity <|> (V . Var <$> symbol "∞")
    close <- symbol ")" <|> symbol "]"

    case (open, a, b, close) of
      ("[", V (Var "-∞"),  _ ,  _ ) -> fail "Can't have an left-inclusive interval with -∞"
      ("(", V (Var "-∞"),  a , ")") -> return $ LeftImproperInterval a
      ("(", V (Var "-∞"),  a , "]") -> return $ LeftImproperSemiInterval a
      ( _ ,   _ , V (Var "∞"), "]") -> fail "Can't have an left-inclusive interval with -∞"
      ("[",   a , V (Var "∞"), ")") -> return $ RightImproperInterval a
      ("(",   a , V (Var "∞"), ")") -> return $ RightImproperSemiInterval a
      ("[",   a ,  b , "]") -> return $ Segment a b
      ("[",   a ,  b , ")") -> return $ SemiInterval a b
      ("(",   a ,  b , "]") -> return $ SemiSegment a b
      ("(",   a ,  b , ")") -> return $ Interval a b

operatorNumber :: Parser OperatorSign
operatorNumber = lexeme $ OpSign <$> L.hexadecimal

opSign :: Parser LogicalSchema
opSign = char 'L' *> (OpLabel <$> operatorNumber)

stop :: Parser LogicalSchema
stop = symbol "stop" *> return Stop

schemaExpr :: Parser SchemaExpr
schemaExpr = makeExprParser baseTerm schemaOpTable
  where
  baseTerm = Primitive . C <$> integer <|> (symbol "Form" *> (Form <$> quantity)) <|> Primitive . V <$> variable

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

