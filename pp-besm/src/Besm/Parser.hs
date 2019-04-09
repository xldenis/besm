{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Besm.Parser
( module Besm.Parser
, parse
-- , parseTest'
-- , parseErrorPretty'
-- , parseErrorPretty
, errorBundlePretty
) where

import           Control.Monad

import qualified Data.List.NonEmpty         as NL
import           Data.Semigroup
import           Data.String                (IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void

import           Besm.Syntax
import qualified Besm.Syntax.NonStandard as NS

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr

import           Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import           Data.CharSet.Unicode.Block (basicLatin, greekAndCoptic, cyrillic)
import           Data.Char (isLetter)

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

prefix :: Parser Text -> (a -> a) -> Operator Parser a
prefix op f = Prefix $
  op >> return f

suffix :: Parser Text -> (a -> a) -> Operator Parser a
suffix op f = Postfix $
  op >> return f

semicolon = lexeme (char ';') >> scn

isBesmLetter c = CharSet.member c (CharSet.filter isLetter allLetters)
  where
  allLetters = basicLatin `CharSet.union` greekAndCoptic `CharSet.union` cyrillic

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

variableAddress :: Parser (Text, SimpleExpr Char)
variableAddress = do
  var <- lexeme . angles $ complexVar

  symbol "="
  exp <- simpleExpr (oneOf $ (subscripts var))
  scn
  return (unVar var, exp)

simpleExpr :: Parser Char -> Parser (SimpleExpr Char)
simpleExpr vars = makeExprParser (constant <|> expVar vars) opTable

constant :: Parser (SimpleExpr Char)
constant = lexeme $
  SConstant . negate <$> (char '-' *> integer) <|>
  SConstant <$> integer


expVar :: Parser Char -> Parser (SimpleExpr Char)
expVar vars = lexeme $ SExpVar <$> vars

opTable :: [[Operator Parser (SimpleExpr Char)]]
opTable =
  [ [ binary (symbol ".") STimes]
  , [ binary (symbol "+") SAdd
    ]
  ]

parameterSection :: Parser [Parameter]
parameterSection = section "Parameters" (many parameterDecl)

parameterDecl :: Parser Parameter
parameterDecl = inFin <|> characteristic

-- | A parameter consisting only of upper and lower bounds.
inFin :: Parser Parameter
inFin = try $ do
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
  op <- choice
    [ symbol "<"   >> return Comparison
    , symbol "|<|" >> return ModuliComparison
    , symbol ",<"  >> return WordComparison
    ]
  b <- variable

  scn

  return $ Characteristic var op init a b

-- The constant section contains a list of _every_ constant and undetermined variable not described in Block V
constantSection :: Parser [SimpleExpr Char]
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
schemaParser = (Seq . concatTuples) <$> some ((,) <$> schemaTerm <*> optional (semicolon >> pure Semicolon) <* scn)
  where
  concatTuples ((term, Just s ) : ls) = term : s : concatTuples ls
  concatTuples ((term, Nothing) : ls) = term : concatTuples ls
  concatTuples []                     = []

-- | Parse one schema statement
schemaTerm :: Parser LogicalSchema
schemaTerm = loop <|> printExpr <|> assign <|> logicalOperator <|> opSign <|> stop <|> nonStandard <|> goto

loop :: Parser LogicalSchema
loop = between (char '[') (char ']') $ do
  var <- lexeme letterChar <* scn
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
simpleVar = lexeme $ Var <$> (letterChar) <*> pure []

complexVar :: Parser Variable
complexVar = lexeme $ do
  a <- try $ letterChar <* char '_'
  sub <- some letterChar <|> some digitChar

  return $ Var a sub

printExpr :: Parser LogicalSchema
printExpr = do
   exp <- try $ schemaExpr <* (mapM_ symbol [",", "=>", "0"])

   return $ Print exp

logicalOperator :: Parser LogicalSchema
logicalOperator = lexeme $ (*>) (char 'P') $ parens $ do
  var <- quantity
  symbol ","

  opSign <- operatorNumber
  semicolon

  branches <- some logicalBranch
  return $ LogicalOperator var opSign branches
  where
  logicalBranch = do
    opSign <- operatorNumber
    symbol "/"

    (,) <$> pure opSign <*> range

  range = do
    open <- symbol "(" <|> symbol "["
    a <- Right <$> quantity <|> (Left <$> symbol "-∞")
    symbol ","
    b <- Right <$> quantity <|> (Left <$> symbol "∞")
    close <- symbol ")" <|> symbol "]"

    case (open, a, b, close) of
      ("[", Left "-∞",       _ ,  _ ) -> fail "Can't have an left-inclusive interval with -∞"
      ("(", Left "-∞", Right a , ")") -> return $ LeftImproperInterval a
      ("(", Left "-∞", Right a , "]") -> return $ LeftImproperSemiInterval a
      ( _ ,       _  , Left "∞", "]") -> fail "Can't have an left-inclusive interval with -∞"
      ("[", Right a  , Left "∞", ")") -> return $ RightImproperInterval a
      ("(", Right a  , Left "∞", ")") -> return $ RightImproperSemiInterval a
      ("[", Right a  , Right b , "]") -> return $ Segment a b
      ("[", Right a  , Right b , ")") -> return $ SemiInterval a b
      ("(", Right a  , Right b , "]") -> return $ SemiSegment a b
      ("(", Right a  , Right b , ")") -> return $ Interval a b

operatorNumber :: Parser OperatorSign
operatorNumber = lexeme $ OpSign <$> L.hexadecimal

opSign :: Parser LogicalSchema
opSign = char 'L' *> (OpLabel <$> operatorNumber)

stop :: Parser LogicalSchema
stop = symbol "stop" *> return (NS NS.Stop (Abs 0) (Abs 0) (Abs 0))

goto :: Parser LogicalSchema
goto = cccc2 <|> cccc <|> rtc <|> clcc <|> jcc
  where
  cccc  = char '/'    *> (CCCC <$> operatorNumber)
  cccc2 = try $ do
    a <- operatorNumber
    string "/="
    b <- operatorNumber
    return $ CCCC2 a b
  rtc = try $ (RTC <$> operatorNumber) <* string "\\="
  clcc = string "/-"  *> (CLCC <$> operatorNumber)
  jcc = string "\\-"  *> (pure JCC)

nonStandard :: Parser LogicalSchema
nonStandard =
  ma <|> mb <|> pn <|> clcc <|> jcc <|> comp <|>
  compword <|> compmod <|> cccc <|> ccccsnd <|> i <|>
  logmult <|> ai <|> aicarry <|> shift <|>
  shiftall <|>

  add  NS.Normalized <|> sub  NS.Normalized <|> mult  NS.Normalized <|>
  div  NS.Normalized <|> adde NS.Normalized <|> sube  NS.Normalized <|>
  ce   NS.Normalized <|> xa   NS.Normalized <|> xb    NS.Normalized <|>
  diva NS.Normalized <|> divb NS.Normalized <|> tn    NS.Normalized <|>
  tmin NS.Normalized <|> tmod NS.Normalized <|> tsign NS.Normalized <|>
  texp NS.Normalized <|>

  add  NS.UnNormalized <|> sub  NS.UnNormalized <|> mult  NS.UnNormalized <|>
  div  NS.UnNormalized <|> adde NS.UnNormalized <|> sube  NS.UnNormalized <|>
  ce   NS.UnNormalized <|> xa   NS.UnNormalized <|> xb    NS.UnNormalized <|>
  diva NS.UnNormalized <|> divb NS.UnNormalized <|> tn    NS.UnNormalized <|>
  tmin NS.UnNormalized <|> tmod NS.UnNormalized <|> tsign NS.UnNormalized <|>
  texp NS.UnNormalized
 where
  nonStandard str sym = do
    try $ do
      lexeme (char '(')
      symbol str <* symbol ","

    a1 <- address <* symbol ","
    a2 <- address <* symbol ","
    a3 <- address
    lexeme (char ')')
    return $ NS sym a1 a2 a3

  address = Abs <$> integer <|> VarQ <$> variable

  ma = nonStandard "Ma" NS.Ma

  mb = nonStandard "Mb" NS.Mb

  add norm = nonStandard "Add" (NS.Add norm)

  sub norm = nonStandard "Sub" (NS.Sub norm)

  mult norm = nonStandard "Mult" (NS.Mult norm)

  div norm = nonStandard "Div" (NS.Div norm)

  adde norm = nonStandard "AddE" (NS.AddE norm)

  sube norm = nonStandard "SubE" (NS.SubE norm)

  ce norm = nonStandard "Ce" (NS.Ce norm)

  xa norm = nonStandard "Xa" (NS.Xa norm)

  xb norm = nonStandard "Xb" (NS.Xb norm)

  diva norm = nonStandard "DivA" (NS.DivA norm)

  divb norm = nonStandard "DivB" (NS.DivB norm)

  tn norm = nonStandard "TN" (NS.TN norm)

  pn = nonStandard "PN" NS.PN

  tmin norm = nonStandard "TMin" (NS.TMin norm)

  tmod norm = nonStandard "TMod" (NS.TMod norm)

  tsign norm = nonStandard "TSign" (NS.TSign norm)

  texp norm = nonStandard "TExp" (NS.TExp norm)

  shift = nonStandard "Shift" NS.Shift

  shiftall = nonStandard "ShiftAll" NS.ShiftAll

  ai = nonStandard "AI" NS.AI

  aicarry = nonStandard "AICarry" NS.AICarry

  i = nonStandard "I" NS.I

  logmult = nonStandard "LogMult" NS.LogMult

  clcc = nonStandard "CLCC" NS.CLCC

  jcc = nonStandard "JCC" NS.JCC

  comp = nonStandard "Comp" NS.Comp

  compword = nonStandard "CompWord" NS.CompWord

  compmod = nonStandard "CompMod" NS.CompMod

  cccc = nonStandard "CCCC" NS.CCCC

  ccccsnd = nonStandard "CCCCSnd" NS.CCCCSnd



schemaExpr :: Parser SchemaExpr
schemaExpr = makeExprParser baseTerm schemaOpTable
  where
  baseTerm = Primitive . C <$> integer <|> (symbol "Form" *> (Form <$> quantity)) <|> Primitive . V <$> variable

schemaOpTable :: [[Operator Parser SchemaExpr]]
schemaOpTable =
  [ [ prefix (symbol "mod") Mod
    ]
  , [ prefix (symbol "sqrt") Sqrt
    , suffix (symbol "^3") Cube
    ]
  , [ binary (symbol "*") Times
    , binary (symbol ":") Div
    ]
  , [ binary (symbol "+") Add
    , binary (symbol "-") Minus
    ]
  ]

list :: Parser a -> Parser [a]
list a = a `sepBy` symbol ","

