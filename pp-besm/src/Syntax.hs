{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax where

import Data.Text (Text)
import Data.String (IsString)
import Data.Text.Prettyprint.Doc
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NL

data ParsedProgramme
  = P
  { vaSection :: VASection
  , pSection  :: [Parameter]
  , cSection  :: [SimpleExpr]
  , kSection  :: LogicalSchema
  } deriving Show

newtype VASection = VA [Block]
  deriving Show

data Block = Block Int (NonEmpty (Text, SimpleExpr))
  deriving Show

data SimpleExpr
  = STimes SimpleExpr SimpleExpr
  | SAdd   SimpleExpr SimpleExpr
  | SConstant Int
  | SExpVar Char
  deriving Show

data Parameter
  = InFin Variable Int Int
  | Charateristic ComparisonOp Int Variable Variable
  deriving Show

data ComparisonOp
  = Comparison
  | WordComparison
  | ModuliComparison
  deriving Show

data Range
  = LeftImproperInterval      Variable
  | LeftImproperSemiInterval  Variable
  | RightImproperInterval     Variable
  | RightImproperSemiInterval Variable
  | Interval      Variable Variable
  | SemiInterval  Variable Variable
  | SemiSegment   Variable Variable
  | Segment       Variable Variable
  deriving Show

data LogicalSchema
  = Loop Char LogicalSchema
  | Seq [LogicalSchema]
  | Assign SchemaExpr Variable
  | LogicalOperator Variable Text [(Text, Range)]
  | OpLabel Text
  | Print SchemaExpr
  | Stop
  | Semicolon
  deriving Show

data SchemaExpr
  = Times SchemaExpr SchemaExpr
  | Div   SchemaExpr SchemaExpr
  | Add   SchemaExpr SchemaExpr
  | Minus SchemaExpr SchemaExpr
  | Constant Int
  | ExpVar Variable
  | Form   Variable
  deriving Show

newtype Variable = Var { unVar :: Text }
  deriving (Show, Eq, IsString)

prettyProgramme :: ParsedProgramme -> Doc a
prettyProgramme (P va p c k) = vcat
  [ prettyVASection va
  , mempty
  , prettyParameters p
  , mempty
  , prettyConstants c
  , mempty
  , prettySchemaSection k
  ]

prettyVASection :: VASection -> Doc a
prettyVASection (VA blocks) = vcat $ pretty "1." <+> pretty "Variable Addresses" : mempty
  : intersperse mempty (zipWith (prettyVABlock) ['a'..] blocks)

prettyVABlock :: Char -> Block -> Doc a
prettyVABlock c (Block ix vars) = nest 2 . vcat $
  parens (pretty c) <+> pretty "Block" <+> pretty c <+> parens (pretty ix <+> pretty "cells") : mempty
  : map (\(var, expr) -> angles (pretty var) <+> pretty "=" <+> prettySimpleExpr expr) (NL.toList vars)

prettyParameters :: [Parameter] -> Doc a
prettyParameters params = vcat $ pretty "2." <+> pretty "Parameters" : mempty
  : map prettyParameter params

prettyParameter :: Parameter -> Doc a
prettyParameter (InFin var init fin) = pretty (unVar var) <+> pretty ":" <+> bound (unVar var) "in" init  <> pretty "," <+> bound (unVar var) "fin" fin
  where
  bound v sub val = pretty v <> pretty "_" <> pretty sub <+> pretty "=" <+> pretty val

prettyConstants :: [SimpleExpr] -> Doc a
prettyConstants cs = vcat $ pretty "3." <+> pretty "Constants" : mempty
 : [concatWith (\a b -> a <> pretty "," <+> b) (map prettyConstant cs)]

prettyConstant :: SimpleExpr -> Doc a
prettyConstant = prettySimpleExpr

prettySimpleExpr :: SimpleExpr -> Doc a
prettySimpleExpr (STimes l r)  = prettySimpleExpr l <+> pretty "*" <+> prettySimpleExpr r
prettySimpleExpr (SAdd l r)    = prettySimpleExpr l <+> pretty "+" <+> prettySimpleExpr r
prettySimpleExpr (SConstant c) = pretty c
prettySimpleExpr (SExpVar var) = pretty var

prettySchemaSection :: LogicalSchema -> Doc a
prettySchemaSection ls = vcat $ pretty "4." <+> pretty "Logical Schema" : mempty
  : [prettySchema ls]

prettySchema :: LogicalSchema -> Doc a
prettySchema (Loop ix ls) = brackets $ pretty ix <+> prettySchema ls
prettySchema (Seq [ls]) = prettySchema ls
prettySchema (Seq lss) = prettySchemaEl lss
  where
  prettySchemaEl (s : Semicolon : ls) = prettySchema s <> pretty ";" <+> prettySchemaEl ls
  prettySchemaEl (s : []) = prettySchema s
  prettySchemaEl (s : ls) = prettySchema s <+> prettySchemaEl ls
  prettySchemaEl [] = mempty
prettySchema (Assign exp var) = prettyExp exp <+> pretty "=>" <+> pretty (unVar var)
prettySchema (LogicalOperator var op ranges) = pretty "P" <> (parens $ hsep
  [ pretty (unVar var)
  , pretty op <> pretty ";"
  ] <+> concatWith (\a b -> a <+> pretty "," <+> b) (map prettyRange ranges))
  where
  prettyRange (op, range) = pretty op <+> pretty "/" <+> pretty "()"
prettySchema (OpLabel op) = pretty "L" <> pretty op
prettySchema (Print exp)  = prettyExp exp <+> pretty ", => 0"
prettySchema Semicolon = pretty ";"
prettySchema Stop = pretty "Stop"

prettyExp :: SchemaExpr -> Doc a
prettyExp (Times l r) = prettyExp l <+> pretty "*" <+> prettyExp r
prettyExp (Div   l r) = prettyExp l <+> pretty ":" <+> prettyExp r
prettyExp (Add   l r) = prettyExp l <+> pretty "+" <+> prettyExp r
prettyExp (Minus l r) = prettyExp l <+> pretty "-" <+> prettyExp r
prettyExp (Constant c) = pretty c
prettyExp (ExpVar var) = pretty (unVar var)
prettyExp (Form   var) = pretty "Form" <+> pretty (unVar var)
