{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Besm.Syntax where

import           Data.List                 (intersperse)
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NL
import           Data.String               (IsString)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc
import           Text.Printf
import qualified Besm.Syntax.NonStandard as NS
{-
  Source level syntax tree for the PP-BESM language suitable for pretty printing.
-}

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
  | Charateristic Variable ComparisonOp Int Variable Variable
  deriving Show

data ComparisonOp
  = Comparison
  | WordComparison
  | ModuliComparison
  deriving Show

data Range
  = LeftImproperInterval      Quantity
  | LeftImproperSemiInterval  Quantity
  | RightImproperInterval     Quantity
  | RightImproperSemiInterval Quantity
  | Interval      Quantity Quantity
  | SemiInterval  Quantity Quantity
  | SemiSegment   Quantity Quantity
  | Segment       Quantity Quantity
  deriving Show

-- TODO: Add parentheses
data LogicalSchema
  = Loop Char LogicalSchema
  | Seq [LogicalSchema]
  | Assign SchemaExpr Variable
  | LogicalOperator Quantity OperatorSign [(OperatorSign, Range)]
  | OpLabel OperatorSign
  | Print SchemaExpr
  | Semicolon
  | NS NS.NonStandardOpcode Address Address Address
  | CCCC OperatorSign
  | CCCC2 OperatorSign OperatorSign
  | RTC OperatorSign
  | CLCC OperatorSign
  | JCC
  deriving Show

newtype OperatorSign = OpSign { fromOperatorSign :: Int }
  deriving (Show)

{-
  The type of addresses for non standard operators. They are either absolute addresses
  or variable quantities.
-}
data Address
  = Abs Int
  | VarQ Variable
  deriving (Show, Eq)

data SchemaExpr
  = Times SchemaExpr SchemaExpr
  | Div   SchemaExpr SchemaExpr
  | Add   SchemaExpr SchemaExpr
  | Minus SchemaExpr SchemaExpr
  | Primitive Quantity
  | Form   Quantity
  | Mod SchemaExpr
  | Sqrt SchemaExpr
  | Cube SchemaExpr
  deriving Show

data Quantity = V Variable | C Int
  deriving (Show, Eq)

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
  [ prettyQuantity var
  , prettyOpSign op <> pretty ";"
  ] <+> concatWith (\a b -> a <+> pretty "," <+> b) (map prettyRange ranges))
  where
  prettyRange (op, range) = prettyOpSign op <+> pretty "/" <+> go range
  go (LeftImproperInterval      up) = parens $ pretty "-∞" <+> pretty "," <+>prettyQuantity up
  go (LeftImproperSemiInterval  up) = pretty "(" <> pretty "-∞" <+> pretty "," <+>prettyQuantity up <> pretty "]"
  go (RightImproperInterval     low) = parens $ prettyQuantity low <+> pretty "," <+> pretty "∞"
  go (RightImproperSemiInterval low) = pretty "[" <> prettyQuantity low <+> pretty "," <+> pretty "∞" <> pretty ")"
  go (Interval      low up) = parens $ prettyQuantity low <+> pretty "," <+>prettyQuantity up
  go (SemiInterval  low up) = pretty "[" <> prettyQuantity low <+> pretty "," <+>prettyQuantity up <> pretty ")"
  go (SemiSegment   low up) = pretty "(" <> prettyQuantity low <+> pretty "," <+>prettyQuantity up <> pretty "]"
  go (Segment       low up) = brackets $  prettyQuantity low <+> pretty "," <+>prettyQuantity up

prettySchema (OpLabel op) = pretty "L" <> prettyOpSign op
prettySchema (Print exp)  = prettyExp exp <+> pretty ", => 0"
prettySchema Semicolon = pretty ";"

prettyQuantity (V v) = pretty (unVar v)
prettyQuantity (C c) = pretty c

prettyExp :: SchemaExpr -> Doc a
prettyExp (Times l r)  = prettyExp l <+> pretty "*" <+> prettyExp r
prettyExp (Div   l r)  = prettyExp l <+> pretty ":" <+> prettyExp r
prettyExp (Add   l r)  = prettyExp l <+> pretty "+" <+> prettyExp r
prettyExp (Minus l r)  = prettyExp l <+> pretty "-" <+> prettyExp r
prettyExp (Primitive var) = prettyQuantity var
prettyExp (Form   var) = pretty "Form" <+> prettyQuantity var

prettyOpSign = pretty . toHex . fromOperatorSign
  where
  toHex :: Int -> String
  toHex i = printf "%04x" i
