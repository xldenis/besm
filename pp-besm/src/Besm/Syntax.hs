{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Besm.Syntax where

import qualified Besm.Syntax.NonStandard as NS
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NL
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Text.Printf

{-
  Source level syntax tree for the PP-BESM language suitable for pretty printing.
-}

data ParsedProgramme = P
  { vaSection :: VASection
  , pSection :: [Parameter]
  , cSection :: [SimpleExpr Char]
  , kSection :: LogicalSchema
  }
  deriving (Show, Eq)

newtype VASection = VA [Block]
  deriving (Show, Eq)

data Block = Block Int (NonEmpty (Text, SimpleExpr Char))
  deriving (Show, Eq)

data SimpleExpr a
  = STimes (SimpleExpr a) (SimpleExpr a)
  | SAdd (SimpleExpr a) (SimpleExpr a)
  | SConstant Int
  | SExpVar a
  deriving (Show, Eq, Functor, Foldable)

data Parameter
  = InFin Variable Int Int
  | Characteristic Variable ComparisonOp Int Variable Variable
  deriving (Show, Eq)

paramVar :: Parameter -> Variable
paramVar (InFin p _ _) = p
paramVar (Characteristic p _ _ _ _) = p

data ComparisonOp
  = Comparison
  | WordComparison
  | ModuliComparison
  deriving (Show, Eq)

data Range
  = LeftImproperInterval Quantity -- (-∞, a)
  | LeftImproperSemiInterval Quantity -- (-∞, a]
  | RightImproperInterval Quantity -- (a, ∞)
  | RightImproperSemiInterval Quantity -- [a, ∞)
  | Interval Quantity Quantity -- (a, b)
  | SemiInterval Quantity Quantity -- (a, b]
  | SemiSegment Quantity Quantity -- [a, b)
  | Segment Quantity Quantity -- [a, b]
  deriving (Show, Eq)

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
  deriving (Show, Eq)

newtype OperatorSign = OpSign {fromOperatorSign :: Int}
  deriving (Show, Eq)

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
  | Div SchemaExpr SchemaExpr
  | Add SchemaExpr SchemaExpr
  | Minus SchemaExpr SchemaExpr
  | Primitive Quantity
  | Form Quantity
  | Mod SchemaExpr
  | Sqrt SchemaExpr
  | Cube SchemaExpr
  deriving (Show, Eq)

data Quantity = V Variable | C Int
  deriving (Show, Eq)

data Variable = Var {var :: Char, subscripts :: [Char]}
  deriving (Show, Eq)

unVar :: Variable -> Text
unVar (Var char []) = T.singleton char
unVar (Var char subs) = T.pack $ char : '_' : subs

prettyProgramme :: ParsedProgramme -> Doc a
prettyProgramme (P va p c k) =
  vcat
    [ prettyVASection va
    , mempty
    , prettyParameters p
    , mempty
    , prettyConstants c
    , mempty
    , prettySchemaSection k
    ]

prettyVASection :: VASection -> Doc a
prettyVASection (VA blocks) =
  vcat $
    pretty "1."
      <+> pretty "Variable Addresses"
      : mempty
      : intersperse mempty (zipWith (prettyVABlock) ['a' ..] blocks)

prettyVABlock :: Char -> Block -> Doc a
prettyVABlock c (Block ix vars) =
  nest 2 . vcat $
    parens (pretty c)
      <+> pretty "Block"
      <+> pretty c
      <+> parens (pretty ix <+> pretty "cells")
      : mempty
      : map (\(var, expr) -> angles (pretty var) <+> pretty "=" <+> prettySimpleExpr expr) (NL.toList vars)

prettyParameters :: [Parameter] -> Doc a
prettyParameters params =
  vcat $
    pretty "2."
      <+> pretty "Parameters"
      : mempty
      : map prettyParameter params

prettyParameter :: Parameter -> Doc a
prettyParameter (InFin var init fin) = pretty (unVar var) <+> pretty ":" <+> bound (unVar var) "in" init <> pretty "," <+> bound (unVar var) "fin" fin
prettyParameter (Characteristic var op init a b) = pretty (unVar var) <+> pretty ":" <+> bound (unVar var) "in" init <> pretty "," <+> pretty (unVar a) <+> prettyComp op <+> pretty (unVar b)
 where
  prettyComp Comparison = pretty "<"
  prettyComp WordComparison = pretty ",<"
  prettyComp ModuliComparison = pretty "|<|"

bound v sub val = pretty v <> pretty "_" <> pretty sub <+> pretty "=" <+> pretty val

prettyConstants :: Pretty b => [SimpleExpr b] -> Doc a
prettyConstants cs =
  vcat $
    pretty "3."
      <+> pretty "List of Constants and Variable Quantities"
      : mempty
      : [concatWith (\a b -> a <> pretty "," <+> b) (map prettyConstant cs)]

prettyConstant :: Pretty b => SimpleExpr b -> Doc a
prettyConstant = prettySimpleExpr

prettySimpleExpr :: Pretty b => SimpleExpr b -> Doc a
prettySimpleExpr (STimes l r) = prettySimpleExpr l <> pretty "." <> prettySimpleExpr r
prettySimpleExpr (SAdd l r) = prettySimpleExpr l <+> pretty "+" <+> prettySimpleExpr r
prettySimpleExpr (SConstant c) = pretty c
prettySimpleExpr (SExpVar var) = pretty var

prettySchemaSection :: LogicalSchema -> Doc a
prettySchemaSection ls =
  vcat $
    pretty "4."
      <+> pretty "Logical Scheme"
      : mempty
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
prettySchema (LogicalOperator var op ranges) =
  pretty "P"
    <> ( parens $
          hsep
            [ prettyQuantity var
            , prettyOpSign op <> pretty ";"
            ]
            <+> concatWith (\a b -> a <+> pretty "," <+> b) (map prettyRange ranges)
       )
 where
  prettyRange (op, range) = prettyOpSign op <+> pretty "/" <+> go range
  go (LeftImproperInterval up) = parens $ pretty "-∞" <+> pretty "," <+> prettyQuantity up
  go (LeftImproperSemiInterval up) = pretty "(" <> pretty "-∞" <+> pretty "," <+> prettyQuantity up <> pretty "]"
  go (RightImproperInterval low) = parens $ prettyQuantity low <+> pretty "," <+> pretty "∞"
  go (RightImproperSemiInterval low) = pretty "[" <> prettyQuantity low <+> pretty "," <+> pretty "∞" <> pretty ")"
  go (Interval low up) = parens $ prettyQuantity low <+> pretty "," <+> prettyQuantity up
  go (SemiInterval low up) = pretty "[" <> prettyQuantity low <+> pretty "," <+> prettyQuantity up <> pretty ")"
  go (SemiSegment low up) = pretty "(" <> prettyQuantity low <+> pretty "," <+> prettyQuantity up <> pretty "]"
  go (Segment low up) = brackets $ prettyQuantity low <+> pretty "," <+> prettyQuantity up
prettySchema (OpLabel op) = pretty "L" <> prettyOpSign op
prettySchema (Print exp) = prettyExp exp <+> pretty ", => 0"
prettySchema Semicolon = pretty ";"
prettySchema JCC = pretty "\\-"
prettySchema (NS op a b c) = prettyNonstandard op a b c
prettySchema e = error (show e)

prettyQuantity (V v) = pretty (unVar v)
prettyQuantity (C c) = pretty c

prettyAddress :: Address -> Doc a
prettyAddress (Abs i) = pretty i
prettyAddress (VarQ v) = pretty $ unVar v

tupled' ls = pretty '(' <> hcat (intersperse (pretty ",") ls) <> pretty ')'
prettyNonstandard :: NS.NonStandardOpcode -> Address -> Address -> Address -> Doc a
prettyNonstandard (NS.Add norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Sub norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Mult norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Div norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.AddE norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.SubE norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Ce norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Xa norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Xb norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.DivA norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.DivB norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.TN norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.PN) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.TMin norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.TMod norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.TSign norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.TExp norm) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Shift) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.ShiftAll) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.AI) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.AICarry) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.I) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Ma) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Mb) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.LogMult) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.CLCC) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.JCC) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Comp) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.CompWord) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.CompMod) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.CCCC) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.CCCCSnd) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.Stop) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]
prettyNonstandard (NS.SwitchStop) a b c = tupled' [pretty "Add", prettyAddress a, prettyAddress b, prettyAddress c]

prettyExp :: SchemaExpr -> Doc a
prettyExp (Times l r) = prettyExp l <+> pretty "*" <+> prettyExp r
prettyExp (Div l r) = prettyExp l <+> pretty ":" <+> prettyExp r
prettyExp (Add l r) = prettyExp l <+> pretty "+" <+> prettyExp r
prettyExp (Minus l r) = prettyExp l <+> pretty "-" <+> prettyExp r
prettyExp (Primitive var) = prettyQuantity var
prettyExp (Form var) = pretty "Form" <+> prettyQuantity var

prettyOpSign = pretty . toHex . fromOperatorSign
 where
  toHex :: Int -> String
  toHex i = printf "%04x" i
