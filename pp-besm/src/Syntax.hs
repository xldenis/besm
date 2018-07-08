{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax where

import Data.Text
import Data.String (IsString)

data ParsedProgramme
  = P
  { vaSection :: VASection
  , pSection  :: [Parameter]
  , cSection  :: [SimpleExpr]
  , kSection  :: LogicalSchema
  } deriving Show

newtype VASection = VA [Block]
  deriving Show

data Block = Block Int [(Text, SimpleExpr)]
  deriving Show

data SimpleExpr
  = STimes SimpleExpr SimpleExpr
  | SAdd SimpleExpr SimpleExpr
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
