{-# LANGUAGE DataKinds #-}
module Lib where

import Data.BitVector.Sized
import Data.Word

import Data.List.NonEmpty

data Programme
  = PP
  { variableAddresses :: BlockV
  , parameters :: [Parameter]
  , constants :: [Constant]
  , programme :: [Operator]
  }

data ArithOperator -- Arithmetic, Logical, Non-Standard
  = LParen
  | NLParen Word8
  | Plus
  | Minus
  | RParen
  | NRParen Word8
  | AssignNoNormalize -- ?
  | Print -- F
  | Assign
  | Times
  | Colon
  | Square
  | Cube
  | Cotan
  | Tan
  | Ln
  | SquareRoot
  | TransformToDecimal
  | Exp
  | ArcSin
  | ArcTan
  | Sin
  | Cos
  | E
  | ExtractExponenent
  | Mod
  | ChangeExponent Word8
  | ShiftMantissa Word8
  | Sign

data Operator
  = Arith ArithOperator
  | Parameter Quantity
  | LogicalOperator LogicalOperator
  | OperatorSign OperatorSign -- should this be 8 bits? not 11?? ?? ? ? ? ?
  | LoopOpen Quantity
  | LoopClose

newtype Quantity = QA { unQ :: Char }
newtype OperatorSign = OS { getOperator :: BitVector 11 }

data LogicalOperator = Op
  { x :: Quantity
  , defaultOp :: OperatorSign
  , choices :: [(OperatorSign, RangeType, Quantity, Maybe Quantity)]
  }

data RangeType
  = LeftImproper
  | LeftImproperSemi
  | RightImproper
  | RightImproperSemi
  | Interval
  | SemiInterval
  | SemiSegment
  | Segment

data BlockV
  = V
  { variableAddrs :: [AddressBlock]
  , loopParameters :: [LoopParameter]
  }

newtype Word11 = W { unWord11 :: BitVector 11 }

data AddressBlock = MainHead
  { blockSize :: Word11
  , heads :: NonEmpty BlockHead
  }

data BlockHead
  = Head
  { a :: Word11 -- 10 bits + sign bit
  , b :: Word11
  , c :: Word11
  , vars :: [VariableAddress]
  }

data Dir = FromStart | FromEnd

data VariableAddress
  = Constant { offset :: Word8, direction :: Dir }
  | Var
    { param1 :: Word8
    , param2 :: Word8
    , param3 :: Word8
    , offset :: Word8
    , direction :: Dir
    }

data LoopParameter = LP
  { i0 :: Quantity
  , lpA  :: Quantity
  , lpB  :: Quantity
  , i :: Quantity
  , k :: Quantity
  }

newtype Opcode = OpC { getCode :: BitVector 6 }
newtype Addr = Addr { unAddr :: BitVector 11 }

data Parameter
  = InFin
    { inP :: Quantity
    , finP :: Quantity
    }
  | CharacteristicLoop
    { theta :: Opcode
    , inP :: Quantity
    , loopA :: Quantity
    , loopB :: Quantity
    }
  | LogicalLoop
    { theta :: Opcode
    , loopAddr :: Addr
    , loopBddr :: Addr
    , loopStart :: Addr
    }

data Constant
  = Cell (BitVector 39)
  | Vacant
