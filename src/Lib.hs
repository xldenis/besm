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

data Operator -- Arithmetic, Logical, Non-Standard
  = LParen
  | NLParen
  | Plus
  | Minus
  | RParen
  | NRParen
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
  | ChangeExponent -- n x
  | ShiftMantissa -- n x
  | Sign
  | Parameter QuantityAddr
  | LogicalOperator LogicalOperator
  | OperatorSign OperatorSign -- should this be 8 bits? not 11?? ?? ? ? ? ?
  | LoopOpen QuantityAddr
  | LoopClose

newtype QuantityAddr = QA { unAddr :: Word8 }
newtype OperatorSign = OS { getOperator :: BitVector 11 }

data LogicalOperator = Op
  { x :: QuantityAddr
  , defaultOp :: OperatorSign
  , choices :: [(OperatorSign, RangeType, QuantityAddr, Maybe QuantityAddr)]
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
  { i0 :: QuantityAddr
  , lpA  :: QuantityAddr
  , lpB  :: QuantityAddr
  , i :: QuantityAddr
  , k :: QuantityAddr
  }

newtype Opcode = OpC { getCode :: BitVector 6 }
newtype Addr = { unAddr :: BitVector 11 }

data Parameter
  = InFin
    { inP :: QuantityAddr
    , finP :: QuantityAddr
    }
  | CharacteristicLoop
    { theta :: Opcode
    , inP :: QuantityAddr
    , loopA :: QuantityAddr
    , loopB :: QuantityAddr
    }
  | LogicalLoop
    { theta :: Opcode
    , loopA :: Addr
    , loopB :: Addr
    , loopStart :: Addr
    }

data Constant
  = Cell (BitVector 39)
  | Vacant
