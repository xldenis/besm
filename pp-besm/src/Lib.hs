{-# LANGUAGE DataKinds #-}
module Lib where

import Data.BitVector.Sized
import Data.Word

import Data.List.NonEmpty

{-
  """
  All information on the problem is always divided into the following four groups:

  1. Information on variable addresses.
  2. Information on parameters.
  3. A list of constants and variable quantities, termed below
     formally information on constants
  4. Information on the logical scheme or, as it will be also
     termed, information on the programme.
  """

  There are additional blocks of information that are introduced:

  - Alpha, Beta, Gamma: Related to coding loops
  - Zero (0): Reserved for 'standard-routines'
-}

data Programme
  = PP
  { variableAddresses :: BlockV
  , parameters :: [Parameter]
  , constants :: [Constant]
  , programme :: [Operator]
  , block0Len :: Word16
  , blockAlphaLen :: Word16
  , blockGammaLen :: Word16
  -- , blockBetLen -- Beta block takes remaining space to addr 02FF
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
  , vars :: NonEmpty VariableAddress
  }

data Dir = FromStart | FromEnd

data VariableAddress
  = Var
    { vaName :: Char
    , param1 :: Word8
    , param2 :: Word8
    , param3 :: Word8
    , offset :: Word8
    , direction :: Dir
    }

data LoopParameter = LP
  { lpName :: Char
  , i0 :: Quantity
  , lpA  :: Quantity
  , lpB  :: Quantity
  , j :: Quantity
  , k :: Quantity
  }

newtype Opcode = OpC { getCode :: BitVector 6 }
newtype Addr = Addr { unAddr :: BitVector 11 }

data Parameter
  = InFin
    { pName :: Char
    , inP :: Quantity
    , finP :: Quantity
    }
  | CharacteristicLoop
    { pName :: Char
    , theta :: Opcode
    , inP :: Quantity
    , loopA :: Quantity
    , loopB :: Quantity
    }
  | LogicalLoop
    { pName :: Char
    , theta :: Opcode
    , loopAddr :: Addr
    , loopBddr :: Addr
    , loopStart :: Addr
    }

data Constant
  = Cell { cName :: Char, val :: (BitVector 39) }
  | Vacant { cName :: Char }
