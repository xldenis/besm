{-# LANGUAGE DataKinds #-}
module Put where

import Lib

import Data.Word
import Data.Serialize.Put
import Data.BitVector.Sized.BitLayout
import Data.BitVector.Sized

import Data.Maybe (fromJust)
import Data.Function
import GHC.TypeNats (KnownNat)

blockV = undefined

blockP = undefined

blockC = undefined

blockK codes = undefined

{-
  The BESM uses 39-bit words which can be of two categories: floating point numbers or instructions.

  The following functions define the bitlayout of both kinds of cells (words).
-}

emptyCell :: BitLayout 39 0
emptyCell = empty

{-

  The bit layout for an instruction is as follows:

  +----------------------------------------------------------+
  | 6 | 5 | .. | 1 | 11 | .. | 1 | 11 | .. | 1 | 11 | .. | 1 |
  +----------------------------------------------------------+
    ^                ^              ^              ^
    |                |              |              |
    +- opcode        +--------------+--------------+
                                    |
                                    +- 11-bit addresses for arguments

-}

opcodeLayout :: BitLayout 39 6
opcodeLayout = (chunk 33 :: Chunk 6) <: emptyCell

instAddr1 :: BitLayout 39 11
instAddr1 = (chunk 22 :: Chunk 11) <: emptyCell

instAddr2 :: BitLayout 39 11
instAddr2 = (chunk 11 :: Chunk 11) <: emptyCell

instAddr3 :: BitLayout 39 11
instAddr3 = (chunk 11 :: Chunk 11) <: emptyCell

buildInstruction :: BitVector 6 -> BitVector 11 -> BitVector 11 -> BitVector 11 -> BitVector 39
buildInstruction op addr1 addr2 addr3 = (bitVector 0)
  & \cell -> inject opcodeLayout cell op
  & \cell -> inject instAddr1    cell addr1
  & \cell -> inject instAddr2    cell addr2
  & \cell -> inject instAddr3    cell addr3

{-
  Layout of numbers:

  +----------------------------------------+
  | 6 | 5 | .. | 1 | 33 | 32 | 31 | .. | 1 |
  +----------------------------------------+
    ^   ^            ^           ^
    |   |            |           |
    |   |            |           +---- 32-bit mantissa
    |   |            +---------------- 1-bit sign of number
    |   +----------------------------- 5-bit exponent
    +--------------------------------- 1-bit sign of exponenet
-}


exponentL :: BitLayout 39 6
exponentL = (chunk 33 :: Chunk 6) <: emptyCell

numberSign :: BitLayout 39 1
numberSign = (chunk 32 :: Chunk 1) <: emptyCell

mantissa :: BitLayout 39 32
mantissa = (chunk 0 :: Chunk 32) <: emptyCell

buildNumber :: BitVector 6 -> BitVector 1 -> BitVector 32 -> BitVector 39
buildNumber exp sign mant = bitVector 0
  & \cell -> inject exponentL   cell exp
  & \cell -> inject numberSign  cell sign
  & \cell -> inject mantissa    cell mant

operatorSign :: BitVector 6
operatorSign = bitVector 0x018

b0 :: KnownNat k => BitVector k
b0 = bitVector 0

{-
  """
  Coding an arithmetical operator consists in substituting its code
  for each symbol in the formula. The forumla-symbol codes are
  written consecutively in cells of block K, four codes to one cell,
  the contents of which are considered as a number. The exponent and
  sign of the line of information on an arithmetical operator remain
  vacant so that this line has the form of a positive number with
  zero exponent.
  """
  p 28.

  However, certain arithmetical operators are written as 16-bit numbers
-}

packArithCodes :: Word8 -> Word8 -> Word8 -> Word8 -> BitVector 32
packArithCodes a b c d = bitVector 0
  & \cell -> inject ((chunk 24 :: Chunk 8) <: empty) cell (bitVector $ fromIntegral a)
  & \cell -> inject ((chunk 16 :: Chunk 8) <: empty) cell (bitVector $ fromIntegral b)
  & \cell -> inject ((chunk 8  :: Chunk 8) <: empty) cell (bitVector $ fromIntegral c)
  & \cell -> inject ((chunk 0  :: Chunk 8) <: empty) cell (bitVector $ fromIntegral d)

buildArithCodes :: BitVector 32 -> BitVector 39
buildArithCodes codes = inject mantissa (bitVector 0) codes

type ParameterAddresses = [(Char, Word8)]

parameterToBits pa p = bitVector . fromIntegral . fromJust $ unQ p `lookup` pa

bitVector' :: (KnownNat k, Integral a) => a -> BitVector k
bitVector' = bitVector . fromIntegral

type BV = BitVector

{-
  First we transform the input token stream into the raw codes for each token. Once that stream has been produced,
  We pass over it again to pack arithmetical operators into as few cells as possible.
-}

data IntermediateValue = Short Word8 | Long Word16 | Full (BitVector 39)

packCells :: [IntermediateValue] -> [BitVector 39]
packCells (Short a : Short b : Short c : Short d : others) = buildArithCodes (packArithCodes a b c d) : packCells others
packCells (Short a : Short b : Short c : others) = buildArithCodes (packArithCodes a b c 0) : packCells others
packCells (Short a : Short b : Long cd : others) = buildArithCodes ((bitVector' a  :: BV 8) <:> (bitVector' b  :: BV  8) <:> (bitVector' cd :: BV 16)) : packCells others
packCells (Short a : Long bc : Short d : others) = buildArithCodes ((bitVector' a  :: BV 8) <:> (bitVector' bc :: BV 16) <:> (bitVector' d  :: BV  8)) : packCells others
packCells (Short a : Short b           : others) = buildArithCodes (packArithCodes a b 0 0) : packCells others
packCells (Short a                     : others) = buildArithCodes (packArithCodes a 0 0 0) : packCells others
packCells (Long ab : Short c : Short d : others) = buildArithCodes ((bitVector' ab :: BV 16) <:> (bitVector' c  :: BV  8) <:> (bitVector' d  :: BV  8)) : packCells others
packCells (Long ab           : Long cd : others) = buildArithCodes ((bitVector' ab :: BV 16) <:> (bitVector' cd :: BV 16)) : packCells others
packCells (Full o                      :  thers) = o : packCells thers
packCells [] = []

encodeCode :: ParameterAddresses -> [Operator] -> [IntermediateValue]
encodeCode pa (Arith op : ops)        = arithOpcode op : encodeCode pa ops
encodeCode pa (Parameter p : ops)     = Short (fromJust $ (unQ p) `lookup` pa) : encodeCode pa ops
encodeCode pa (OperatorSign os : LogicalOperator lo : o : ops) = encodeLogicalOp pa (Just os) lo o
encodeCode pa (LogicalOperator lo : o : ops)                   = encodeLogicalOp pa  Nothing  lo o
encodeCode pa (OperatorSign os : ops) = Full (buildInstruction operatorSign (getOperator os)       b0 b0) : encodeCode pa ops
encodeCode pa (LoopOpen p : ops)      = Full (buildInstruction operatorSign (parameterToBits pa p) b0 b0) : encodeCode pa ops
encodeCode pa (LoopClose : ops)       = Full (buildInstruction (bitVector 0x01F) b13FF b13FF b13FF)       : encodeCode pa ops
  where b13FF = bitVector 0x13FF


{-
  """
  If the sign of an operator number stands in front of a logical
  operator, this number expecially need not be coded, and its magni-
  tude is placed in the first address of the first line of information.
  """

  This means that given the expression:

  L0102P(x, N, ....)

  Instead of coding it as:

  | 018 | 0102 |      |      |
  |-----|------|------|------|
  | 018 |      | <x>  |  N   |
  |-----|------|------|------|
  | ... | .... | .... | .... |

  We code it as

  | 018 | 0102 | <x>  |  N   |
  |-----|------|------|------|
  | ... | .... | .... | .... |


  """
  When distributing information on a logical operator in the
  block K it is necessary to arrange afterwards that in the ifrst
  line of information following (after) the last line of information
  on the logical operator the third address be vacant. Usually this
  is true. If it is not the case then after the (k + 1)st cell of
  information on the logical operator it is necessary to leave a blank
  cell.
  """
-}

encodeLogicalOp :: ParameterAddresses -> Maybe OperatorSign -> LogicalOperator -> Operator -> [IntermediateValue]
encodeLogicalOp pa opSign (Op x defaultOp branches) following = let
  followingOperator = encodeCode pa [following]
  thirdAddress      = extract instAddr3 (head $ packCells followingOperator)
  padding           = if thirdAddress == b0 then [] else [Full b0]
  operatorHead = Full $ buildInstruction operatorSign (maybe b0 getOperator opSign) (parameterToBits pa x) (getOperator defaultOp)

  in operatorHead : map encodeBranch branches ++ padding ++ followingOperator
  where

  {-
    """
    If a segment has only one boundary point, the code of its
    quantity is palced in the first address of the line of information
    while the second address remains vacant.
    """
  -}
  encodeBranch (op, rangeType, lowerBound, upperBound) =
    let secondBound = maybe b0 (parameterToBits pa) upperBound
    in Full $ buildInstruction (rangeCode rangeType) (parameterToBits pa lowerBound) (secondBound) (getOperator op)



rangeCode :: RangeType -> BitVector 6
rangeCode (LeftImproper)      = bitVector 1
rangeCode (LeftImproperSemi)  = bitVector 2
rangeCode (RightImproper)     = bitVector 3
rangeCode (RightImproperSemi) = bitVector 4
rangeCode (Interval)          = bitVector 8
rangeCode (SemiInterval)      = bitVector 13
rangeCode (SemiSegment)       = bitVector 18
rangeCode (Segment)           = bitVector 19

arithOpcode :: ArithOperator -> IntermediateValue
arithOpcode (NLParen n)           = Long (0x0200 + fromIntegral n)
arithOpcode (NRParen n)           = Long (0x0600 + fromIntegral n)
arithOpcode (ChangeExponent n)    = Long (0xFD00 + fromIntegral n)
arithOpcode (ShiftMantissa n)     = Long (0xFE00 + fromIntegral n)
arithOpcode (Print)               = Long  0x0700
arithOpcode (LParen)              = Short 0x01
arithOpcode (Plus)                = Short 0x03
arithOpcode (Minus)               = Short 0x04
arithOpcode (RParen)              = Short 0x05
arithOpcode (AssignNoNormalize)   = Short 0x07
arithOpcode (Assign)              = Short 0x08
arithOpcode (Times)               = Short 0x09
arithOpcode (Colon)               = Short 0x0A
arithOpcode (Square)              = Short 0x0B
arithOpcode (Cube)                = Short 0x0C
arithOpcode (Cotan)               = Short 0xF0
arithOpcode (Tan)                 = Short 0xF1
arithOpcode (Ln)                  = Short 0xF2
arithOpcode (SquareRoot)          = Short 0xF3
arithOpcode (TransformToDecimal)  = Short 0xF4
arithOpcode (Exp)                 = Short 0xF5
arithOpcode (ArcSin)              = Short 0xF6
arithOpcode (ArcTan)              = Short 0xF7
arithOpcode (Sin)                 = Short 0xF8
arithOpcode (Cos)                 = Short 0xF9
arithOpcode (E)                   = Short 0xFA
arithOpcode (ExtractExponenent)   = Short 0xFB
arithOpcode (Mod)                 = Short 0xFC
arithOpcode (Sign)                = Short 0xFF
