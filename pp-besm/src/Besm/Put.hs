{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Besm.Put where

import Data.BitVector.Sized (BV (..), mkBV, pattern BV)
import qualified Data.BitVector.Sized as BV hiding (bitVector')
import Data.Char (digitToInt)
import Data.List (mapAccumL)
import Data.Serialize.Put
import Data.Word

import Data.Function
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
import GHC.TypeNats (KnownNat)

import Besm.Lower
import qualified Besm.Syntax.NonStandard as NS
import Data.Bits (bit, setBit, (.|.))
import Data.Digits (digits)
import Data.Text (Text)
import Text.Printf

import Data.Foldable
import Data.Parameterized.Pair

import BitLayout
import Data.Parameterized
import Data.Parameterized.List
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import GHC.TypeNats

{-
  A programme has to be prepared for the PP by being coded in binary.

  It will have the following layout after coding:

  +--------------+
  | Header Table |
  |--------------|
  |    Block V   |
  |--------------| <-- begins immediately after V
  |    Block P   |
  |--------------| <-- begins immediately after C
  |    Block C   |
  |--------------| <-- the last address of C has to be less than 0xEF
  |    Block K   |
  +--------------+
-}

encodeProgramme :: Programme -> [BV 39]
encodeProgramme p@(PP{..}) =
  let
    qa = calculateQuantityAddresses p
    vCells = blockV qa variableAddresses
    pCells = blockP qa parameters
    cCells = blockC constants
    kCells = blockK qa programme
    headerTable =
      programmeSummaryTable
        (fromIntegral block0Len)
        (length vCells)
        (length cCells)
        (length pCells)
        (length kCells)
        (fromIntegral blockGammaLen)
        (fromIntegral blockAlphaLen)
        (fromIntegral blockBetaLen)
   in
    replicate 7 b0 ++ headerTable ++ vCells ++ pCells ++ cCells ++ kCells

{-
  After preparing the programme, we also prepare a summary table will be prepended.

  These values are stored in the third address of each cell (viewed as an instruction).

  +------+----------------------------------------------+
  | Cell | Purpose                                      |
  |------|----------------------------------------------|
  | 0007 | Number of cells in block 0                   |
  | 0008 | Address of last cell in block V              |
  | 0009 | Address of first cell in block C             |
  | 000A | Address of last cell in block C              | <---- unclear whether this is really meant to be C or if it's a typo (P).
  | 000B | Address of first cell in block K minus 1     |
  | 000C | Address of last cell in block K              |
  | 000D | Address of first cell in block gamma minus 1 |
  | 000E | Address of first cell in block alpha minus 1 |
  | 000F | Address of first cell in block beta minus 1  |
  +------+----------------------------------------------+

-}

programmeSummaryTable :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [BV 39]
programmeSummaryTable olen vlen clen plen klen gammalen alphalen betalen =
  [ buildInstruction zero zero zero (bitVector' $ olen)
  , buildInstruction zero zero zero (bitVector' $ lastVAddr)
  , buildInstruction zero zero zero (bitVector' $ lastVAddr + plen + 1)
  , buildInstruction zero zero zero (bitVector' $ lastVAddr + plen + clen) -- last of C
  , buildInstruction zero zero zero (bitVector' $ lastVAddr + plen + clen) -- first of K - 1
  , buildInstruction zero zero zero (bitVector' $ lastVAddr + plen + clen + klen)
  , buildInstruction zero zero zero (bitVector' $ 0x2FF - (gammalen + alphalen + betalen) - 1)
  , buildInstruction zero zero zero (bitVector' $ 0x2FF - (alphalen + betalen) - 1)
  , buildInstruction zero zero zero (bitVector' $ 0x2FF - betalen - 1)
  ]
 where
  zero :: KnownNat n => BV n
  zero = BV.zero knownNat
  lastVAddr = 0x0010 + vlen - 1

{-
  """
  After this layout it is precisely defined in whcih cell will be
  located information on one or another letter symbol. This permits
  coding all symbols for quantities according to the following rule:
  each letter symbol is coded by the two successive sexadecimal numerals
  of the address of information on this symbol (for i_sigma, dependent on
  higher-order parameters, the address of the first cell with information
  on it is taken as the code). By this each quantity enters into a
  one-to-one correspondence to a given eight-place binary number.

  After coding the letter symbols of the quantity it is possible to
  code informatio non the symbols and information on the operators in
  the logical scheme. Everywhere below the expression "n" will denote
  the code of the quantity 𝔑.
  """ p 25.

  Before much of the programme can be coded, we need to get the offsets
  of all the quantities (variables & constants) in the program. We build
  a map which is used to convert from the name in the token-stream to the
  offset.
-}

type QuantityAddresses = [(Text, Word8)]

calculateQuantityAddresses :: Programme -> QuantityAddresses
calculateQuantityAddresses (PP{..}) =
  let
    (offV, vMap) = offsetsInV variableAddresses 0x0010
    (offP, pMap) = offsetsInP parameters offV
    (offC, cMap) = offsetsInC constants offP

    {- The address of the last cell of block C should not exceed 00EF. p25 -}
   in
    if offC > 0xEF
      then error "The last constant can't exceed 0x45"
      else vMap ++ pMap ++ cMap
 where
  offsetsInV :: BlockV -> Word8 -> (Word8, [(Text, Word8)])
  offsetsInV (V{..}) offset =
    let
      (off', map1) = Prelude.concat <$> mapAccumL variableAddrMap offset variableAddrs
      (off'', map2) = mapAccumL loopParamMap off' loopParameters
     in
      (off'', map1 ++ map2)

  offsetsInC :: [Constant] -> Word8 -> (Word8, [(Text, Word8)])
  offsetsInC constants offset = mapAccumL (\off c -> (off + 1, (cName c, off))) offset constants

  offsetsInP :: [ParameterInfo] -> Word8 -> (Word8, [(Text, Word8)])
  offsetsInP params offset = mapAccumL (\off p -> (off + 1, (pName p, off))) offset params

  variableAddrMap :: Word8 -> AddressBlock -> (Word8, QuantityAddresses)
  variableAddrMap offset (MainHead{..}) = Prelude.concat <$> mapAccumL headMap (offset + 1) (NonEmpty.toList heads)

  headMap :: Word8 -> BlockHead -> (Word8, QuantityAddresses)
  headMap offset (Head{..}) = mapAccumL varMap (offset + 1) (NonEmpty.toList vars)

  varMap :: Word8 -> VariableAddress -> (Word8, (Text, Word8))
  varMap offset va = (offset + 1, (vaName va, offset))

  loopParamMap :: Word8 -> LoopParameter -> (Word8, (Text, Word8))
  loopParamMap offset lp = (offset + 2, (lpName lp, offset))

{-
  Turn a BV into a human readable hex representation
-}

toHexString :: BV n -> String
toHexString = printf "%016x" . BV.asUnsigned

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
instAddr3 = (chunk 0 :: Chunk 11) <: emptyCell

buildInstruction :: BV 6 -> BV 11 -> BV 11 -> BV 11 -> BV 39
buildInstruction op addr1 addr2 addr3 =
  op <:> addr1 <:> addr2 <:> addr3

-- (bitVector 0)
-- & \cell -> inject opcodeLayout cell op
-- & \cell -> inject instAddr1    cell addr1
-- & \cell -> inject instAddr2    cell addr2
-- & \cell -> inject instAddr3    cell addr3

{-
  Layout of numbers:
                         2^-1 2^-2  ..
  +----------------------------------------+
  | 6 | 5 | .. | 1 | 33 | 32 | 31 | .. | 1 |
  +----------------------------------------+
    ^   ^            ^           ^
    |   |            |           |
    |   |            |           +---- 32-bit mantissa
    |   |            +---------------- 1-bit sign of number
    |   +----------------------------- 5-bit exponent
    +--------------------------------- 1-bit sign of exponenet

  The exponent is encoded in two's-complement notation, while the
  mantissa is a direct most-significant-bit encoding of the value.

-}

exponentL :: BitLayout 39 6
exponentL = (chunk 33 :: Chunk 6) <: emptyCell

numberSign :: BitLayout 39 1
numberSign = (chunk 32 :: Chunk 1) <: emptyCell

mantissa :: BitLayout 39 32
mantissa = (chunk 0 :: Chunk 32) <: emptyCell

buildNumber :: BV 6 -> BV 1 -> BV 32 -> BV 39
buildNumber exp sign mant =
  exp <:> sign <:> mant

-- bitVector 0
-- & \cell -> inject exponentL   cell exp
-- & \cell -> inject numberSign  cell sign
-- & \cell -> inject mantissa    cell mant

operatorSign :: BV 6
operatorSign = mkBV knownNat 0x018

b0 :: KnownNat k => BV k
b0 = mkBV knownNat 0

{-
  Block V: Variable Addresses

-}

blockV :: QuantityAddresses -> BlockV -> [BV 39]
blockV qa (V varAddrs loopParams) = (varAddrs >>= encodeMainHead qa) ++ (loopParams >>= encodeLoopParams qa)

encodeMainHead :: QuantityAddresses -> AddressBlock -> [BV 39]
encodeMainHead qa (MainHead size heads) =
  (buildInstruction (BV.mkBV knownNat 0x01F) (BV.zero knownNat) (unWord11 size) (BV.zero knownNat))
    : (encodeHead qa =<< (NonEmpty.toList heads))

encodeHead :: QuantityAddresses -> BlockHead -> [BV 39]
encodeHead qa (Head a b c vars) =
  buildInstruction (BV.zero knownNat) (unWord11 a) (unWord11 b) (unWord11 c)
    : zipWith (encodeVariable qa) [1 ..] (NonEmpty.toList vars)

encodeVariable :: QuantityAddresses -> Integer -> VariableAddress -> BV 39
encodeVariable qa ix (VaInfo _ p1 p2 p3 off dir) =
  buildNumber
    (bitVector' ix)
    (signBit dir)
    (packArithCodes (maybe 0 (quantityOffset qa) p1) (maybe 0 (quantityOffset qa) p2) (maybe 0 (quantityOffset qa) p3) off)
 where
  signBit FromStart = b0
  signBit FromEnd = BV.one knownNat

encodeLoopParams :: QuantityAddresses -> LoopParameter -> [BV 39]
encodeLoopParams pa (LP{..}) =
  buildInstruction (BV.zero knownNat) (quantityOffsetBits pa i0) (quantityOffsetBits pa lpA) (quantityOffsetBits pa lpB)
    : [buildInstruction (BV.zero knownNat) (BV.zero knownNat) (quantityOffsetBits pa j) (quantityOffsetBits pa k)]

{-
  Block P: Parameters

  """
  The order of information on parameters in block P is of major
  importance in laying out this block for the PP. To determine this
  order the loops in the logical scehme of the problem are ordered.
  The loops in the scheme are ordered from left to right according
  to their open-brackets. Information on the corresponding parameters
  in block P is locatted in this same order. For example, if the logical
  scheme, in which for simplicity only the boundaries of the loops ares
  shown, has the form

      [ ] [ [ [ ] ] [ ] ] [ [ ] ]
      i   k l m     r     s t

  the parameters in the block will be ordered in the following manner:
  i,m,l,r,k,t,s.
  """

  Note: In practice this seems like they are ordered left-to-right by **closing** bracket.

  Here, it is assumed that the parameters are already arragned in the correct order.
-}

blockP :: QuantityAddresses -> [ParameterInfo] -> [BV 39]
blockP qa params = map (encodeParameter qa) params

encodeParameter qa (InFin{..}) = buildInstruction (BV.zero knownNat) (quantityOffsetBits qa inP) (quantityOffsetBits qa finP) (BV.zero knownNat)
encodeParameter qa (CharacteristicLoop{..}) = buildInstruction (getCode theta) (quantityOffsetBits qa inP) (quantityOffsetBits qa loopA) (quantityOffsetBits qa loopB)

{-
  Block C: Constants
-}

blockC :: [Constant] -> [BV 39]
blockC constants = map toCell constants
 where
  toCell (Vacant{}) = b0
  toCell (Cell _ v) = numberToBesmFloating v

{-
  Block K: Programme
-}

blockK :: QuantityAddresses -> [Operator] -> [BV 39]
blockK pa codes = packCells $ encodeCode pa codes

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

-- packArithCodes :: Word8 -> Word8 -> Word8 -> Word8 -> BV 32
-- packArithCodes a b c d = bitVector 0
--   & \cell -> inject ((chunk 24 :: Chunk 8) <: empty) cell (bitVector $ fromIntegral a)
--   & \cell -> inject ((chunk 16 :: Chunk 8) <: empty) cell (bitVector $ fromIntegral b)
--   & \cell -> inject ((chunk 8  :: Chunk 8) <: empty) cell (bitVector $ fromIntegral c)
--   & \cell -> inject ((chunk 0  :: Chunk 8) <: empty) cell (bitVector $ fromIntegral d)

packArithCodes :: Word8 -> Word8 -> Word8 -> Word8 -> BV 32
packArithCodes a b c d =
  (BV.word8 a) <:> (BV.word8 b) <:> (BV.word8 c) <:> (BV.word8 d)

buildArithCodes :: BV 32 -> BV 39
buildArithCodes codes = inject mantissa (BV.zero knownNat) codes

quantityOffsetBits pa p = mkBV knownNat $ quantityOffset pa p

quantityOffset :: Integral a => QuantityAddresses -> Quantity -> a
quantityOffset pa p = fromIntegral . fromJust' $ unQ p `lookup` pa
 where
  fromJust' (Just a) = a
  fromJust' Nothing = error $ show p

bitVector' :: (KnownNat k, Integral a) => a -> BV k
bitVector' = mkBV knownNat . fromIntegral

-- type BV = BV

{-
  First we transform the input token stream into the raw codes for each token. Once that stream has been produced,
  We pass over it again to pack arithmetical operators into as few cells as possible.
-}

data IntermediateValue = Short Word8 | Long Word16 | Full (BV 39)
  deriving (Show, Eq)

infixl 6 <:>

(<:>) :: (KnownNat n, KnownNat m) => BV n -> BV m -> BV (n + m)
(<:>) = BV.concat knownNat knownNat

packCells :: [IntermediateValue] -> [BV 39]
packCells (Short a : Short b : Short c : Short d : others) = buildArithCodes (packArithCodes a b c d) : packCells others
packCells (Short a : Short b : Short c : others) = buildArithCodes (packArithCodes a b c 0) : packCells others
packCells (Short a : Short b : Long cd : others) = buildArithCodes ((bitVector' a :: BV 8) <:> (bitVector' b :: BV 8) <:> (bitVector' cd :: BV 16)) : packCells others
packCells (Short a : Long bc : Short d : others) = buildArithCodes ((bitVector' a :: BV 8) <:> (bitVector' bc :: BV 16) <:> (bitVector' d :: BV 8)) : packCells others
packCells (Short a : Short b : others) = buildArithCodes (packArithCodes a b 0 0) : packCells others
packCells (Short a : others) = buildArithCodes (packArithCodes a 0 0 0) : packCells others
packCells (Long ab : Short c : Short d : others) = buildArithCodes ((bitVector' ab :: BV 16) <:> (bitVector' c :: BV 8) <:> (bitVector' d :: BV 8)) : packCells others
packCells (Long ab : Long cd : others) = buildArithCodes ((bitVector' ab :: BV 16) <:> (bitVector' cd :: BV 16)) : packCells others
packCells (Full o : thers) = o : packCells thers
packCells (Long ab : others) = buildArithCodes ((bitVector' ab :: BV 16) <:> b0) : packCells others
packCells [] = []

encodeCode :: QuantityAddresses -> [Operator] -> [IntermediateValue]
encodeCode pa (Arith op : ops) = arithOpcode op : encodeCode pa ops
encodeCode pa (Parameter p : ops) = Short (fromJust $ (unQ p) `lookup` pa) : encodeCode pa ops
encodeCode pa (OperatorSign os : LogicalOperator lo : o : ops) = encodeLogicalOp pa (Just os) lo o ++ encodeCode pa ops
encodeCode pa (LogicalOperator lo : o : ops) = encodeLogicalOp pa Nothing lo o ++ encodeCode pa ops
encodeCode pa (OperatorSign os : ops) = Full (buildInstruction operatorSign (getOperator os) b0 b0) : encodeCode pa ops
encodeCode pa (LoopOpen p : ops) = Full (buildInstruction operatorSign (quantityOffsetBits pa p) b0 b0) : encodeCode pa ops
encodeCode pa (LoopClose : ops) = Full (buildInstruction (mkBV knownNat 0x01F) b13FF b13FF b13FF) : encodeCode pa ops
 where
  b13FF = mkBV knownNat 0x13FF
encodeCode pa (NS ns a b c : ops) = Full (buildInstruction (putOpCode ns) (bvAddr pa a) (bvAddr pa b) (bvAddr pa c)) : encodeCode pa ops
encodeCode pa (Empty : ops) = Full b0 : encodeCode pa ops
encodeCode pa [LogicalOperator lo] = encodeLogicalOp pa Nothing lo Empty
encodeCode pa [] = []

{-
  Non-Standard Operators are simply machine-code carried up to the level of PP.

  To encode them we simply need to encode them in the correct format while placing
  quantities instead of addresses.
-}

bvAddr :: QuantityAddresses -> Address -> BV 11
bvAddr qa (Abs a) = bitVector' a
bvAddr qa (VarQ v) = bitVector' . fromJust $ unVar v `lookup` qa

bv :: KnownNat k => Integer -> BV k
bv = mkBV knownNat

putOpCode :: NS.NonStandardOpcode -> BV 6
putOpCode (NS.Add n) = bv 0x001
putOpCode (NS.Sub n) = bv $ normToBit n .|. 0x002
putOpCode (NS.Mult n) = bv $ normToBit n .|. 0x003
putOpCode (NS.Div n) = bv $ normToBit n .|. 0x004
putOpCode (NS.AddE n) = bv $ normToBit n .|. 0x005
putOpCode (NS.SubE n) = bv $ normToBit n .|. 0x006
putOpCode (NS.Ce n) = bv $ normToBit n .|. 0x007
putOpCode (NS.Xa n) = bv $ normToBit n .|. 0x008
putOpCode (NS.Xb n) = bv $ normToBit n .|. 0x009
putOpCode (NS.DivA n) = bv $ normToBit n .|. 0x00A
putOpCode (NS.DivB n) = bv $ normToBit n .|. 0x00B
putOpCode (NS.TN n) = bv $ normToBit n .|. 0x00C
putOpCode (NS.PN) = bv 0x02C
putOpCode (NS.TMin n) = bv $ normToBit n .|. 0x00D
putOpCode (NS.TMod n) = bv $ normToBit n .|. 0x00E
putOpCode (NS.TSign n) = bv $ normToBit n .|. 0x00F
putOpCode (NS.TExp n) = bv $ normToBit n .|. 0x010
putOpCode (NS.Shift) = bv 0x011
putOpCode (NS.ShiftAll) = bv 0x031
putOpCode (NS.AI) = bv 0x012
putOpCode (NS.AICarry) = bv 0x032
putOpCode (NS.I) = bv 0x013
putOpCode (NS.Ma) = bv 0x016
putOpCode (NS.Mb) = bv 0x017
putOpCode (NS.LogMult) = bv 0x01D
putOpCode (NS.JCC) = bv 0x019
putOpCode (NS.CLCC) = bv 0x01A
putOpCode (NS.Comp) = bv 0x014
putOpCode (NS.CompWord) = bv 0x034
putOpCode (NS.CompMod) = bv 0x015
putOpCode (NS.CCCC) = bv 0x01B
putOpCode (NS.CCCCSnd) = bv 0x01B
putOpCode (NS.Stop) = bv 0x01F
putOpCode (NS.SwitchStop) = bv 0x01C

normToBit :: NS.NormalizeResult -> Integer
normToBit NS.Normalized = 0
normToBit NS.UnNormalized = bit 5

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

encodeLogicalOp :: QuantityAddresses -> Maybe OperatorSign -> LogicalOperator -> Operator -> [IntermediateValue]
encodeLogicalOp pa opSign (Op x defaultOp branches) following =
  let
    followingOperator = encodeCode pa [following]
    thirdAddress = extract instAddr3 (head $ packCells followingOperator)
    padding = if thirdAddress == b0 then [] else [Full b0]
    operatorHead = Full $ buildInstruction operatorSign (maybe b0 getOperator opSign) (quantityOffsetBits pa x) (getOperator defaultOp)
   in
    operatorHead : map encodeBranch branches ++ padding ++ followingOperator
 where
  {-
    """
    If a segment has only one boundary point, the code of its
    quantity is palced in the first address of the line of information
    while the second address remains vacant.
    """
  -}
  encodeBranch (op, rangeType, lowerBound, upperBound) =
    let secondBound = maybe b0 (quantityOffsetBits pa) upperBound
     in Full $ buildInstruction (rangeCode rangeType) (quantityOffsetBits pa lowerBound) (secondBound) (getOperator op)

rangeCode :: RangeType -> BV 6
rangeCode (RightImproperSemi) = mkBV knownNat 1
rangeCode (Segment) = mkBV knownNat 2
rangeCode (SemiSegment) = mkBV knownNat 3
rangeCode (LeftImproper) = mkBV knownNat 4
rangeCode (RightImproper) = mkBV knownNat 8
rangeCode (LeftImproperSemi) = mkBV knownNat 13
rangeCode (SemiInterval) = mkBV knownNat 18
rangeCode (Interval) = mkBV knownNat 19

arithOpcode :: ArithOperator -> IntermediateValue
arithOpcode (NLParen n) = Long (0x0200 + fromIntegral n)
arithOpcode (NRParen n) = Long (0x0600 + fromIntegral n)
arithOpcode (ChangeExponent n) = Long (0xFD00 + fromIntegral n)
arithOpcode (ShiftMantissa n) = Long (0xFE00 + fromIntegral n)
arithOpcode (Print) = Long 0x0700
arithOpcode (LParen) = Short 0x01
arithOpcode (Plus) = Short 0x03
arithOpcode (Minus) = Short 0x04
arithOpcode (RParen) = Short 0x05
arithOpcode (AssignNoNormalize) = Short 0x07
arithOpcode (Assign) = Short 0x08
arithOpcode (Times) = Short 0x09
arithOpcode (Colon) = Short 0x0A
arithOpcode (Square) = Short 0x0B
arithOpcode (Cube) = Short 0x0C
arithOpcode (Cotan) = Short 0xF0
arithOpcode (Tan) = Short 0xF1
arithOpcode (Ln) = Short 0xF2
arithOpcode (SquareRoot) = Short 0xF3
arithOpcode (TransformToDecimal) = Short 0xF4
arithOpcode (Exp) = Short 0xF5
arithOpcode (ArcSin) = Short 0xF6
arithOpcode (ArcTan) = Short 0xF7
arithOpcode (Sin) = Short 0xF8
arithOpcode (Cos) = Short 0xF9
arithOpcode (E) = Short 0xFA
arithOpcode (ExtractExponenent) = Short 0xFB
arithOpcode (Mod) = Short 0xFC
arithOpcode (Sign) = Short 0xFF

{-
  Converts numbers to BESM floating point

  For now only handles positive integers, needs to be expanded to all floating points

  Known bugs:

  Can't represent fractional values.
    - Probably want to take a type of (Int, Int) as input

-}

numberToBesmFloating :: Int -> BV 39
numberToBesmFloating num =
  let
    bits = (== 1) <$> digits 2 (Prelude.abs num)
    exp = (== 1) <$> digits 2 (length bits) -- this is soooo wrong for negative numbers...
    sBit = fromEnum $ num < 0
   in
    buildNumber (bitVector' $ length bits) (BV.bool $ sBit == 1) (toBitVector bits)
 where
  toBitVector :: [Bool] -> BV 32
  -- toBitVector bits =
  toBitVector bits = case BV.bitsBE (pad ++ bits) of
    p ->
      viewPair
        ( \prf b ->
            case polyEqF prf (knownNat @32) of
              Just Refl -> b
              _ -> error "wtf man"
        )
        p
   where
    pad = take (32 - length bits) (repeat False)

-- toBitVector' (len - 1) (bits) b0
-- where
-- len = length bits
-- toBitVector' 0 (x:_:_)  _  = error "wtf man"
-- toBitVector' 0 []     bv = bv
-- toBitVector' i (1:xs) bv = toBitVector' (i - 1) xs (bv `BV.setBit` (width knownNat bv - (len - i)))
-- toBitVector' i (_:xs) bv = toBitVector' (i - 1) xs bv
-- toBitVector' i []     bv = bv

{-
  Converts from a BESM address format which is a mixture of binary, quaternary and hex.
-}

fromBesmAddress :: String -> Maybe Int
fromBesmAddress inp =
  let
    str = replicate (4 - length inp) '0' ++ inp
    (b : q : h1 : h2 : []) = str
    bin = digitToInt b
    quat = digitToInt q
   in
    case bin < 2 && quat < 4 of
      True -> Just $ digitToInt h2 + digitToInt h1 * 2 ^ 4 + quat * 2 ^ 8 + bin * 2 ^ 10
      False -> Nothing

unsafeFromBesmAddress :: String -> Int
unsafeFromBesmAddress = fromJust . fromBesmAddress
