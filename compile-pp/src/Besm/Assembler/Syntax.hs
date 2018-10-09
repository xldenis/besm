{-# LANGUAGE DataKinds, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving, DeriveTraversable, BinaryLiterals #-}
module Besm.Assembler.Syntax where

import Besm.Put (buildNumber, buildInstruction)
import Data.BitVector.Sized
import Data.Bits

data Address
  = Operator Int
  | Offset Address Int
  | Absolute Int
  | Procedure String Address
  | Unknown String
  | RTC Address
  deriving (Show, Eq, Ord)

formatAddr (Operator i) = "op. " ++ show i
formatAddr (Offset a i) = formatAddr a ++ " + " ++ show i
formatAddr (Absolute i) = "abs. " ++ show i
formatAddr (Procedure s op) = show s ++ formatAddr op
formatAddr (Unknown str) = "uk. " ++ str
formatAddr (RTC a) = "rtc. " ++ formatAddr a

offAddr (Offset a o) i = Offset a (o + i)
offAddr a i = Offset a i

op = Operator

rtc = RTC

isUnknown (Unknown _) = True
isUnknown _           = False

unknowns :: Address -> [String]
unknowns (Offset o _) = unknowns o
unknowns (Procedure _ o) = unknowns o
unknowns (Unknown u) = [u]
unknowns (RTC o) = unknowns o
unknowns _ = []

type BasicBlock = BB Address

data ConstantInfo
  = Size Int -- number of cells to reserve
  | Val  Int -- Value to store in one cell
  | Raw  Int -- Raw value to store
  | Addr Address -- Pointer to an address
  deriving (Show, Eq)

constantSize (Size i) = i
constantSize _ = 1

data BB a = BB
  { instrs      :: [Instr a]
  , terminator  ::  Term  a
  , baseAddress :: a
  } deriving (Show, Functor, Foldable, Eq, Traversable)

data Procedure a = Proc
  { procName :: String
  , blocks :: [BB a]
  } deriving (Show, Eq, Functor)

instLen :: BB a -> Int
instLen bb = length (instrs bb) + termLen (terminator bb)
  where
  termLen (RetRTC _) = 2
  termLen (Chain _) = 0
  termLen _        = 1

data NormalizeResult
  = Normalized
  | UnNormalized
  deriving (Show, Eq)

type Instruction = Instr Address
type RawInstr = Instr Int

data Instr a
  = Add       a a a NormalizeResult
  | Sub       a a a NormalizeResult
  | Mult      a a a NormalizeResult
  | Div       a a a NormalizeResult
  | AddE      a a a NormalizeResult
  | SubE      a a a NormalizeResult
  | Ce        a a a NormalizeResult
  | Xa        a a a NormalizeResult
  | Xb            a NormalizeResult
  | DivA      a a a NormalizeResult
  | DivB          a NormalizeResult
  | TN        a   a NormalizeResult
  | PN        a
  | TMin      a   a NormalizeResult
  | TMod      a   a NormalizeResult -- I think the 'modulus' actually means magnitude of mantissa? might actually mean the sexadecimal width of the number??? wtf...
  | TSign     a a a NormalizeResult
  | TExp      a   a NormalizeResult
  | Shift     a a a
  | ShiftAll  a a a
  | AI        a a a
  | AICarry   a a a
  | I         a a a
  | Ma        a a a
  | Mb          a
  | LogMult   a a a
  | CallRTC     a a
  | CLCC          a
  | JCC
  | Empty
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Terminator = Term Address

data Term a
  = Comp      a a a a
  | CompWord  a a a a
  | CompMod   a a a a
  | CCCC          a
  | CCCCSnd     a a
  | Stop
  | SwitchStop
  | Chain     a -- meta-linguistic, chains two basic blocks together
  | RetRTC a
  deriving (Show, Eq, Functor, Foldable, Traversable)

type RawBlock = BB Int

asmToCell :: RawBlock -> [BitVector 39]
asmToCell (BB is tm adx) =
  map (instToCell . fmap fromIntegral) is ++ (termToCell $ fmap fromIntegral tm)

instToCell :: Instr Integer -> BitVector 39
instToCell (Add       a b c n) = buildInstruction (bitVector 0x001) (bitVector a) (bitVector b) (bitVector c)
instToCell (Sub       a b c n) = buildInstruction (bitVector $ normToBit n .|. 0x002) (bitVector a) (bitVector b) (bitVector c)
instToCell (Mult      a b c n) = buildInstruction (bitVector $ normToBit n .|. 0x003) (bitVector a) (bitVector b) (bitVector c)
instToCell (Div       a b c n) = buildInstruction (bitVector $ normToBit n .|. 0x004) (bitVector a) (bitVector b) (bitVector c)
instToCell (AddE      a b c n) = buildInstruction (bitVector $ normToBit n .|. 0x005) (bitVector a) (bitVector b) (bitVector c)
instToCell (SubE      a b c n) = buildInstruction (bitVector $ normToBit n .|. 0x006) (bitVector a) (bitVector b) (bitVector c)
instToCell (Ce        a b c n) = buildInstruction (bitVector $ normToBit n .|. 0x007) (bitVector a) (bitVector b) (bitVector c)
instToCell (Xa        a b c n) = buildInstruction (bitVector $ normToBit n .|. 0x008) (bitVector a) (bitVector b) (bitVector c)
instToCell (Xb            c n) = buildInstruction (bitVector $ normToBit n .|. 0x009) (bitVector 0) (bitVector 0) (bitVector c)
instToCell (DivA      a b c n) = buildInstruction (bitVector $ normToBit n .|. 0x00A) (bitVector a) (bitVector b) (bitVector c)
instToCell (DivB          c n) = buildInstruction (bitVector $ normToBit n .|. 0x00B) (bitVector 0) (bitVector 0) (bitVector c)
instToCell (TN        a   c n) = buildInstruction (bitVector $ normToBit n .|. 0x00C) (bitVector a) (bitVector 0) (bitVector c)
instToCell (PN        a      ) = buildInstruction (bitVector 0x02C) (bitVector a) (bitVector 0) (bitVector 0)
instToCell (TMin      a   c n) = buildInstruction (bitVector $ normToBit n .|. 0x00D) (bitVector a) (bitVector 0) (bitVector c)
instToCell (TMod      a   c n) = buildInstruction (bitVector $ normToBit n .|. 0x00E) (bitVector a) (bitVector 0) (bitVector c)
instToCell (TSign     a b c n) = buildInstruction (bitVector $ normToBit n .|. 0x00F) (bitVector a) (bitVector b) (bitVector c)
instToCell (TExp      a   c n) = buildInstruction (bitVector $ normToBit n .|. 0x010) (bitVector a) (bitVector 0) (bitVector c)
instToCell (Shift     a b c  ) = buildInstruction (bitVector 0x011) (bitVector a) (bitVector b) (bitVector c)
instToCell (ShiftAll  a b c  ) = buildInstruction (bitVector 0x031) (bitVector a) (bitVector b) (bitVector c)
instToCell (AI        a b c  ) = buildInstruction (bitVector 0x012) (bitVector a) (bitVector b) (bitVector c)
instToCell (AICarry   a b c  ) = buildInstruction (bitVector 0x032) (bitVector a) (bitVector b) (bitVector c)
instToCell (I         a b c  ) = buildInstruction (bitVector 0x013) (bitVector a) (bitVector b) (bitVector c)
instToCell (Ma        a b c  ) = buildInstruction (bitVector 0x016) (bitVector a) (bitVector b) (bitVector c)
instToCell (Mb          b    ) = buildInstruction (bitVector 0x017) (bitVector 0) (bitVector b) (bitVector 0)
instToCell (LogMult   a b c  ) = buildInstruction (bitVector 0x01D) (bitVector a) (bitVector b) (bitVector c)
instToCell (CallRTC     b c  ) = buildInstruction (bitVector 0x01B) (bitVector 0) (bitVector b) (bitVector c)
instToCell (JCC              ) = buildInstruction (bitVector 0x019) (bitVector 0) (bitVector 0) (bitVector 0)
instToCell (CLCC          c  ) = buildInstruction (bitVector 0x01A) (bitVector 0) (bitVector 0) (bitVector c)
instToCell (Empty)             = bitVector 0

normToBit :: NormalizeResult -> Integer
normToBit Normalized   = 0
normToBit UnNormalized = bit 5

termToCell :: Term Integer -> [BitVector 39]
termToCell (Comp      a b c _) = pure $ buildInstruction (bitVector 0x014) (bitVector a) (bitVector b) (bitVector c)
termToCell (CompWord  a b c _) = pure $ buildInstruction (bitVector 0x034) (bitVector a) (bitVector b) (bitVector c)
termToCell (CompMod   a b c _) = pure $ buildInstruction (bitVector 0x015) (bitVector 0) (bitVector b) (bitVector c)
termToCell (CCCC          c  ) = pure $ buildInstruction (bitVector 0x01B) (bitVector 0) (bitVector 0) (bitVector c)
termToCell (CCCCSnd     b c  ) = pure $ buildInstruction (bitVector 0x01B) (bitVector 0) (bitVector b) (bitVector c)
termToCell (Stop)              = pure $ buildInstruction (bitVector 0x01F) (bitVector 0) (bitVector 0) (bitVector 0)
termToCell (SwitchStop)        = pure $ buildInstruction (bitVector 0x01C) (bitVector 0) (bitVector 0) (bitVector 0)
termToCell (RetRTC a)          = [instToCell (AI 0b10100001111 (a+1) (a+1)), bitVector 0]
termToCell (Chain     _)       = []
