{-# LANGUAGE DataKinds, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving, DeriveTraversable, BinaryLiterals, TemplateHaskell #-}
module Besm.Assembler.Syntax
( module Besm.Assembler.Syntax
, unsafeFromBesmAddress
) where

import Besm.Put (buildNumber, buildInstruction, unsafeFromBesmAddress, numberToBesmFloating)
import Data.BitVector.Sized
import Data.Bits

data Address
  = Operator Int
  | Block Address
  | Offset Address Int
  | Absolute Int
  | Unknown String -- ^ Address of a variable or constant value. Each Unknown needs to have an associated ConstantDef
  | Procedure String Address -- ^ Address inside of another procedure
  | RTC Address -- ^ Address of the RTC instructions at the end of the block the argument belongs to.
  deriving (Show, Eq, Ord)

-- | Basic pretty-printing of addresses.
formatAddr :: Address -> String
formatAddr (Operator i) = "op. " ++ show i
formatAddr (Block a) = "blk. " ++ formatAddr a
formatAddr (Offset a i) = formatAddr a ++ " + " ++ show i
formatAddr (Absolute i) = "abs. " ++ show i
formatAddr (Procedure s op) = "proc. " ++ show s ++ " " ++ formatAddr op
formatAddr (Unknown str) = "uk. " ++ str
formatAddr (RTC a) = "rtc. " ++ formatAddr a

-- | Smart constructor to offset from an address.
offAddr (Offset a o) i = Offset a (o + i)
offAddr a i = Offset a i

op :: Int -> Address
op = Operator

rtc :: Address -> Address
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

-- | In which address of the constant cell does @Addr@ store the value
data AddrPos = First | Second | Third
  deriving (Show, Eq)

data ConstantInfo a
  = Size Int -- ^ number of cells to reserve
  | Val  Int -- ^ Value to store in one cell
  | Raw  Int -- ^ Raw value to store
  | Addr AddrPos a   -- ^ Pointer to an address
  {-|
    Indicate this variable is meant to be a working cell. At layout time, all working cells
    will be grouped together (and may potentially even be optimized to reduce the total amount).

    All working cells will be initialized to 0 in the final program.
  -}
  | Cell
  {- | This is a cell that holds a partial instruction that potentially
      references other unkowns / variables. At linking time those constants will be resolved,
      and properly initialized.
  -}
  | Template (Instr a)
  deriving (Show, Eq, Functor, Traversable, Foldable)

-- | Get the size in cells of a constant
constantSize :: ConstantInfo a -> Int
constantSize (Size i) = i
constantSize _ = 1

constantToCell :: ConstantInfo Int -> [BitVector 39]
constantToCell (Size i) = replicate i (bitVector 0)
constantToCell (Val  i) = [numberToBesmFloating i]
constantToCell (Raw  i) = [bitVector $ fromIntegral i]
constantToCell (Addr pos i) = [bitVector . fromIntegral $ i `shift` (offset pos)]
  where offset First = 22
        offset Second = 11
        offset Third  = 0
constantToCell (Template i) = [instToCell $ fmap fromIntegral i]
constantToCell (Cell) = [bitVector 0]

{- |
  To help preserve sanity while writing the compiler, the assembly is structured using
  basic blocks. It turns out that the 'operators' in the compiler almost map 1-1 with
  basic blocks except in a few cases where an operator may attempt to perform multiple
  comparisons. Every basic block is given a 'base address' which is usually the operator
  address the block defines. This is also used as the block's name in debugging output.
-}
data BB a = BB
  { instrs      :: [Instr a]
  , terminator  ::  Term  a
  , baseAddress :: a
  } deriving (Show, Functor, Foldable, Eq, Traversable)

data Procedure a = Proc
  { procName :: String
  , blocks :: [BB a]
  } deriving (Show, Eq, Functor)

-- | The size of a block in memory including the terminator instruction
blockLen :: BB a -> Int
blockLen bb = length (instrs bb) + termLen (terminator bb)
  where
  termLen (RetRTC _) = 2
  termLen (Chain _) = 0
  termLen _        = 1

{-|
  Since BESM only uses floating point numbers, they must be normalized after operations
  however, it can be desirable to suppress normalization which is why those operations
  have a bitflag that can be set to indicate that normalization should be prevented.
-}
data NormalizeResult
  = Normalized
  | UnNormalized
  deriving (Show, Eq)

type Instruction = Instr Address
type RawInstr = Instr Int

{-|
  The type of non-terminator instructions. This includes all arithmetical operations,
  logical operations like shifting and bit-wise, bit-wise addition, IO, and sub-routine calls.
-}
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
  | TN        a   a NormalizeResult -- ^ Transfer Number
  | PN        a                     -- ^ Print number in decimal format
  | TMin      a   a NormalizeResult -- ^ Transfer number with negation
  | TMod      a   a NormalizeResult -- ^ Transfer the modulus (absolute value) of a number
  | TSign     a a a NormalizeResult -- ^ Transfer a number with the sign of the result determined by the sign of the second argument
  | TExp      a   a NormalizeResult -- ^ Transfer the exponent of a number
  | Shift     a a a                 -- ^ Shift the lower 32 bits of a word
  | ShiftAll  a a a                 -- ^ Shift __all__ the bits of a word
  | AI        a a a                 -- ^ Perform bitwise addition of the lower 32 bits and use the opcode of the first argument
  | AICarry   a a a                 -- ^ Perform bitwise addition of all the bits in a word
  | I         a a a                 -- ^ "Extract mantissa", splits the exponent and mantissa of the first cell into the other two (idk how yet).
  | Ma        a a a                 -- ^ IO An Ma is always followed by an Mb
  | Mb          a                   -- ^ IO An Mb always follows an Ma
  | LogMult   a a a                 -- ^ Bit-wise AND
  | CallRTC     a a                 -- ^ Call a subroutine using the RTC pattern. First argument is subroutine entry and second is exit.
  | CLCC          a                 -- ^ Call a subroutine using a jump to local control
  | JCC                             -- ^ Return to global control
  | Empty                           -- ^ Empty cell
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Terminator = Term Address

data Term a
  {- |
    @Comp a b c d@ If @a < b@, then go to c otherwise go to d.
    The fourth argument is an artificial addition that is used during assembly.
    It may get converted to a @CCCC@ if d can't be placed immediately after the comparison.
  -}
  = Comp      a a a a
  | CompWord  a a a a
  | CompMod   a a a a
  | CCCC          a -- ^ Unconditional jump
  | CCCCSnd     a a -- ^ @CCCCSnd a b@ jumps to b, but writes the current instruction into a. Used for RTC.
  | Stop
  | SwitchStop
  | Chain     a -- ^  meta-linguistic, will be eliminated entirely before codegen.
  | RetRTC a    -- ^ Insert the return-to-control instructions
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
