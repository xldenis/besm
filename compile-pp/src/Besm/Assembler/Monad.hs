{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Besm.Assembler.Monad where

import Besm.Assembler.Syntax
import Control.Monad.State
import Control.Monad.Fix

-- * SnocList

-- | Since we want to append instructions, we build them in reverse and then flip the whole
-- | list at the very end, instead of performing an expensive append for each instruction.
newtype SnocList a = SnocList { unSnocList :: [a] }
  deriving (Eq, Show)

instance Semigroup (SnocList a) where
  SnocList xs <> SnocList ys = SnocList $ ys ++ xs

instance Monoid (SnocList a) where
  mempty = SnocList []

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList $ x : xs

getSnocList :: SnocList a -> [a]
getSnocList = reverse . unSnocList


-- * Builder Monad
-- $builder The Builder monad. It is basically a simplified version of llvm-hs' IRBuilder monad.

data BuilderState = BuilderState
  { currentBlock :: CurrentBlock
  , builtBlocks  :: SnocList BasicBlock
  }

-- | The current basic block being assembled by a builder
data CurrentBlock = CurrentBlock
  { currentInstrs :: SnocList Instruction
  , blockAddr     :: Address
  , currentAddr   :: Address
  }

emptyCurrentBlock :: Address -> CurrentBlock
emptyCurrentBlock addr = CurrentBlock mempty addr addr

newtype Builder a = Builder { unBuilder :: State BuilderState a }
  deriving (Functor, Applicative, Monad, MonadState BuilderState, MonadFix)

runProcedure :: String -- ^ The name of the procedure
  -> Builder a  -- ^ The builder for the body
  -> Procedure Address
runProcedure nm bbs = Proc nm (runBuilder (op 999) bbs)

runBuilder :: Address  -- ^ The address to start the first, anonymous, block at.
  -> Builder a -- ^ The assembly builder to run
  -> [BasicBlock]
runBuilder i = getSnocList . builtBlocks . flip execState (BuilderState (emptyCurrentBlock i) (SnocList [])) . unBuilder

modifyBlock :: MonadState BuilderState m => (CurrentBlock -> CurrentBlock) -> m ()
modifyBlock f = do
  modify $ \s -> s { currentBlock = f (currentBlock s) }

-- | Emit an instruction into the current basic block and increment the current address
emitInstr :: Instruction -> Builder Address
emitInstr instr = do
  addr <- gets (currentAddr . currentBlock)

  modifyBlock $ \bb -> bb
    { currentInstrs = currentInstrs bb `snoc` instr
    , currentAddr = offAddr (currentAddr bb) 1
    }

  return addr

-- | Terminate a block and implicitly create a new, anonymous block.
emitTerm :: Terminator -> Builder Address
emitTerm term = do
  bb <- gets currentBlock

  let basicBlock = BB
        { instrs      = getSnocList $ currentInstrs bb
        , baseAddress = blockAddr bb
        , terminator  = term
        }

  modify $ \bs -> bs
    { currentBlock = emptyCurrentBlock (Block (blockAddr bb))
    , builtBlocks  = builtBlocks bs `snoc` basicBlock
    }

  return (currentAddr bb)

-- | Create a named operator block.
operator :: Int -> Builder a -> Builder Address
operator nm body = do
  bb <- gets currentBlock
  case bb of
    CurrentBlock (SnocList []) _ _ -> modifyBlock (const $ CurrentBlock mempty (op nm) (op nm)) >> body >> return (op nm)
    CurrentBlock a  _ _ -> emitTerm (Chain (op nm)) >> operator nm body

-- | Create a new anonymous block. The base address will be just be the current adddress, incremented.
block :: Builder a -> Builder Address
block body = do
  bb <- gets currentBlock
  case bb of
    CurrentBlock (SnocList []) _ _ -> body >> return (currentAddr bb)
    CurrentBlock a  _ _ -> emitTerm (Chain (currentAddr bb)) >> block body

-- | Emit an empty cell. This is used to create space to insert a template.
empty = emitInstr $ Empty

-- ** BESM Machine Code
{- $machine-code
  Since the BESM operates exclusively with floating point numbers, they may need to be normalized
  after operations are applied. For this reason, many instructions offer two versions:
  one with normalization, and one without. In the Builder monad, these are distinguished by the prime
  marker. A primed function will __not__ perform normalization.
-}
-- | Addition
add, add' :: Address -> Address -> Address -> Builder Address
add   a b c  = emitInstr $ Add  a b c Normalized
add'  a b c  = emitInstr $ Add  a b c UnNormalized

-- | Add exponents. Adds the top six bits of each word and uses the mantissa of the first argument.
addE, addE' :: Address -> Address -> Address -> Builder Address
addE  a b c  = emitInstr $ AddE a b c Normalized
addE' a b c  = emitInstr $ AddE a b c UnNormalized

-- | Change Exponent. Use the exponent of the second argument with the mantissa of the first.
ce, ce' :: Address -> Address -> Address -> Builder Address
ce    a b c  = emitInstr $ Ce   a b c Normalized
ce'   a b c  = emitInstr $ Ce   a b c UnNormalized

-- | Division
div, div' :: Address -> Address -> Address -> Builder Address
div   a b c  = emitInstr $ Div  a b c Normalized
div'  a b c  = emitInstr $ Div  a b c UnNormalized

-- | Multiplication
mult, mult' :: Address -> Address -> Address -> Builder Address
mult  a b c  = emitInstr $ Mult a b c Normalized
mult' a b c  = emitInstr $ Mult a b c UnNormalized

-- | Subtraction
sub, sub' :: Address -> Address -> Address -> Builder Address
sub   a b c  = emitInstr $ Sub  a b c Normalized
sub'  a b c  = emitInstr $ Sub  a b c UnNormalized

-- | Subtract Exponents. Subtract the exponents of the first two arguments and use the mantissa of the first.
subE, subE' :: Address -> Address -> Address -> Builder Address
subE  a b c  = emitInstr $ SubE a b c Normalized
subE' a b c  = emitInstr $ SubE a b c UnNormalized

-- | Extract mantissa
i a b c = emitInstr $ I a b c

{-|
  Transfer exponent. Transfers the exponent of an address.
-}
tExp, tExp' ::  Address -> Address -> Builder Address
{-|
  == Notes:

  It's currently unclear if this transfers it to the exponent bits of second argument or if it transfers it
  to the lower six bits (or how normalization plays out in these scenarios).
-}
tExp  a c    = emitInstr $ TExp a c   Normalized
tExp' a c    = emitInstr $ TExp a c   UnNormalized

{-| Transfer the modulus of a number. This transfers the absolute value of the first argument to the second one. -}
tMod, tMod' ::  Address -> Address -> Builder Address
tMod  a c    = emitInstr $ TMod a c   Normalized
tMod' a c    = emitInstr $ TMod a c   UnNormalized

{-| Transfer with change of sign. @tSign a b c@ transfers @a@ to @c@ while using the sign of @b@. -}
tSign, tSign' ::  Address -> Address -> Address -> Builder Address
tSign  a b c    = emitInstr $ TSign a b c   Normalized
tSign' a b c    = emitInstr $ TSign a b c   UnNormalized

{-| Transfer number. Transfers the first argument to the second -}
tN, tN' ::  Address -> Address -> Builder Address
tN  a c      = emitInstr $ TN   a c   Normalized
tN' a c      = emitInstr $ TN   a c   UnNormalized

ai, ai' :: Address -> Address -> Address -> Builder Address
{-|
  Add Instructions

  @ai a b c@
  Performs a bitwise addition of the lower 33 bits of @a@ and @b@, and uses the upper six bits of @a@, storing the result in @c@.
-}
ai  a b c    = emitInstr $ AI a b c

{-|
  Performs a bitwise addition of all 39 bits of @a@ and @b@, storing into @c@.
-}
ai' a b c    = emitInstr $ AICarry a b c

{-|
  Bitwise And

  Performs a logical, bitwise AND of the first two arguments and stores it into the final argument.

-}
bitAnd :: Address -> Address -> Address -> Builder Address
bitAnd a b c = emitInstr $ LogMult a b c

-- ** Control-Flow
{- $control-flow

  The BESM has two primary mechanism for sub-routine calls.

  The first is the "Local Counter", it has two Instruction Counters and can switch between
  them with the help of the CLCC and JCC instructions.

  #return-to-control#
  The second is using a Return-To-Control pattern. If an address (a) is stored in the second
  position of the CCCC instruction, the current central counter value will be written there
  before jumping to the CCCC target. Then the end of the subroutine contains two cells of like so:

  >   ┌────┬──────┬───┬───┐
  >   │ AI │ 110F │ a │ t │
  >   ├────┼──────┼───┼───┤
  > t │ 0  │      │ 0 │ 0 │
  >   └────┴──────┴───┴───┘


  Typically, t is taken as cell a.

  This construction allows for a general form of sub-routine calls.
-}


-- | Jump with local control. Jumps to an address, activating the local control system.
clcc ::  Address -> Builder Address
clcc addr   = emitInstr $ CLCC addr

-- | Jump to Control. Transfers control to the global control system.
jcc :: Builder Address
jcc         = emitInstr $ JCC

{-|
  @chain@ is not an actual instruction. It is a meta-linguistic addition that is
  used to indicate that one block should flow into another. If those two blocks can be
  laid out sequentially then it will be erased and control flow will transition naturally.
  Otherwise, it will be converted to a 'CCCC' that goes to the desired block.
-}
chain ::  Address -> Builder Address
chain addr = emitTerm $ Chain addr

-- | Unconditional Jump.
cccc ::  Address -> Builder Address
cccc addr = emitTerm $ CCCC addr

stop :: Builder Address
stop = emitTerm $ Stop

checkStop :: Builder Address
checkStop = emitTerm $ SwitchStop

{- |
  Call with Return-To-Control.

  Performs a jump to an operator using the return-to-control pattern (see above). The second argument
  is the operator which contains the return-to-control instructions.
-}
callRtc :: Address -> Address -> Builder Address
callRtc op retOp = emitInstr $ CallRTC (rtc retOp) op

-- | Return-To-Control. See above for an explanation of how this works.
retRTC :: Builder Address
retRTC = do
  addr <- gets (currentAddr . currentBlock)
  emitTerm $ RetRTC addr

{-|
  The instruction @comp a b c@ checks if:

  1. \(Exp(a) = Exp(b)\) and \(Mant(a) < Mant(b)\)
  2. \(Exp(a) > Exp(b)\) and \(Mant(a) < 0\)
  3. \(Exp(a) < Exp(b)\) and \(Mant(b) > 0\)

  If any of these conditions are met, then instruction c is carried out
  otherwise we go to instruction d.

  Note: this corresponds to asking x < y if x and y are both normalized.

  Instruction D doesn't actually belong in the machine code, this is a construction
  which will either disappear by making D the following instruction so it flows naturally
  or a CCCC <d> to jump there if the conditional fails.
-}

comp :: Address -> Address -> Address -> Address -> Builder Address
comp a b c d = emitTerm $ Comp a b c d

{-|
  If a and b are not bit-wise equal then go to c otherwise go to d

  Same as with comp, D is a meta-linguistic construct that will get compiled down into either
  zero or one instruction.
-}

compWord :: Address -> Address -> Address -> Address -> Builder Address
compWord a b c d = emitTerm $ CompWord a b c d

{-|
  Compares the absolute value of a and b, executing c if:

  1. \(E(x) = E(y)\) and \(|M(x)| < |M(y)|\)
  2. \(E(x) < E(y)\) and \(|M(y)| \neq 0\)

  Otherwise d is carried out
-}

compMod :: Address -> Address -> Address -> Address -> Builder Address
compMod a b c d = emitTerm $ CompWord a b c d

{-|
  Bit-Shifting

  The shift instruction will only shift bits within the mantissa, the exponent will always
  be set to 0.
-}

shift :: Address -> Address -> Address -> Builder Address
shift a b c = emitInstr $ Shift a b c

{-|
  ShiftAll does a full word shift setting any overflow bits to zero.
-}
shift' :: Address -> Address -> Address -> Builder Address
shift' a b c = emitInstr $ ShiftAll a b c

{-|
  The amount to be shifted by (m) is stored as m if the direction is to the left and as
  64 + m if the direction is to the right. These two helpers help distinguish between
  those cases and make it less confusing.
-}
left :: Int -> Address
left i = Absolute i

right :: Int -> Address
right i = Absolute (64 + i)

{-|
  IO

  Mostly incomplete

  There are several forms of IO available to the BESM

  - Magnetic Drums
  - Tape Drives
  - Photo-Electric Printer
  - Punch-Card Reader

  These are all used through the Ma / Mb instruction pair. Providing a different
  magic value to the first address of Ma tells it which operation to perform and where.

  Codes:

  0x030N: Write to mag drive N
  0x010N: Read from mag drive N
  0x0080: Read from tape
  0x028N: Write to tape drive N
  0x008N: Read from tape drive N
  0x02CN: Rewind tape drive

-}

readMD :: Int -> Address -> Address -> Address -> Builder Address
readMD n n1 n2 a = (emitInstr $ Ma (Absolute $ 0x100 + n) n1 a) >> (emitInstr $ Mb n2)

ma :: Address -> Address -> Address -> Builder Address
ma n n1 a = emitInstr $ Ma n n1 a

mb ::  Address -> Builder Address
mb n2 = emitInstr $ Mb n2

-- * Common Definitions
{- $common-defs
  These snippets are generic to the BESM, mostly references to values in the immutable
  diode store (though they may not be implemented as such right now).
-}

-- | A bitmask to select the first address of a cell
firstAddr :: Address
firstAddr = Absolute $ unsafeFromBesmAddress "1167"

-- | A bitmask to select the second address of a cell
secondAddr :: Address
secondAddr =  Absolute $ unsafeFromBesmAddress "116E"

-- | A bitmask to select the third address of a cell
thirdAddr :: Address
thirdAddr = Absolute $ unsafeFromBesmAddress "116F"


-- | A denormalized 1, this cell only has the lowest bit set.
one :: Address
one = Absolute $ unsafeFromBesmAddress "10B9"

-- | A normalized one which corresponds to 0.5 * 2^1 since in BESM all numbers are floats.
unity :: Address
unity = Absolute $ unsafeFromBesmAddress "1081"

-- | Normalized 2
two :: Address
two  = Absolute (unsafeFromBesmAddress "1082")

-- | Normalized 4
four :: Address
four = Absolute (unsafeFromBesmAddress "1084")

zero :: Address
zero = Absolute 0

{- |
  A denormalized 1 shifted to the left 22 places, putting it in the second address of the cell.
  This is useful when the first address needs to be incremented in a loop, for example, when reading
  from a block of values using TN instructions.
-}

oneFirstAddr :: Address
oneFirstAddr = Absolute $ unsafeFromBesmAddress "10B7"

{-| Standard Cells
  These are standard cells that are used as pseudo-registers in BESM code.
-}

cellA :: Address
cellA = Unknown "A"

cellB :: Address
cellB = Unknown "B"

cellA1 :: Address
cellA1 = Unknown "A + 1" -- A + 1


