{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Besm.Assembler.Monad where

import Besm.Assembler.Syntax
import Control.Monad.State
import Control.Monad.Fix

import Debug.Trace

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

data BuilderState = BuilderState
  { currentBlock :: CurrentBlock
  , builtBlocks  :: SnocList BasicBlock
  }

data CurrentBlock = CurrentBlock
  { currentInstrs :: SnocList Instruction
  , blockAddr     :: Address
  , currentAddr   :: Address
  }

emptyCurrentBlock addr = CurrentBlock mempty addr addr

newtype Builder a = Builder { unBuilder :: State BuilderState a }
  deriving (Functor, Applicative, Monad, MonadState BuilderState, MonadFix)

runProcedure nm bbs = Proc nm (runBuilder (op 999) bbs)

runBuilder i = getSnocList . builtBlocks . flip execState (BuilderState (emptyCurrentBlock i) (SnocList [])) . unBuilder

modifyBlock f = do
  modify $ \s -> s { currentBlock = f (currentBlock s) }

emitInstr :: Instruction -> Builder Address
emitInstr instr = do
  addr <- gets (currentAddr . currentBlock)

  modifyBlock $ \bb -> bb
    { currentInstrs = currentInstrs bb `snoc` instr
    , currentAddr = offAddr (currentAddr bb) 1
    }

  return addr

emitTerm :: Terminator -> Builder Address
emitTerm term = do
  bb <- gets currentBlock

  let basicBlock = BB
        { instrs      = getSnocList $ currentInstrs bb
        , baseAddress = blockAddr bb
        , terminator  = term
        }

  modify $ \bs -> bs
    { currentBlock = emptyCurrentBlock (currentAddr bb `offAddr` 1)
    , builtBlocks  = builtBlocks bs `snoc` basicBlock
    }

  return (currentAddr bb)

operator :: Int -> Builder a -> Builder Address
operator nm body = do
  bb <- gets currentBlock
  case bb of
    CurrentBlock (SnocList []) _ _ -> modifyBlock (const $ CurrentBlock mempty (op nm) (op nm)) >> body >> return (op nm)
    CurrentBlock a  _ _ -> emitTerm (Chain (op nm)) >> operator nm body

block :: Builder a -> Builder Address
block body = do
  bb <- gets currentBlock
  case bb of
    CurrentBlock (SnocList []) _ _ -> body >> return (currentAddr bb)
    CurrentBlock a  _ _ -> emitTerm (Chain (currentAddr bb)) >> block body

empty = emitInstr $ Empty

add, add' :: Address -> Address -> Address -> Builder Address
add   a b c  = emitInstr $ Add  a b c Normalized
add'  a b c  = emitInstr $ Add  a b c UnNormalized

addE, addE' :: Address -> Address -> Address -> Builder Address
addE  a b c  = emitInstr $ AddE a b c Normalized
addE' a b c  = emitInstr $ AddE a b c UnNormalized

ce, ce' :: Address -> Address -> Address -> Builder Address
ce    a b c  = emitInstr $ Ce   a b c Normalized
ce'   a b c  = emitInstr $ Ce   a b c UnNormalized

div, div' :: Address -> Address -> Address -> Builder Address
div   a b c  = emitInstr $ Div  a b c Normalized
div'  a b c  = emitInstr $ Div  a b c UnNormalized

mult, mult' :: Address -> Address -> Address -> Builder Address
mult  a b c  = emitInstr $ Mult a b c Normalized
mult' a b c  = emitInstr $ Mult a b c UnNormalized

sub, sub' :: Address -> Address -> Address -> Builder Address
sub   a b c  = emitInstr $ Sub  a b c Normalized
sub'  a b c  = emitInstr $ Sub  a b c UnNormalized

subE, subE' :: Address -> Address -> Address -> Builder Address
subE  a b c  = emitInstr $ SubE a b c Normalized
subE' a b c  = emitInstr $ SubE a b c UnNormalized

tExp, tExp' ::  Address -> Address -> Builder Address
tExp  a c    = emitInstr $ TExp a c   Normalized
tExp' a c    = emitInstr $ TExp a c   UnNormalized

tMod, tMod' ::  Address -> Address -> Builder Address
tMod  a c    = emitInstr $ TMod a c   Normalized
tMod' a c    = emitInstr $ TMod a c   UnNormalized

tSign, tSign' ::  Address -> Address -> Address -> Builder Address
tSign  a b c    = emitInstr $ TSign a b c   Normalized
tSign' a b c    = emitInstr $ TSign a b c   UnNormalized

tN, tN' ::  Address -> Address -> Builder Address
tN  a c      = emitInstr $ TN   a c   Normalized
tN' a c      = emitInstr $ TN   a c   UnNormalized

ai, ai' :: Address -> Address -> Address -> Builder Address
ai  a b c    = emitInstr $ AI a b c
ai' a b c    = emitInstr $ AICarry a b c

bitAnd :: Address -> Address -> Address -> Builder Address
bitAnd a b c = emitInstr $ LogMult a b c

{-
  Control-Flow

  The BESM has two primary mechanism for sub-routine calls.

  The first is the "Local Counter", it has two Instruction Counters and can switch between
  them with the help of the CLCC and JCC instructions.

  The second is using a Return-To-Control pattern. If an address (a) is stored in the second
  position of the CCCC instruction, the current central counter value will be written there
  before jumping to the CCCC target. Then the end of the subroutine contains two cells of like so:

    ┌────┬──────┬───┬───┐
    │ AI │ 110F │ a │ t │
    ├────┼──────┼───┼───┤
  t │ 0  │      │ 0 │ 0 │
    └────┴──────┴───┴───┘

  Typically, t is taken as cell a.

  This construction allows for a 'general' form of sub-routine calls.
-}

clcc ::  Address -> Builder Address
clcc addr   = emitInstr $ CLCC addr

jcc :: Builder Address
jcc         = emitInstr $ JCC

callRtc :: Address -> Address -> Builder Address
callRtc op retOp = emitInstr $ CallRTC (rtc retOp) op

chain ::  Address -> Builder Address
chain addr = emitTerm $ Chain addr

cccc ::  Address -> Builder Address
cccc addr = emitTerm $ CCCC addr

stop :: Builder Address
stop = emitTerm $ Stop

checkStop :: Builder Address
checkStop = emitTerm $ SwitchStop

retRTC :: Builder Address
retRTC = do
  addr <- gets (currentAddr . currentBlock)
  emitTerm $ RetRTC addr

{-
  The comp instruction checks if:

  1. Exp(a) = Exp(b) and Mant(a) < Mant(b)
  2. Exp(a) > Exp(b) and Mant(a) < 0
  3. Exp(a) < Exp(b) and Mant(b) > 0

  If any of these conditions are met, then instruction c is carried out
  otherwise we go to instruction d.

  Note: this corresponds to asking x < y if x and y are both normalized.

  Instruction D doesn't actually belong in the machine code, this is a construction
  which will either disappear by making D the following instruction so it flows naturally
  or a CCCC <d> to jump there if the conditional fails.
-}

comp :: Address -> Address -> Address -> Address -> Builder Address
comp a b c d = emitTerm $ Comp a b c d

{-
  If a and b are not bit-wise equal then go to c otherwise go to d

  Same as with comp, D is a meta-linguistic construct that will get compiled down into either
  zero or one instruction.
-}

compWord :: Address -> Address -> Address -> Address -> Builder Address
compWord a b c d = emitTerm $ CompWord a b c d

{-
  Compares the absolute value of a and b, executing c if:

  1. E(x) = E(y) and |M(x)| < |M(y)|
  2. E(x) < E(y) and |M(y)| != 0

  Otherwise d is carried out
-}

compMod :: Address -> Address -> Address -> Address -> Builder Address
compMod a b c d = emitTerm $ CompWord a b c d

{-
  Bit-Shifting

  The shift instruction will only shift bits within the mantissa, the exponent will always
  be set to 0.
-}

shift :: Address -> Address -> Address -> Builder Address
shift a b c = emitInstr $ Shift a b c

{-
  ShiftAll does a full word shift setting any overflow bits to zero.
-}
shift' :: Address -> Address -> Address -> Builder Address
shift' a b c = emitInstr $ ShiftAll a b c

{-
  The amount to be shifted by (m) is stored as m if the direction is to the left and as
  64 + m if the direction is to the right. These two helpers help distinguish between
  those cases and make it less confusing.
-}
left :: Int -> Address
left i = Absolute i

right :: Int -> Address
right i = Absolute (64 + i)

{-
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

cellA :: Address
cellA = Unknown "A"

cellB :: Address
cellB = Unknown "B"

cellA1 :: Address
cellA1 = Unknown "A + 1" -- A + 1

