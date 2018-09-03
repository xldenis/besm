{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Monad where

import Syntax
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

runProcedure nm bbs = Proc (nm, runBuilder (op 999) bbs)

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

tN, tN' ::  Address -> Address -> Builder Address
tN  a c      = emitInstr $ TN   a c   Normalized
tN' a c      = emitInstr $ TN   a c   UnNormalized

ai, ai' :: Address -> Address -> Address -> Builder Address
ai a b c     = emitInstr $ AI a b c
ai' a b c    = emitInstr $ AICarry a b c

bitAnd :: Address -> Address -> Address -> Builder Address
bitAnd a b c = emitInstr $ LogMult a b c

jcc :: Builder Address
jcc         = emitInstr $ JCC

callRtc :: Address -> Address -> Builder Address
callRtc op retOp = emitInstr $ CallRTC (rtc retOp) op

shift :: Address -> Address -> Address -> Builder Address
shift a b c = emitInstr $ Shift a b c

clcc ::  Address -> Builder Address
clcc addr   = emitInstr $ CLCC addr

readMD :: Int -> Address -> Address -> Address -> Builder Address
readMD n n1 n2 a = (emitInstr $ Ma (Absolute $ 0x100 + n) n1 a) >> (emitInstr $ Mb n2)

ma :: Address -> Address -> Address -> Builder Address
ma n n1 a = emitInstr $ Ma n n1 a

mb ::  Address -> Builder Address
mb n2 = emitInstr $ Mb n2

comp :: Address -> Address -> Address -> Address -> Builder Address
comp a b c d = emitTerm $ Comp a b c d

compWord :: Address -> Address -> Address -> Address -> Builder Address
compWord a b c d = emitTerm $ CompWord a b c d

chain ::  Address -> Builder Address
chain addr = emitTerm $ Chain addr

cccc ::  Address -> Builder Address
cccc addr = emitTerm $ CCCC addr

stop :: Builder Address
stop = emitTerm $ Stop

checkStop :: Builder Address
checkStop = emitTerm $ SwitchStop

retRTC = do
  addr <- gets (currentAddr . currentBlock)
  emitTerm $ RetRTC addr

cellA :: Address
cellA = Unknown "A"

cellB :: Address
cellB = Unknown "B"

cellA1 :: Address
cellA1 = Unknown "A + 1" -- A + 1

