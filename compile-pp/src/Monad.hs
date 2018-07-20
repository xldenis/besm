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

runBuilder i = getSnocList . builtBlocks . flip execState (BuilderState (emptyCurrentBlock i) (SnocList [])) . unBuilder

modifyBlock f = do
  modify $ \s -> s { currentBlock = f (currentBlock s) }

emitInstr :: Instruction -> Builder Address
emitInstr instr = do
  modifyBlock $ \bb -> bb
    { currentInstrs = currentInstrs bb `snoc` instr
    , currentAddr = offAddr (currentAddr bb) 1
    }
  gets (currentAddr . currentBlock)

emitTerm :: Terminator -> Builder Address
emitTerm term = do
  bb <- gets currentBlock

  let basicBlock = BB
        { instrs      = unSnocList $ currentInstrs bb
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


add   a b c  = emitInstr $ Add  a b c Normalized
add'  a b c  = emitInstr $ Add  a b c UnNormalized

addE  a b c  = emitInstr $ AddE a b c Normalized
addE' a b c  = emitInstr $ AddE a b c UnNormalized

ce    a b c  = emitInstr $ Ce   a b c Normalized
ce'   a b c  = emitInstr $ Ce   a b c UnNormalized

div   a b c  = emitInstr $ Div  a b c Normalized
div'  a b c  = emitInstr $ Div  a b c UnNormalized

mult  a b c  = emitInstr $ Mult a b c Normalized
mult' a b c  = emitInstr $ Mult a b c UnNormalized

sub   a b c  = emitInstr $ Sub  a b c Normalized
sub'  a b c  = emitInstr $ Sub  a b c UnNormalized

subE  a b c  = emitInstr $ SubE a b c Normalized
subE' a b c  = emitInstr $ SubE a b c UnNormalized

tExp  a c    = emitInstr $ TExp a c   Normalized
tExp' a c    = emitInstr $ TExp a c   UnNormalized

tMod  a c    = emitInstr $ TMod a c   Normalized
tMod' a c    = emitInstr $ TMod a c   UnNormalized

tN  a c      = emitInstr $ TN   a c   Normalized
tN' a c      = emitInstr $ TN   a c   UnNormalized

ai a b c     = emitInstr $ AI a b c
bitAnd a b c = emitInstr $ LogMult a b c

jcc         = emitInstr $ JCC
callRtc op  = emitInstr $ CallRTC (rtc op) op
shift a b c = emitInstr $ Shift a b c
clcc addr   = emitInstr $ CLCC addr

cccc addr = emitTerm $ CCCC addr
comp a b c d = emitTerm $ Comp a b c d
compWord a b c d = emitTerm $ CompWord a b c d
chain addr = emitTerm $ Chain addr
stop = emitTerm $ Stop
checkStop = emitTerm $ SwitchStop
