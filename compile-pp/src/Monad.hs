{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Monad where

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

data Address
  = Operator Int
  | Offset Address Int
  | Absolute Int
  | Procedure String
  | Unknown
  deriving (Show, Eq)

formatAddr (Operator i) = "op. " ++ show i
formatAddr (Offset a i) = formatAddr a ++ " + " ++ show i
formatAddr (Absolute i) = "abs. " ++ show i
formatAddr (Procedure s) = show s
formatAddr (Unknown) = "unknown"

offAddr (Offset a o) i = Offset a (o + i)
offAddr a i = Offset a i

op = Operator

rtc = undefined

data BasicBlock = BB
  { instrs      :: [Instruction]
  , terminator  :: Terminator
  , baseAddress :: Address
  } deriving (Show)

data NormalizeResult
  = Normalized
  | UnNormalized
  deriving (Show, Eq)

data Instruction
  = Add       Address Address Address NormalizeResult
  | Sub       Address Address Address NormalizeResult
  | Mult      Address Address Address NormalizeResult
  | Div       Address Address Address NormalizeResult
  | AddE      Address Address Address NormalizeResult
  | SubE      Address Address Address NormalizeResult
  | Ce        Address Address Address NormalizeResult
  | Xa        Address Address Address NormalizeResult
  | Xb                        Address NormalizeResult
  | DivA      Address Address Address NormalizeResult
  | DivB                      Address NormalizeResult
  | TN        Address         Address NormalizeResult
  | PN        Address
  | TMin      Address         Address NormalizeResult
  | TMod      Address         Address NormalizeResult -- I think the 'modulus' actually means magnitude of mantissa? might actually mean the sexadecimal width of the number??? wtf...
  | TSign     Address Address Address NormalizeResult
  | TExp      Address         Address NormalizeResult
  | Shift     Address Address Address
  | ShiftAll  Address Address Address
  | AI        Address Address Address
  | AICarry   Address Address Address
  | I         Address Address Address
  | Ma        Address Address Address
  | Mb        Address Address Address
  | LogMult   Address Address Address
  | CallRTC           Address Address
  | CLCC                      Address
  | JCC
  deriving (Show, Eq)

data Terminator
  = Comp      Address Address Address Address
  | CompWord  Address Address Address Address
  | CompMod   Address Address Address Address
  | CCCC                      Address
  | CCCCSnd           Address Address
  | Stop
  | SwitchStop
  | Chain Address -- meta-linguistic, chains two basic blocks together
  deriving (Show)

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

tN a  c      = emitInstr $ TN   a c   Normalized
tN' a c      = emitInstr $ TN   a c   UnNormalized

jcc         = emitInstr $ JCC
callRtc op  = emitInstr $ CallRTC (rtc op) op
shift a b c = emitInstr $ Shift a b c
clcc addr   = emitInstr $ CLCC addr



cccc addr = emitTerm $ CCCC addr
comp a b c d = emitTerm $ Comp a b c d
compWord a b c d = emitTerm $ CompWord a b c d
chain addr = emitTerm $ Chain addr
stop = emitTerm $ Stop
