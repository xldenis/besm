{-# LANGUAGE DeriveFunctor #-}
module Besm.Syntax.NonStandard where

{-
  This module contains a definition of all non-standard operators (aka machine code)
-}

data NormalizeResult
  = Normalized
  | UnNormalized
  deriving (Show, Eq)


data NonStandardOpcode
  = Add       NormalizeResult
  | Sub       NormalizeResult
  | Mult      NormalizeResult
  | Div       NormalizeResult
  | AddE      NormalizeResult
  | SubE      NormalizeResult
  | Ce        NormalizeResult
  | Xa        NormalizeResult
  | Xb        NormalizeResult
  | DivA      NormalizeResult
  | DivB      NormalizeResult
  | TN        NormalizeResult
  | PN
  | TMin      NormalizeResult
  | TMod      NormalizeResult -- I think the 'modulus' actually means magnitude of mantissa? might actually mean the sexadecimal width of the number??? wtf...
  | TSign     NormalizeResult
  | TExp      NormalizeResult
  | Shift
  | ShiftAll
  | AI
  | AICarry
  | I
  | Ma
  | Mb
  | LogMult
  | CLCC
  | JCC
  | Comp
  | CompWord
  | CompMod
  | CCCC
  | CCCCSnd
  | Stop
  | SwitchStop
  deriving (Show, Eq)


