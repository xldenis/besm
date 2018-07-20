module Syntax where

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
