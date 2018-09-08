{-# LANGUAGE DataKinds #-}
module Besm.Lower where

import qualified Besm.Syntax               as S

import           Data.BitVector.Sized
import           Data.List
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NL
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Word
import           Data.Function (on)

{-
  """
  All information on the problem is always divided into the following four groups:

  1. Information on variable addresses.
  2. Information on parameters.
  3. A list of constants and variable quantities, termed below
     formally information on constants
  4. Information on the logical scheme or, as it will be also
     termed, information on the programme.
  """

  There are additional blocks of information that are introduced:

  - Alpha, Beta, Gamma: Related to coding loops
  - Zero (0): Reserved for 'standard-routines'

  This module converts a programme from the source level AST to a linear IR token stream
  which makes it simpler to encode into the binary input format that the BESM uses.

-}

data Programme
  = PP
  { variableAddresses :: BlockV
  , parameters        :: [ParameterInfo]
  , constants         :: [Constant]
  , programme         :: [Operator]
  , block0Len         :: Word16
  , blockAlphaLen     :: Word16
  , blockGammaLen     :: Word16
  -- , blockBetLen -- Beta block takes remaining space to addr 02FF
  } deriving Show

data ArithOperator -- Arithmetic, Logical, Non-Standard
  = LParen
  | NLParen Word8 -- N left parentheses, saves space for N > 2
  | Plus
  | Minus
  | RParen
  | NRParen Word8
  | AssignNoNormalize
  | Print
  | Assign
  | Times
  | Colon -- Division
  | Square
  | Cube
  | Cotan
  | Tan
  | Ln
  | SquareRoot
  | TransformToDecimal
  | Exp
  | ArcSin
  | ArcTan
  | Sin
  | Cos
  | E -- Exponent of FP number
  | ExtractExponenent
  | Mod
  | ChangeExponent Word8
  | ShiftMantissa Word8
  | Sign
  deriving Show

data Operator
  = Arith ArithOperator
  | Parameter Quantity
  | LogicalOperator LogicalOperator
  | OperatorSign OperatorSign -- should this be 8 bits? not 11?? ?? ? ? ? ?
  | LoopOpen Quantity
  | LoopClose
  deriving Show

newtype Quantity = QA { unQ :: Text } deriving Show
newtype OperatorSign = OS { getOperator :: BitVector 11 } deriving Show

data LogicalOperator = Op
  { x         :: Quantity
  , defaultOp :: OperatorSign
  , choices   :: [(OperatorSign, RangeType, Quantity, Maybe Quantity)]
  } deriving Show

data RangeType
  = LeftImproper
  | LeftImproperSemi
  | RightImproper
  | RightImproperSemi
  | Interval
  | SemiInterval
  | SemiSegment
  | Segment
  deriving Show

data BlockV
  = V
  { variableAddrs  :: [AddressBlock]
  , loopParameters :: [LoopParameter]
  } deriving Show

newtype Word11 = W { unWord11 :: BitVector 11 }
  deriving Show

data AddressBlock = MainHead
  { blockSize :: Word11
  , heads     :: NonEmpty BlockHead
  } deriving Show

data BlockHead
  = Head
  { a    :: Word11 -- 10 bits + sign bit
  , b    :: Word11
  , c    :: Word11
  , vars :: NonEmpty VariableAddress
  } deriving Show

data Dir = FromStart | FromEnd
  deriving Show

data VariableAddress = VaInfo
  { vaName    :: Text
  , param1    :: Maybe Quantity
  , param2    :: Maybe Quantity
  , param3    :: Maybe Quantity
  , offset    :: Word8
  , direction :: Dir
  } deriving Show

data LoopParameter = LP
  { lpName :: Text
  , i0     :: Quantity
  , lpA    :: Quantity
  , lpB    :: Quantity
  , j      :: Quantity
  , k      :: Quantity
  } deriving Show

newtype Opcode = OpC { getCode :: BitVector 6 } deriving Show
newtype Addr = Addr { unAddr :: BitVector 11 } deriving Show

data ParameterInfo
  = InFin
    { pName :: Text
    , inP   :: Quantity
    , finP  :: Quantity
    }
  | CharacteristicLoop
    { pName :: Text
    , theta :: Opcode
    , inP   :: Quantity
    , loopA :: Quantity
    , loopB :: Quantity
    }
  deriving Show

data Constant
  = Cell { cName :: Text, val :: Int }
  | Vacant { cName :: Text }
  deriving Show

toWord11 :: Int -> Word11
toWord11 i = W $ bitVector (fromIntegral i)

lowerOpSign :: S.OperatorSign -> OperatorSign
lowerOpSign = OS . unWord11 . toWord11 . S.fromOperatorSign

lowerProgramme :: S.ParsedProgramme -> Programme
lowerProgramme (S.P va p c k) = PP
  (lowerVariableAddresses va)
  (lowerParameters p)
  (lowerConstants c)
  (lowerSchema k)
  0 0 0

data VAIR = VAIR
  { irName   :: Text
  , irOffset :: Int
  , irSlopes :: [Int]
  , irVars   :: [Char]
  } deriving Show

lowerVariableAddresses :: S.VASection -> BlockV
lowerVariableAddresses (S.VA blocks) = V
  { variableAddrs = map lowerVariableAddressBlock blocks
  , loopParameters = [] -- the high level AST doesnt even attempt to parse these!
  }
  where
  lowerVariableAddressBlock (S.Block size vars) = MainHead (toWord11 size) lowerHeads
    where
    lowerHeads = NL.fromList $ map lowerHead groupedVars

    lowerHead group = let
      [a, b, c] = take 3 $ (irSlopes $ NL.head group) ++ repeat 0
      in Head (toWord11 a) (toWord11 b) (toWord11 c) (NL.map lowerVariableAddress group)

    lowerVariableAddress (VAIR nm off _ vars) = let
        [p1, p2, p3] = take 3 $ (map (Just . QA . T.singleton) vars) ++ repeat Nothing
        (dir, diff) =
          if off < 255 then (FromStart, fromIntegral off)
          else if off > (size - 255) then (FromEnd, fromIntegral $ size - off)
          else error "error"
        in VaInfo nm p1 p2 p3 diff dir

    {-
      Take the list of variable addresses m_i, i ∈ {0,.., n}, of a block M_(k) each having the form:

      m_i = A_i * x_i + B_i * y_i + C_i * z_i + D

      and group them according to the tuple (A,B,C) producing sublists with all variables that share
      the same constant parameters.
    -}

    groupedVars :: [NonEmpty VAIR]
    groupedVars =
        NL.groupBy ((==) `on` irSlopes)
      . NL.sortBy (compare `on` irSlopes)
      $ NL.map unpackVariableAddress vars

    splitConstant eq = (vars, toInt c)
      where (c, vars) = partition isConstant eq
            toInt [S.SConstant i] =  i
            toInt []              = 0
            toInt _               = error "omg no"
            isConstant (S.SConstant _) = True
            isConstant _               = False

    unpackVariableAddress (name, eq) = VAIR name off slopes vars
      where
      ((slopes, vars), off) = unzipLeft $ splitConstant $ unwrapVA eq
      unzipLeft (vars, c) = (unzip $ map vaConstant vars, c)
      vaConstant (S.STimes (S.SConstant c) (S.SExpVar v)) = (c, v)


    unwrapVA (S.SAdd l r) = unwrapVA l ++ unwrapVA r
    unwrapVA l            = [l]


lowerParameters ps = map lowerParameter ps
  where
  lowerParameter (S.InFin var init fin) = InFin (S.unVar var) (QA . T.pack $ show init) (QA . T.pack $ show fin)
  lowerParameter (S.Charateristic var op init a b) = undefined

lowerSchema :: S.LogicalSchema -> [Operator]
lowerSchema (S.Loop var ls) = LoopOpen (QA $ T.singleton var) : lowerSchema ls ++ [LoopClose]
lowerSchema (S.Seq lss) = concatMap lowerSchema lss
lowerSchema (S.Assign exp var) = lowerExp exp ++ [Arith Assign, Parameter (lowerVariable var)]
lowerSchema (S.LogicalOperator var op ranges) = [LogicalOperator (lowerLogicalOperator var op ranges)]
lowerSchema (S.OpLabel op) = [OperatorSign (lowerOpSign op)]
lowerSchema (S.Print exp) = lowerExp exp ++ [Arith Print]
-- I havent found any information on how Stop is coded so I assume it's equivalent to
-- and end-of-input-marker
lowerSchema (S.Stop) = []
-- ; is a purely syntactic construct that is parsed solely for prettyprinting purposes
lowerSchema (S.Semicolon) = []

lowerVariable = QA . S.unVar

lowerQuantity (S.C c) = QA . T.pack $ show c
lowerQuantity (S.V v) = lowerVariable v

lowerLogicalOperator :: S.Quantity -> S.OperatorSign -> [(S.OperatorSign, S.Range)] -> LogicalOperator
lowerLogicalOperator var op ranges = Op
  { x = (lowerQuantity var)
  , defaultOp = lowerOpSign op
  , choices = map lowerRange ranges
  }
  where
  lowerRange (nm, S.LeftImproperInterval      r) = (lowerOpSign nm, LeftImproper, lowerQuantity r, Nothing)
  lowerRange (nm, S.LeftImproperSemiInterval  r) = error "LeftImproperSemiInterval not yet implemented."
  lowerRange (nm, S.RightImproperInterval     r) = error "RightImproperInterval not yet implemented."
  lowerRange (nm, S.RightImproperSemiInterval r) = error "RightImproperSemiInterval not yet implemented."
  lowerRange (nm, S.Interval      l r) = error "Interval not yet implemented."
  lowerRange (nm, S.SemiInterval  l r) = error "SemiInterval not yet implemented."
  lowerRange (nm, S.SemiSegment   l r) = error "SemiSegment not yet implemented."
  lowerRange (nm, S.Segment       l r) = error "Segment not yet implemented."

lowerExp :: S.SchemaExpr -> [Operator]
lowerExp (S.Times l r) = lowerExp l ++ [Arith Times] ++ lowerExp r
lowerExp (S.Add   l r) = lowerExp l ++ [Arith Plus] ++ lowerExp r
lowerExp (S.Minus l r) = lowerExp l ++ [Arith Minus] ++ lowerExp r
lowerExp (S.Div   l r) = lowerExp l ++ [Arith Colon] ++ lowerExp r
lowerExp (S.Primitive var) = [ Parameter $ lowerQuantity var ]
lowerExp (S.Form v) = [Arith TransformToDecimal, Parameter (lowerQuantity v)]



lowerConstants = map lowerConstant
  where lowerConstant (S.SConstant i) = Cell (T.pack $ show i) i
        lowerConstant (S.SExpVar v) = Vacant $ T.singleton v

