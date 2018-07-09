{-# LANGUAGE DataKinds #-}
module Lower where

import qualified Syntax               as S

import           Data.BitVector.Sized
import           Data.List
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NL
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Word

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
  | NLParen Word8
  | Plus
  | Minus
  | RParen
  | NRParen Word8
  | AssignNoNormalize -- ?
  | Print -- F
  | Assign
  | Times
  | Colon
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
  | E
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
  = Cell { cName :: Text, val :: (BitVector 39) }
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

lowerVariableAddresses (S.VA blocks) = V
  { variableAddrs = map lowerVariableAddressBlock blocks
  , loopParameters = [] -- the high level AST doesnt even attempt to parse these!
  }
  where
  lowerVariableAddressBlock (S.Block size vars) = MainHead (toWord11 size) groupedHeads
    where
    groupedHeads = NL.fromList $ map (\group -> let
        [a, b, c] = take 3 $ (irSlopes $ NL.head group) ++ repeat 0
        in Head (toWord11 a) (toWord11 b) (toWord11 c) (NL.map (\(VAIR nm off _ vars) -> let
          [p1, p2, p3] = take 3 $ (map (Just . QA . T.singleton) vars) ++ repeat Nothing
          (dir, diff) =
            if off < 255 then (FromStart, fromIntegral off)
            else if off > (size - 255) then (FromEnd, fromIntegral $ size - off)
            else error "error"
          in VaInfo nm ( p1) ( p2) ( p3) diff dir
        ) group)
      ) groupedVars

    groupedVars = NL.groupBy (\a b -> irSlopes a == irSlopes b) . NL.sortBy (\a b -> irSlopes a `compare` irSlopes b) $ NL.map unpackVariableAddress vars


    splitConstant eq = (vars, toInt c)
      where (c, vars) = partition isConstant eq
            toInt [S.SConstant i] =  i
            toInt []              = 0
            toInt _               = error "omg no"

    unpackVariableAddress (name, eq) = VAIR name off slopes vars
      where ((slopes, vars), off) = xxx $ splitConstant $ unwrapVA eq

    isConstant (S.SConstant _) = True
    isConstant _               = False

    unwrapVA (S.SAdd l r) = unwrapVA l ++ unwrapVA r
    unwrapVA l            = [l]

    vaConstant (S.STimes (S.SConstant c) (S.SExpVar v)) = (c, v)

    xxx (vars, c) = (unzip $ map vaConstant vars, c)


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
lowerSchema (S.Stop) = [] -- ?????
lowerSchema (S.Semicolon) = []


lowerVariable = QA . S.unVar

lowerLogicalOperator :: S.Variable -> S.OperatorSign -> [(S.OperatorSign, S.Range)] -> LogicalOperator
lowerLogicalOperator var op ranges = Op
  { x = (lowerVariable var)
  , defaultOp = lowerOpSign op
  , choices = map lowerRange ranges
  }
  where
  lowerRange (nm, S.LeftImproperInterval      r) = (lowerOpSign nm, LeftImproper, lowerVariable r, Nothing)
  lowerRange (nm, S.LeftImproperSemiInterval  r) = undefined
  lowerRange (nm, S.RightImproperInterval     r) = undefined
  lowerRange (nm, S.RightImproperSemiInterval r) = undefined
  lowerRange (nm, S.Interval      l r) = undefined
  lowerRange (nm, S.SemiInterval  l r) = undefined
  lowerRange (nm, S.SemiSegment   l r) = undefined
  lowerRange (nm, S.Segment       l r) = undefined

lowerExp :: S.SchemaExpr -> [Operator]
lowerExp (S.Times l r) = lowerExp l ++ [Arith Times] ++ lowerExp r
lowerExp (S.Add   l r) = lowerExp l ++ [Arith Plus] ++ lowerExp r
lowerExp (S.Minus l r) = lowerExp l ++ [Arith Minus] ++ lowerExp r
lowerExp (S.Div   l r) = lowerExp l ++ [Arith Colon] ++ lowerExp r
lowerExp (S.Constant c) = [Parameter . QA $ T.pack $ show c]
lowerExp (S.ExpVar v) = [Parameter $ lowerVariable v]
lowerExp (S.Form v) = [Arith TransformToDecimal, Parameter (lowerVariable v)]

lowerConstants = map lowerConstant
  where lowerConstant (S.SConstant i) = Cell (T.pack $ show i) (bitVector $ fromIntegral i)
        lowerConstant (S.SExpVar v) = Vacant $ T.singleton v

