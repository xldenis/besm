{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Besm.Parser
import Besm.Syntax as S
import Data.Char
import Data.Foldable
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Data.Either

import Control.Applicative
import Control.Monad (forM, when)
import Data.Bifunctor (first)

main :: IO Bool
main = checkSequential $$(discover)

prop_parameter = withTests 1000 $ parse_and_pretty (forAll genParameter) parameterDecl prettyParameter

prop_block = withTests 1000 $ do
  parse_and_pretty (forAll $ genBlock "abc") variableAddressBlock (prettyVABlock 'a')

prop_prog = withTests 1000 $ do
  parse_and_pretty (forAll genProgramme) pp prettyProgramme

parse_and_pretty :: (Show a, Eq a) => PropertyT IO a -> Parser a -> (a -> Doc a) -> Property
parse_and_pretty propGen parser pretty =
  property $ do
    gened <- propGen

    tripping gened (render . pretty) (parse' parser)
 where
  render = renderStrict . layoutPretty defaultLayoutOptions
  parse' p = first parseErrorPretty . parse p ""

genParameter :: MonadGen m => m Parameter
genParameter =
  Gen.choice
    [ InFin <$> genVariable <*> Gen.int range <*> Gen.int range
    , Characteristic <$> genVariable <*> genComp <*> Gen.int range <*> genVariable <*> genVariable
    ]
 where
  genComp = Gen.choice [pure Comparison, pure WordComparison, pure ModuliComparison]

  range = Range.linear 0 maxBound

letterChar :: MonadGen m => m Char
letterChar = Gen.filter isBesmLetter (Gen.enum '\0' '\1103')

genVariable :: MonadGen m => m Variable
genVariable = S.Var <$> letterChar <*> pure []

genBlock :: (Alternative m, MonadGen m) => [Char] -> m Block
genBlock subscripts = do
  numCells <- Gen.int (Range.linear 0 300)

  blockVar <- Gen.alpha
  equations <- Gen.nonEmpty (Range.linear 1 10) (genEquation blockVar subscripts)

  return $ Block numCells equations

genEquation :: (Alternative m, MonadGen m) => Char -> [Char] -> m (Text, SimpleExpr Char)
genEquation var subscripts = do
  expr <- genExpr subscripts

  let usedSubs = toList expr <|> constantSub expr
  return (pack (var : '_' : usedSubs), expr)
 where
  constantSub (SConstant i) = show i
  constantSub _ = []

genExpr :: (Applicative m, MonadGen m) => [Char] -> m (SimpleExpr Char)
genExpr vars = do
  usedVars <- take <$> Gen.int (Range.linear 0 3) <*> Gen.shuffle vars

  elems <-
    forM
      usedVars
      ( \var ->
          Gen.choice
            [ pure $ SExpVar var
            , STimes <$> genSimpleConstant <*> pure (SExpVar var)
            ]
      )

  constant <-
    if null elems
      then pure <$> genSimpleConstant
      else Gen.list (Range.linear 0 1) genSimpleConstant
  pure $ foldl1 SAdd (elems ++ constant)

genSimpleConstant :: MonadGen m => m (SimpleExpr a)
genSimpleConstant = SConstant <$> Gen.int (Range.linear 0 100)

genProgramme :: (Alternative m, MonadGen m) => m ParsedProgramme
genProgramme = do
  parameters <- Gen.list (Range.linear 1 10) genParameter

  let paramVars = map (var . paramVar) parameters

  va <- VA <$> Gen.list (Range.linear 0 3) (genBlock paramVars)
  constants <- Gen.list (Range.linear 1 10) (genSimpleConstant <|> SExpVar <$> letterChar)
  let (cvals, vars) = partitionEithers $ map splitConstants constants

  prog <- genSchemeElem vars cvals
  return $
    P
      va
      parameters
      constants
      (Seq [JCC])
 where
  splitConstants (SConstant i) = Left i
  splitConstants (SExpVar c) = Right (S.Var c [])

genSchemeElem :: MonadGen m => [Variable] -> [Int] -> m LogicalSchema
genSchemeElem vars constants = do
  Gen.choice
    [ pure JCC
    , Print <$> genSchemeExpr vars constants
    ]

genSchemeExpr :: MonadGen m => [Variable] -> [Int] -> m SchemaExpr
genSchemeExpr vars constants = do
  let qtities = map V vars ++ map C constants
  Gen.recursive
    Gen.choice
    [ Primitive <$> Gen.element qtities
    , Form <$> Gen.element qtities
    ]
    []
