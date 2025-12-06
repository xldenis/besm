module Besm.Assembler.CFG where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List

import Data.Maybe
import Data.Tuple (swap)

programmeToGraph :: [BasicBlock] -> Gr Address ()
programmeToGraph bbs =
  let
    graphInfo = map blockToGraphInfo bbs
    nodeLabels = nub $ concatMap (uncurry (:)) graphInfo
    nodeDict = zip nodeLabels [1 ..]
    (nodes, edges) = unzip $ map (toNodes nodeDict) graphInfo
   in
    mkGraph (map swap nodeDict) (concat edges)
 where
  toNodes dict (i, edgeI) = case i `lookup` dict of
    Just ix ->
      let
        node = (ix, i)
        edges = map (\e -> (ix, fromJust $ e `lookup` dict, ())) edgeI
       in
        (node, edges)
    Nothing -> error "you dun goofed"

blockToGraphInfo :: BasicBlock -> (Address, [Address])
blockToGraphInfo (BB _ t nm _) =
  (nm, termToAddrs t)

termToAddrs :: Terminator -> [Address]
termToAddrs (Comp _ _ t f) = [t, f]
termToAddrs (CompWord _ _ t f) = [t, f]
termToAddrs (CompMod _ _ t f) = [t, f]
termToAddrs (CCCC blk) = [blk]
termToAddrs (CCCCSnd _ blk) = [blk]
termToAddrs (Chain blk) = [blk]
termToAddrs (Stop _ _ _ _) = []
termToAddrs SwitchStop = []
termToAddrs (RetRTC _) = []
