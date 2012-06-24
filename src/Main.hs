{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main, gain) where

import Graphics.Blobs.CommonIO
import Graphics.Blobs.Dfd.Types
import Graphics.Blobs.Dfd.UI
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Operations
import Graphics.Blobs.NetworkFile
import Graphics.UI.WX
import qualified Graphics.Blobs.NetworkUI as NetworkUI
import qualified Graphics.Blobs.State as State

-- ---------------------------------------------------------------------

-- In this implementation
-- g = ()
-- n = DfdNode
-- e = [DfdFlow]

main :: IO ()
main = start $
  do{ state <- State.empty
    ; NetworkUI.create state emptyGlobal -- global state is a list of possible flows
                             undefined	 -- dummy node state (for typechecker)
                             undefined	 -- dummy edge state (for typechecker)
                             graphOps	 -- operations available from menu
    }


instance InfoKind DfdNode DfdGlobal where
    blank = DfdProcess
    check _n _ _i = []

instance GuiGlobalEdit DfdNode DfdGlobal where
  editDialogWithGlobal parentWindow dialogTitle initial global = editNodeDialog parentWindow dialogTitle initial global


instance GuiEdit DfdGlobal where
  editDialog = editGlobalDialog

instance InfoKind [DfdFlow] DfdGlobal where
    blank = []
    check _n _ _i = []

instance GuiGlobalEdit [DfdFlow] DfdGlobal where
  editDialogWithGlobal parentWindow dialogTitle initial global = editFlowDialog parentWindow dialogTitle initial global

instance Descriptor DfdGlobal where
  descriptor g = "flows:"  ++ descriptor (flows g)

instance Descriptor DfdNode where
  descriptor DfdExternal = "External"
  descriptor DfdProcess  = "Process"
  descriptor DfdStore    = "Store"

instance Descriptor [DfdFlow] where
  descriptor xs = show $ map (\(DfdFlow s) -> s) xs

-- GraphOps g n e
graphOps :: GraphOps DfdGlobal DfdNode [DfdFlow]
graphOps = GraphOps { ioOps = map pureGraphOp
                                  [  ] }

{-
-- Some basic kinds of info to store in the nodes/edges
instance InfoKind Int () where
    blank = 0
    check n _ i | i<0 = ["Number should not be negative in "++n]
                | otherwise = []
instance InfoKind [Int] () where
    blank = []
    check _ _ _ = []

-- A simple range of operations on a graph network.
graphOps :: GraphOps () [Int] [Int]
graphOps = GraphOps { ioOps = map pureGraphOp
                                  [ ("push numbers one step", onePush)
                                  , ("clear all numbers", revert) ] }
  where
    onePush (g, nodemap, edgemap) =
            (g, IntMap.mapWithKey (\k v-> (edgemap `accumulateIn` k) v) nodemap
              , IntMap.map (\e-> nodemap `pushAlongEdge` e) edgemap)
    revert  (g, nodemap, edgemap) =
            (g, IntMap.map (setInfo blank) nodemap
              , IntMap.map (setEdgeInfo blank) edgemap)

-- Every edge is augmented with the sum of the numbers in its from-node.
pushAlongEdge :: IntMap.IntMap (Node [Int]) -> Edge [Int] -> Edge [Int]
nodemap `pushAlongEdge` edge = setEdgeInfo (nub (sum n: getEdgeInfo edge)) edge
  where n = (getInfo . fromJust . flip IntMap.lookup nodemap . getEdgeFrom)
            edge

-- Every node is augmented with a list of all the numbers in its incoming edges.
accumulateIn :: IntMap.IntMap (Edge [Int]) -> NodeNr -> Node [Int] -> Node [Int]
(edgemap `accumulateIn` nr) node = setInfo (nub (es++getInfo node)) node
  where es = (concat . IntMap.elems
             . IntMap.map getEdgeInfo
             . IntMap.filter (\e-> getEdgeTo e == nr) )
             edgemap
-}

gain :: IO ()
gain = main -- :-)

{-
foo = do
  fc <- readFile "./f.blobs"
  case fromStringAssocs fc of
    Left str -> return str
    Right (assocs, errs, b) -> return $ "Right " ++ (show assocs) ++ "," ++ (show errs) ++ "," ++ (show b)
-}