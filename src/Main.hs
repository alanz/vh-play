{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main, gain) where

import qualified Graphics.Blobs.NetworkUI as NetworkUI
import Graphics.UI.WX
import qualified Graphics.Blobs.State as State
import Graphics.Blobs.InfoKind
import Text.Parse

import Text.XML.HaXml.Types
import qualified Text.XML.HaXml.XmlContent.Haskell as XML
import List(isPrefixOf)

import Graphics.Blobs.Network
import Graphics.Blobs.Operations
import qualified Data.IntMap as IntMap
import List (nub)
import Maybe (fromJust)

-- In this implementation
-- g = ()
-- n = Int
-- e = [Int]

main :: IO ()
main = start $
  do{ state <- State.empty
    ; NetworkUI.create state ()		-- global state is just the unit value
                             undefined	-- dummy node state (for typechecker)
                             undefined	-- dummy edge state (for typechecker)
                             graphOps	-- operations available from menu
    }


data DfdNode = DfdExternal | DfdProcess | DfdStore deriving (Show,Eq)

instance Parse DfdNode where
    parse = oneOf
            [ do { isWord "DfdExternal"
               ; return DfdExternal
               }
            , do { isWord "DfdProcess"
               ; return DfdProcess
               }
            , do { isWord "DfdStore"
               ; return DfdStore
               }
            ]


instance InfoKind DfdNode () where
    blank = DfdProcess
    check n _ i = []


instance XML.HTypeable DfdNode where
    toHType v = XML.Defined "DfdNode" []
                    [ XML.Constr "DfdExternal" [] []
                    , XML.Constr "DfdProcess"  [] []
                    , XML.Constr "DfdStore"    [] []
                    ]

instance XML.XmlContent DfdNode where
    parseContents = do
        { e@(Elem t _ _) <- XML.element  ["DfdExternal","DfdProcess","DfdStore"]
        ; case t of
          _ | "DfdExternal" `isPrefixOf` t -> XML.interior e $
                do { return (DfdExternal)
                   }
            | "DfdProcess" `isPrefixOf` t -> XML.interior e $
                do { return (DfdProcess)
                   }
            | "DfdStore" `isPrefixOf` t -> XML.interior e $
                do { return (DfdStore)
                   }
        }
    toContents v@(DfdExternal) =
        [XML.mkElemC (XML.showConstr 0 (XML.toHType v)) []]
    toContents v@(DfdProcess) =
        [XML.mkElemC (XML.showConstr 1 (XML.toHType v)) []]
    toContents v@(DfdStore) =
        [XML.mkElemC (XML.showConstr 2 (XML.toHType v)) []]


-- GraphOps g n e
graphOps :: GraphOps () DfdNode ()
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
