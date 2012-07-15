{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main, gain) where

import Graphics.Blobs.Colors
import Graphics.Blobs.Document
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Math
import Graphics.Blobs.Operations
import Graphics.Blobs.Shape
import Graphics.Blobs.VH.Loader
import Graphics.Blobs.VH.Types
import Graphics.Blobs.VH.UI
import Graphics.UI.WX
import qualified Graphics.Blobs.NetworkUI as NetworkUI
import qualified Graphics.Blobs.Palette as P
import qualified Graphics.Blobs.State as State

-- ---------------------------------------------------------------------

-- In this implementation
-- g = ()
-- n = VhNode
-- e = [VhFlow]

main :: IO ()
main = start $
  do{ state <- State.empty
    ; NetworkUI.create state emptyGlobal -- global state is a list of possible flows
                             undefined	 -- dummy node state (for typechecker)
                             undefined	 -- dummy edge state (for typechecker)
                             undefined   -- dummy network config (for typechecker)
                             palettes    -- default palette
                             graphOps	 -- operations available from menu
    }


instance InfoKind VhNode VhGlobal where
    blank = VhProcess
    check _n _ _i = []

instance GuiGlobalEdit VhNode VhGlobal where
  editDialogWithGlobal parentWindow dialogTitle initial global = editNodeDialog parentWindow dialogTitle initial global


instance GuiEdit VhGlobal where
  editDialog = editGlobalDialog

instance InfoKind [VhFlow] VhGlobal where
    blank = []
    check _n _ _i = []

instance GuiGlobalEdit [VhFlow] VhGlobal where
  editDialogWithGlobal parentWindow dialogTitle initial global = editFlowDialog parentWindow dialogTitle initial global

instance Descriptor VhGlobal where
  descriptor g = "flows:"  ++ descriptor (flows g)

instance Descriptor VhNode where
  descriptor VhExternal = "External"
  descriptor VhProcess  = "Process"
  descriptor VhStore    = "Store"
  descriptor VhPortIn   = "In"
  descriptor VhPortOut  = "Out"

instance Descriptor [VhFlow] where
  descriptor xs = show $ map (\(VhFlow s) -> s) xs

instance NetworkConfig () where
  prohibitDoubleEdges _ = False
  prohibitReverseEdges _ = False

-- GraphOps g n e c
graphOps :: GraphOps VhGlobal VhNode [VhFlow] ()
graphOps = GraphOps { ioOps = map pureGraphOp
                                  [  ] }


{-
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

-- ---------------------------------------------------------------------

palettes :: [(PaletteId, P.Palette VhNode)]
palettes = [(toPaletteId "default",palette)
           ]

palette :: P.Palette VhNode
palette =   P.Palette
  [ ("Process"
    , ( Circle  { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 200 128
                                        }
              , shapeRadius = 0.5 }
      , Just VhProcess ))
  , ("External"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 2
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 200 128 200
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5
                                 , DoublePoint (-0.5) 0.5 ] }
      , Just VhExternal ))
  , ("Store"
    , ( Composite { shapeSegments =
                    [ Lines { shapeStyle = ShapeStyle
                                               { styleStrokeWidth = 2
                                               , styleStrokeColour = RGB 0 0 0
                                               , styleFill = RGB 128 128 128
                                               }
                            , shapePerimeter = [ DoublePoint (-0.6) (-0.4)
                                               , DoublePoint  0.6 (-0.4) ] }
                    , Lines { shapeStyle = ShapeStyle
                                               { styleStrokeWidth = 2
                                               , styleStrokeColour = RGB 0 0 0
                                               , styleFill = RGB 128 128 128
                                               }
                            , shapePerimeter = [ DoublePoint (-0.6) 0.4
                                               , DoublePoint  0.6 0.4 ] }
                    ] }
      , Just VhStore ))
  , ("In"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 200 200
                                        }
                , shapePerimeter = [ DoublePoint (-0.2) (-0.2)
                                   , DoublePoint (-0.2) ( 0.2)
                                   , DoublePoint ( 0.2) ( 0.0) ] }
      , Just VhPortIn ))

  , ("Out"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                          , styleStrokeColour = RGB 0 0 0
                                          , styleFill = RGB 128 200 200
                                        }
                , shapePerimeter = [ DoublePoint (-0.2) ( 0.0)
                                   , DoublePoint ( 0.2) (-0.2)
                                   , DoublePoint ( 0.2) ( 0.2) ] }
      , Just VhPortOut ))
  ]

-- ---------------------------------------------------------------------

foo = example "./src/Vh.hs"

-- EOF
