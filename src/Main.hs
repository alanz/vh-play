{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main, gain) where

import Graphics.Blobs.Colors
import Graphics.Blobs.Document
import Graphics.Blobs.Dfd.Types
import Graphics.Blobs.Dfd.UI
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Math
import Graphics.Blobs.Operations
import Graphics.Blobs.Shape
import Graphics.UI.WX
import qualified Graphics.Blobs.NetworkUI as NetworkUI
import qualified Graphics.Blobs.Palette as P
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
                             undefined   -- dummy network config (for typechecker)
                             palettes    -- default palette
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
  descriptor DfdPortIn   = "In"
  descriptor DfdPortOut  = "Out"

instance Descriptor [DfdFlow] where
  descriptor xs = show $ map (\(DfdFlow s) -> s) xs

instance NetworkConfig () where
  prohibitDoubleEdges _ = False
  prohibitReverseEdges _ = False

-- GraphOps g n e c
graphOps :: GraphOps DfdGlobal DfdNode [DfdFlow] ()
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

palettes :: [(PaletteId, P.Palette DfdNode)]
palettes = [(toPaletteId "context",contextPalette)
           ,(toPaletteId "dfd",    dfdPalette)
            ]


contextPalette :: P.Palette DfdNode
contextPalette = P.Palette
  [ paletteEntryProcess
  , paletteEntryExternal
  ]

dfdPalette :: P.Palette DfdNode
dfdPalette = P.Palette
  [ paletteEntryProcess
  , paletteEntryStore
  , paletteEntryIn
  , paletteEntryOut
  ]


-- ---------------------------------------------------------------------

paletteEntryProcess :: (String, (Shape, Maybe DfdNode))
paletteEntryProcess =
   ("Process"
    , ( Circle  { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 200 128
                                        }
              , shapeRadius = 0.5 }
      , Just DfdProcess ))



paletteEntryExternal :: (String, (Shape, Maybe DfdNode))
paletteEntryExternal =
  ("External"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 2
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 200 128 200
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5
                                 , DoublePoint (-0.5) 0.5 ] }
      , Just DfdExternal ))

paletteEntryStore :: (String, (Shape, Maybe DfdNode))
paletteEntryStore =
  ("Store"
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
      , Just DfdStore ))

paletteEntryIn :: (String, (Shape, Maybe DfdNode))
paletteEntryIn =
  ("In"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 200 200
                                        }
                , shapePerimeter = [ DoublePoint (-0.2) (-0.2)
                                   , DoublePoint (-0.2) ( 0.2)
                                   , DoublePoint ( 0.2) ( 0.0) ] }
      , Just DfdPortIn ))

paletteEntryOut :: (String, (Shape, Maybe DfdNode))
paletteEntryOut =
  ("Out"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                          , styleStrokeColour = RGB 0 0 0
                                          , styleFill = RGB 128 200 200
                                        }
                , shapePerimeter = [ DoublePoint (-0.2) ( 0.0)
                                   , DoublePoint ( 0.2) (-0.2)
                                   , DoublePoint ( 0.2) ( 0.2) ] }
      , Just DfdPortOut ))


