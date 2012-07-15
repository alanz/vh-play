{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main, gain) where

import qualified Data.Text as T
import Graphics.Blobs.Colors
import Graphics.Blobs.Document
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Math
import Graphics.Blobs.Network
import Graphics.Blobs.Operations
import Graphics.Blobs.Shape
import Graphics.Blobs.State
import Graphics.Blobs.VH.Loader
import Graphics.Blobs.VH.Types
import Graphics.Blobs.VH.UI
import Graphics.UI.WX
import qualified Data.IntMap as IntMap
import qualified Graphics.Blobs.NetworkUI as NetworkUI
import qualified Graphics.Blobs.Palette as P
import qualified Graphics.Blobs.PersistentDocument as PD
import qualified Graphics.Blobs.State as State
import Language.Haskell.BuildWrapper.Base

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
graphOps = GraphOps { ioOps = (myGraphOp ("Load file",loadOp)) : (map pureGraphOp
                                  [ ("change the network", changeNet)  ]) }

changeNet  (g, nodemap, edgemap) =
  (g, getIt, edgemap)

getIt :: IntMap.IntMap (Node VhNode)
getIt = IntMap.fromList [(1, constructNode "Name" (DoublePoint 7 5) True (Left "External") VhExternal Nothing )]

-- ---------------------------------------------------------------------

testOp :: (g, IntMap.IntMap (Node VhNode), IntMap.IntMap (Edge e))
          -> IO (g,IntMap.IntMap (Node VhNode), IntMap.IntMap (Edge e))
testOp (g,n,e) = do
  return (g,getIt,e)

loadOp :: (g, IntMap.IntMap (Node VhNode), IntMap.IntMap (Edge e))
          -> IO (g,IntMap.IntMap (Node VhNode), IntMap.IntMap (Edge e))
loadOp  (g,n,e) = do
  outlines <- getPage "./src/Vh.hs"
  let xs = map (\ol -> mkNode (T.unpack  $ odName ol)) outlines
      -- xs = map (\ol -> mkNode ol) ["mary","joe","bob"]
      -- xs = map (\ol -> mkNode ol) ["mary"]
      -- xs = [mkNode (show $ length outlines)]
      ns = IntMap.fromList $ zip [1..] xs
  return (g,ns,e)

mkNode :: String -> Node VhNode
mkNode name = constructNode name (DoublePoint 7 5) True (Left "External") VhExternal Nothing

-- ---------------------------------------------------------------------

myGraphOp
  :: (String,
      (g, IntMap.IntMap (Node VhNode), IntMap.IntMap (Edge e)) -> IO (g,IntMap.IntMap (Node VhNode), IntMap.IntMap (Edge e)))
     -> (String, State g VhNode e c -> IO ())
myGraphOp (opName,operation) =
  (opName, \state-> do{ pDoc <- getDocument state
                      ; doc  <- PD.getDocument pDoc
                      ; let network = getNetwork doc
                            g = getGlobalInfo doc
                            n = networkNodes network
                            e = networkEdges network
                      ; (g',n',e') <- operation (g,n,e)
                      ; let
                            network' = setNodeAssocs (IntMap.assocs n')
                                       $ setEdgeAssocs (IntMap.assocs e')
                                       -- $ setGlobalInfo g'
                                       $ network
                      ; PD.updateDocument opName (setNetworkAndGlobal network' g') pDoc
                      }
  )





{-
data Node n = Node
    { nodePosition  :: DoublePoint  -- ^ the position of the node on screen
    , nodeName      :: !String
    , nodeNameAbove :: Bool         -- ^ should the name be displayed above (True) of below (False)
    , nodeShape     :: Either String Shape.Shape	-- ^ name from palette, or shape
    , nodeInfo      :: n
    , nodeArity     :: Maybe (PortNr,PortNr)	-- ^ number of in/out connection ports
    } deriving (Show, Read, Data, Typeable)
-}

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

-- foo = example "./src/Vh.hs"

-- EOF
