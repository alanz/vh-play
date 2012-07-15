{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

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
import Language.Haskell.BuildWrapper.Base
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Graphics.Blobs.NetworkUI as NetworkUI
import qualified Graphics.Blobs.Palette as P
import qualified Graphics.Blobs.PersistentDocument as PD
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
    blank = VhFunction
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
  descriptor VhClass       = "Class"
  descriptor VhData        = "Data"
  descriptor VhFamily      = "Family"
  descriptor VhFunction    = "Function"
  descriptor VhPattern     = "Pattern"
  descriptor VhSyn         = "Syn"
  descriptor VhType        = "Type"
  descriptor VhInstance    = "Instance"
  descriptor VhField       = "Field"
  descriptor VhConstructor = "Constructor"
  descriptor VhSplice      = "Splice"

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
getIt = IntMap.fromList [(1, constructNode "Name" (DoublePoint 7 5) True (Left "External") VhFunction Nothing )]

-- ---------------------------------------------------------------------

testOp :: (g, IntMap.IntMap (Node VhNode), IntMap.IntMap (Edge e))
          -> IO (g,IntMap.IntMap (Node VhNode), IntMap.IntMap (Edge e))
testOp (g,n,e) = do
  return (g,getIt,e)

loadOp :: (g, IntMap.IntMap (Node VhNode), IntMap.IntMap (Edge e))
          -> IO (g,IntMap.IntMap (Node VhNode), IntMap.IntMap (Edge e))
loadOp  (g,n,e) = do
  outlines <- getPage "./src/Vh.hs"
  let xs = map (\(p,ol) -> mkNode (T.unpack  $ odName ol) p (outlineTypeToVhNode $ head $ odType ol)) $ zip (cycle posns) outlines
      -- xs = map (\ol -> mkNode ol) ["mary","joe","bob"]
      -- xs = map (\ol -> mkNode ol) ["mary"]
      -- xs = [mkNode (show $ length outlines)]
      ns = IntMap.fromList $ zip [1..] xs
  return (g,ns,e)


outlineTypeToVhNode Class       = VhClass
outlineTypeToVhNode Data        = VhData
outlineTypeToVhNode Family      = VhFamily
outlineTypeToVhNode Function    = VhFunction
outlineTypeToVhNode Pattern     = VhPattern
outlineTypeToVhNode Syn	        = VhSyn
outlineTypeToVhNode Type        = VhType
outlineTypeToVhNode Instance    = VhInstance
outlineTypeToVhNode Field       = VhField
outlineTypeToVhNode Constructor = VhConstructor
outlineTypeToVhNode Splice      = VhSplice

posns = [DoublePoint (a/1) (b/1) | a <- [1,4..15], b <- [1,3..9]]

mkNode :: String -> DoublePoint-> VhNode -> Node VhNode
mkNode name pos typ = constructNode name pos True (Left $ descriptor typ) typ Nothing

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


-- ---------------------------------------------------------------------

palettes :: [(PaletteId, P.Palette VhNode)]
palettes = [(toPaletteId "default",palette)
           ]

palette :: P.Palette VhNode
palette =   P.Palette
  [ ("Class"
    , ( Circle  { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 200 128
                                        }
              , shapeRadius = 0.5 }
      , Just VhClass ))
  , ("Data"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 2
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 200 200 200
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5
                                 , DoublePoint (-0.5) 0.5 ] }
      , Just VhData ))
  , ("Family"
    , ( Circle  { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 200 200 128
                                        }
              , shapeRadius = 0.5 }
      , Just VhFamily ))
  , ("Function"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 2
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 200 128 200
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5
                                 , DoublePoint (-0.5) 0.5 ] }
      , Just VhFunction ))

  , ("Pattern"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 2
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 200 128 128
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5
                                 , DoublePoint (-0.5) 0.5 ] }
      , Just VhPattern ))
  , ("Syn"
    , ( Circle  { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 200 200
                                        }
              , shapeRadius = 0.5 }
      , Just VhSyn ))
  , ("Type"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 2
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 200 128
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5
                                 , DoublePoint (-0.5) 0.5 ] }
      , Just VhType ))


  , ("Instance"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 2
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 128 200
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5
                                 , DoublePoint (-0.5) 0.5 ] }
      , Just VhInstance ))
  , ("Field"
    , ( Circle  { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 128 128
                                        }
              , shapeRadius = 0.5 }
      , Just VhField ))
  , ("Constructor"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 2
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 100 100 100
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5
                                 , DoublePoint (-0.5) 0.5 ] }
      , Just VhConstructor ))
  , ("Splice"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 2
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 100 128 100
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5
                                 , DoublePoint (-0.5) 0.5 ] }
      , Just VhSplice ))

  ]

-- ---------------------------------------------------------------------

-- foo = example "./src/Vh.hs"

-- EOF
