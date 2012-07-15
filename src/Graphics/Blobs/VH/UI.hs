module Graphics.Blobs.VH.UI where

import Data.List
import Graphics.Blobs.VH.Types
import Graphics.Blobs.SafetyNet
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.XTC

instance Labeled VhNode where
  toLabel VhClass       = "Class"
  toLabel VhData        = "Data"
  toLabel VhFamily      = "Family"
  toLabel VhFunction    = "Function"
  toLabel VhPattern     = "Pattern"
  toLabel VhSyn         = "Syn"
  toLabel VhType        = "Type"
  toLabel VhInstance    = "Instance"
  toLabel VhField       = "Field"
  toLabel VhConstructor = "Constructor"
  toLabel VhSplice      = "Splice"


instance Labeled VhFlow where
  toLabel (VhFlow x) = x


-- ---------------------------------------------------------------------

editNodeDialog :: Window a1             -- ^ Parent frame
                  -> String             -- ^ Window title
                  -> VhNode            -- ^ Existing value
                  -> VhGlobal          -- ^ Global state
                  -> IO (Maybe VhNode) -- ^ Updated value if changed
editNodeDialog parentWindow dialogTitle initial _global = do
  do{ let selectVal = case initial of
            VhClass -> 0
            VhData -> 1
            VhFamily -> 2
            VhFunction -> 3
            VhPattern -> 4
            VhSyn -> 5
            VhType -> 6
            VhInstance -> 7
            VhField -> 8
            VhConstructor -> 9
            VhSplice -> 10

    ; d <- dialog parentWindow [text := dialogTitle]
    ; ok    <- button d [text := "Ok"]
    ; can   <- button d [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok

    ; rb <- (mkRadioView d Vertical [VhClass,VhData,VhFunction,VhPattern,VhSyn,
                                     VhType,VhInstance,VhField,VhConstructor,VhSplice]
            [ text := "Node Type", selection := selectVal ]) :: IO (RadioView VhNode ())

    ; set d [layout :=  column 2 [  widget rb
                                  , floatBottomRight $ row 5 [widget ok, widget can]
                                  ]
            ]

    ; showModal d $ \stop1 ->
                do set ok  [on command := safetyNet parentWindow $
                                          do sel <- get rb typedSelection
                                             stop1 $ Just sel
                                               ]
                   set can [on command := safetyNet parentWindow $ stop1 Nothing]
    }

-- ---------------------------------------------------------------------

editFlowDialog :: Window a1               -- ^ Parent frame
                  -> String               -- ^ Window title
                  -> [VhFlow]            -- ^ Existing value
                  -> VhGlobal            -- ^ Global value
                  -> IO (Maybe [VhFlow]) -- ^ Updated value if changed

editFlowDialog parentWindow dialogTitle initial global = do
  do{ d     <- dialog parentWindow [text := dialogTitle]
    ; ok    <- button d [text := "Ok"]
    ; can   <- button d [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok

    ; mvc <- mkMultiListView d [typedItems := initial]
    ; mvg <- mkMultiListView d [typedItems := (flows global) \\ initial]
    ; add <- button d [text := "<<"
                       , on command := do
                                         gval <- get mvg typedSelections
                                         cval <- get mvc typedItems
                                         let newcval = sort $ nub (gval ++ cval)
                                             newgval = (flows global) \\ newcval
                                         set mvc [ typedItems := newcval ]
                                         set mvg [ typedItems := newgval ]
                      ]
    ; remove <- button d [text := ">>"
                       , on command := do
                                         gval <- get mvg typedItems
                                         cval <- get mvc typedSelections
                                         cvalall <- get mvc typedItems
                                         let newcval = cvalall \\ cval
                                             newgval = (flows global) \\ newcval
                                         set mvc [ typedItems := newcval ]
                                         set mvg [ typedItems := newgval ]
                      ]

    ; set d [layout :=  column 2 [  row 5 [minsize (sz 100 100) $ widget mvc,
                                           column 5 [widget add, widget remove],
                                           minsize (sz 100 100) $ widget mvg]
                                 , floatBottomRight $ row 5 [widget ok, widget can]
                                 ]
            ]

    ; showModal d $ \stop1 ->
                do set ok  [on command := safetyNet parentWindow $
                                          do xs <- get mvc typedItems
                                             stop1 $ Just xs
                                               ]
                   set can [on command := safetyNet parentWindow $ stop1 Nothing]
    }

-- ---------------------------------------------------------------------

editGlobalDialog :: Window a1               -- ^ Parent frame
                  -> String               -- ^ Window title
                  -> VhGlobal            -- ^ Existing value
                  -> IO (Maybe VhGlobal) -- ^ Updated value if changed

editGlobalDialog parentWindow dialogTitle initial = do
  do{ d     <- dialog parentWindow [text := dialogTitle ]
    ; ok    <- button d [text := "Ok"]
    ; can   <- button d [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok

    ; mv <- mkMultiListView d [typedItems := sort $ nub [VhFlow "foo",VhFlow "bar",VhFlow "baz"]]

    ; let foo = "" :: String
    ; ve <- mkValueEntry d [ typedValue := (Just foo) ]
    ; new   <- button d [text := "New"
                        , on command := do
                                          cur  <- get mv typedItems
                                          mVal <- get ve typedValue
                                          case mVal of
                                            Just v ->
                                              set mv [ typedItems := sort $ nub (cur ++ [(VhFlow v)])]
                                            Nothing -> return ()
                        ]

    ; set d [layout :=  column 2 [  widget mv
                                 , row 5 [widget ve, widget new]
                                 , floatBottomRight $ row 5 [widget ok, widget can]
                                 ]
            ]


    ; showModal d $ \stop1 ->
                do set ok  [on command := safetyNet parentWindow $
                                          do xs <- get mv typedItems
                                             stop1 $ Just (initial { flows = xs })
                                               ]
                   -- set new [on command := safetyNet parentWindow $
                   --                        do sel <- get mv typedSelections
                   --                           n   <- get vi typedSelection
                   --                           stop1 $ Just (initial { flows = sel })
                   --                             ]

                   set can [on command := safetyNet parentWindow $ stop1 Nothing]

    }

-- EOF
