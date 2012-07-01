module Graphics.Blobs.VH.UI where

import Data.List
import Graphics.Blobs.VH.Types
import Graphics.Blobs.SafetyNet
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.XTC

instance Labeled VhNode where
  toLabel VhExternal = "External"
  toLabel VhProcess  = "Process"
  toLabel VhStore    = "Store"
  toLabel VhPortIn   = "In"
  toLabel VhPortOut  = "Out"

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
            VhExternal -> 0
            VhProcess  -> 1
            VhStore    -> 2
            VhPortIn   -> 3
            VhPortOut  -> 4
    ; d <- dialog parentWindow [text := dialogTitle]
    ; ok    <- button d [text := "Ok"]
    ; can   <- button d [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok

    ; rb <- (mkRadioView d Vertical [VhExternal, VhProcess, VhStore, VhPortIn, VhPortOut ]
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
