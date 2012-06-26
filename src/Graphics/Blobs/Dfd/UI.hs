module Graphics.Blobs.Dfd.UI where

import Data.List
import Graphics.Blobs.Dfd.Types
import Graphics.Blobs.SafetyNet
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.XTC

instance Labeled DfdNode where
  toLabel DfdExternal = "External"
  toLabel DfdProcess  = "Process"
  toLabel DfdStore    = "Store"
  toLabel DfdPortIn   = "In"
  toLabel DfdPortOut  = "Out"

instance Labeled DfdFlow where
  toLabel (DfdFlow x) = x


-- ---------------------------------------------------------------------

editNodeDialog :: Window a1             -- ^ Parent frame
                  -> String             -- ^ Window title
                  -> DfdNode            -- ^ Existing value
                  -> DfdGlobal          -- ^ Global state
                  -> IO (Maybe DfdNode) -- ^ Updated value if changed
editNodeDialog parentWindow dialogTitle initial _global = do
  do{ let selectVal = case initial of
            DfdExternal -> 0
            DfdProcess  -> 1
            DfdStore    -> 2
            DfdPortIn   -> 3
            DfdPortOut  -> 4
    ; d <- dialog parentWindow [text := dialogTitle]
    ; ok    <- button d [text := "Ok"]
    ; can   <- button d [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok

    ; rb <- (mkRadioView d Vertical [DfdExternal, DfdProcess, DfdStore, DfdPortIn, DfdPortOut ]
            [ text := "Node Type", selection := selectVal ]) :: IO (RadioView DfdNode ())

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
                  -> [DfdFlow]            -- ^ Existing value
                  -> DfdGlobal            -- ^ Global value
                  -> IO (Maybe [DfdFlow]) -- ^ Updated value if changed

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
                  -> DfdGlobal            -- ^ Existing value
                  -> IO (Maybe DfdGlobal) -- ^ Updated value if changed

editGlobalDialog parentWindow dialogTitle initial = do
  do{ d     <- dialog parentWindow [text := dialogTitle ]
    ; ok    <- button d [text := "Ok"]
    ; can   <- button d [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok

    ; mv <- mkMultiListView d [typedItems := sort $ nub [DfdFlow "foo",DfdFlow "bar",DfdFlow "baz"]]

    ; let foo = "" :: String
    ; ve <- mkValueEntry d [ typedValue := (Just foo) ]
    ; new   <- button d [text := "New"
                        , on command := do
                                          cur  <- get mv typedItems
                                          mVal <- get ve typedValue
                                          case mVal of
                                            Just v ->
                                              set mv [ typedItems := sort $ nub (cur ++ [(DfdFlow v)])]
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
