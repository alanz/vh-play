module Graphics.Blobs.Dfd.UI where

import Data.Maybe
import Graphics.Blobs.CommonIO
import Graphics.Blobs.Dfd.Types
import Graphics.Blobs.SafetyNet
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.XTC

instance Labeled DfdNode where
  toLabel DfdExternal = "External"
  toLabel DfdProcess  = "Process"
  toLabel DfdStore    = "Store"

{-
editDialog :: Window a1 -- ^ Parent frame
              -> String -- ^ Window title
              -> a      -- ^ Existing value
              -> IO (Maybe a) -- ^ Updated value if changed
-}

editNodeDialog :: Window a1             -- ^ Parent frame
                  -> String             -- ^ Window title
                  -> DfdNode            -- ^ Existing value
                  -> IO (Maybe DfdNode) -- ^ Updated value if changed

editNodeDialog parentWindow dialogTitle initial = do
  do{ let selectVal = case initial of
            DfdExternal -> 0
            DfdProcess -> 1
            DfdStore -> 2
    ; d <- dialog parentWindow [text := dialogTitle]
    ; ok    <- button d [text := "Ok"]
    ; can   <- button d [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok

    ; rb <- (mkRadioView d Vertical [DfdExternal, DfdProcess, DfdStore ]
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
                  -> IO (Maybe [DfdFlow]) -- ^ Updated value if changed

editFlowDialog parentWindow dialogTitle initial = do
  do{ d     <- dialog parentWindow [text := dialogTitle]
    ; ok    <- button d [text := "Ok"]
    ; can   <- button d [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok

    ; rb <- (mkRadioView d Vertical [DfdExternal, DfdProcess, DfdStore ]
            [ text := "Flow" ]) :: IO (RadioView DfdNode ())


    ; set d [layout :=  column 2 [  widget rb
                                  , floatBottomRight $ row 5 [widget ok, widget can]
                                  ]
            ]

    ; showModal d $ \stop1 ->
                do set ok  [on command := safetyNet parentWindow $
                                          do sel <- get rb typedSelection
                                             stop1 $ Just []
                                               ]
                   set can [on command := safetyNet parentWindow $ stop1 Nothing]
    }

-- ---------------------------------------------------------------------

editGlobalDialog :: Window a1               -- ^ Parent frame
                  -> String               -- ^ Window title
                  -> DfdGlobal            -- ^ Existing value
                  -> IO (Maybe DfdGlobal) -- ^ Updated value if changed

editGlobalDialog parentWindow dialogTitle initial = do
  do{ d     <- dialog parentWindow [text := dialogTitle]
    ; ok    <- button d [text := "Ok"]
    ; can   <- button d [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok

    ; rb <- (mkRadioView d Vertical [DfdExternal, DfdProcess, DfdStore ]
            [ text := "Flow" ]) :: IO (RadioView DfdNode ())


    ; set d [layout :=  column 2 [  widget rb
                                  , floatBottomRight $ row 5 [widget ok, widget can]
                                  ]
            ]

    ; showModal d $ \stop1 ->
                do set ok  [on command := safetyNet parentWindow $
                                          do sel <- get rb typedSelection
                                             stop1 $ Just emptyGlobal
                                               ]
                   set can [on command := safetyNet parentWindow $ stop1 Nothing]
    }

-- EOF
