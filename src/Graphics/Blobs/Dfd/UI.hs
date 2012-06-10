module Graphics.Blobs.Dfd.UI where

import Graphics.Blobs.CommonIO
import Graphics.Blobs.Dfd.Types
import Graphics.Blobs.SafetyNet
import Graphics.UI.WX
import Graphics.UI.WXCore

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

    ; rb <- radioBox d Vertical ["External", "Process", "Store" ]
            [ text := "Node Type", selection := selectVal ]


    ; set d [layout :=  column 2 [  widget rb
                                  , floatBottomRight $ row 5 [widget ok, widget can]
                                  ]
            ]

    ; showModal d $ \stop1 ->
                do set ok  [on command := safetyNet parentWindow $
                                          do sel <- get rb selection
                                             case sel of
                                               0 -> stop1 (Just (DfdExternal))
                                               1 -> stop1 (Just (DfdProcess))
                                               2 -> stop1 (Just (DfdStore))
                                               ]
                   set can [on command := safetyNet parentWindow $ stop1 Nothing]
    }

-- EOF
