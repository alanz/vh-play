{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Blobs.Dfd.Types where

import Data.List(isPrefixOf)
import Text.Parse
import Text.XML.HaXml.Types
import qualified Text.XML.HaXml.XmlContent.Haskell as XML

-- ---------------------------------------------------------------------

data DfdNode = DfdExternal | DfdProcess | DfdStore deriving (Show,Eq)
data DfdFlow = DfdFlow String deriving (Show,Eq)

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

instance Parse DfdFlow where
    parse = do { isWord "DfdFlow"
               ; return (DfdFlow [])
               }

instance XML.HTypeable DfdNode where
    toHType _v = XML.Defined "DfdNode" []
                    [ XML.Constr "DfdExternal" [] []
                    , XML.Constr "DfdProcess"  [] []
                    , XML.Constr "DfdStore"    [] []
                    ]

instance XML.XmlContent DfdNode where
    parseContents = do
        { e@(Elem (N t) _ _) <- XML.element  ["DfdExternal","DfdProcess","DfdStore"]
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

instance XML.HTypeable DfdFlow where
    toHType _v = XML.Defined "DfdFlow" []
                    [ XML.Constr "DfdFlow" [] []
                    ]
instance XML.XmlContent DfdFlow where
    parseContents = do { return (DfdFlow []) }
    toContents _v@(DfdFlow _xs) = []

