{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Blobs.Dfd.Types where

import Data.List(isPrefixOf)
import Graphics.Blobs.InfoKind
import Text.Parse
import Text.XML.HaXml.Types
-- import qualified Text.XML.HaXml.XmlContent.Haskell as XML
import Text.XML.HaXml.XmlContent.Haskell

-- ---------------------------------------------------------------------

data DfdNode = DfdExternal | DfdProcess | DfdStore deriving (Show,Eq)
data DfdFlow = DfdFlow String deriving (Show,Eq)
data DfdGlobal = DfdGlobal { flows :: [DfdFlow] } deriving (Show,Eq)


emptyGlobal :: DfdGlobal
emptyGlobal = DfdGlobal { flows = [] }

instance Descriptor DfdGlobal where
  descriptor a = "DfdGlobal:"

-- Run DrIFT-cabalized on this file to generate results

{-!for DfdNode   derive : Parse,XmlContent !-}
{-!for DfdFlow   derive : Parse,XmlContent !-}
{-!for DfdGlobal derive : Parse,XmlContent !-}


{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Parse DfdGlobal where
    parse = constructors
	[ ( "DfdGlobal"
	  , return DfdGlobal `discard` isWord "{" `apply` field "flows"
			     `discard` isWord "}"
	  )
	]

instance HTypeable DfdGlobal where
    toHType v =
	Defined "DfdGlobal" [] [Constr "DfdGlobal" [] [toHType aa]]
      where
	(DfdGlobal aa) = v
instance XmlContent DfdGlobal where
    parseContents = do
	{ inElementWith (flip isPrefixOf) "DfdGlobal" $
		fmap DfdGlobal parseContents
	}
    toContents v@(DfdGlobal aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance Parse DfdFlow where
    parse = constructors
	[ ( "DfdFlow" , fmap DfdFlow parse )
	]

instance HTypeable DfdFlow where
    toHType v =
	Defined "DfdFlow" [] [Constr "DfdFlow" [] [toHType aa]]
      where
	(DfdFlow aa) = v
instance XmlContent DfdFlow where
    parseContents = do
	{ inElementWith (flip isPrefixOf) "DfdFlow" $
		fmap DfdFlow parseContents
	}
    toContents v@(DfdFlow aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance Parse DfdNode where
    parse = enumeration "DfdNode" [ DfdExternal , DfdProcess
				    , DfdStore ]

instance HTypeable DfdNode where
    toHType v =
	Defined "DfdNode" []
		[Constr "DfdExternal" [] [],Constr "DfdProcess" [] [],
		 Constr "DfdStore" [] []]
instance XmlContent DfdNode where
    parseContents = do
	{ e@(Elem (N t) _ _) <- elementWith (flip isPrefixOf) ["DfdStore","DfdProcess","DfdExternal"]
	; case t of
	  _ | "DfdStore" `isPrefixOf` t -> interior e $ return DfdStore
	    | "DfdProcess" `isPrefixOf` t -> interior e $ return DfdProcess
	    | "DfdExternal" `isPrefixOf` t -> interior e $ return DfdExternal
	}
    toContents v@DfdExternal =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@DfdProcess =
	[mkElemC (showConstr 1 (toHType v)) []]
    toContents v@DfdStore =
	[mkElemC (showConstr 2 (toHType v)) []]

--  Imported from other files :-

-- EOF
