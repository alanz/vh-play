{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Blobs.Dfd.Types where

import Text.Parse
import Data.Data
import Data.Aeson.TH

-- ---------------------------------------------------------------------

data DfdNode = DfdExternal | DfdProcess | DfdStore | DfdPortIn | DfdPortOut deriving (Show,Eq,Data,Typeable)

data DfdFlow = DfdFlow String deriving (Show,Eq,Ord,Data,Typeable)

data DfdGlobal = DfdGlobal { flows :: [DfdFlow] } deriving (Show,Eq,Data,Typeable)

deriveJSON id ''DfdNode
deriveJSON id ''DfdFlow
deriveJSON id ''DfdGlobal

instance Parse DfdNode where
    parse = do { isWord "DfdNode"
               ; return DfdExternal
               }


instance Parse DfdFlow where
    parse = do { isWord "DfdFlow"
               ; return DfdFlow `apply` parse
               }


instance Parse DfdGlobal where
    parse = do { isWord "DfdGlobal"
               ; return DfdGlobal {flows=[]}
               }


emptyGlobal :: DfdGlobal
emptyGlobal = DfdGlobal { flows = [] }


-- EOF
