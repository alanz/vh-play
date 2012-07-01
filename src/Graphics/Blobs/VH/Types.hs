{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Blobs.VH.Types where

import Text.Parse
import Data.Data
import Data.Aeson.TH

-- ---------------------------------------------------------------------

data VhNode = VhExternal | VhProcess | VhStore | VhPortIn | VhPortOut deriving (Show,Eq,Data,Typeable)

data VhFlow = VhFlow String deriving (Show,Eq,Ord,Data,Typeable)

data VhGlobal = VhGlobal { flows :: [VhFlow] } deriving (Show,Eq,Data,Typeable)

deriveJSON id ''VhNode
deriveJSON id ''VhFlow
deriveJSON id ''VhGlobal

instance Parse VhNode where
    parse = do { isWord "VhNode"
               ; return VhExternal
               }


instance Parse VhFlow where
    parse = do { isWord "VhFlow"
               ; return VhFlow `apply` parse
               }


instance Parse VhGlobal where
    parse = do { isWord "VhGlobal"
               ; return VhGlobal {flows=[]}
               }


emptyGlobal :: VhGlobal
emptyGlobal = VhGlobal { flows = [] }


-- EOF
