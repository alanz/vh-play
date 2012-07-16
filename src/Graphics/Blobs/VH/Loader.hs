{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Blobs.VH.Loader
       (
         getPage
       , getFiles
       , getAllPages
       ) where

{-

This file is based initially on Language.Haskell.BuildWrapper.CMD

https://github.com/JPMoresmau/BuildWrapper/blob/master/src-exe/Language/Haskell/BuildWrapper/CMD.hs

-}

import Control.Monad.State
import Data.Aeson
import Data.Version (showVersion)
import Language.Haskell.BuildWrapper.API
import Language.Haskell.BuildWrapper.Base hiding (tempFolder,cabalPath, cabalFile, cabalFlags,verbosity)
import Language.Haskell.BuildWrapper.Cabal
import Language.Haskell.BuildWrapper.GHC
-- import Paths_buildwrapper
import System.Console.CmdArgs hiding (Verbosity(..),verbosity)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

type CabalFile = FilePath
type CabalPath = FilePath
type TempFolder = FilePath

-- | all the different actions and their parameters
data BWCmd=Synchronize {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], force::Bool}
        | Synchronize1 {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], force::Bool, file:: FilePath}
        | Write {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, contents::String}
        | Configure {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], verbosity::Verbosity,cabalTarget::WhichCabal}
        | Build {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], verbosity::Verbosity,output::Bool,cabalTarget::WhichCabal}
        | Build1 {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath}
        | Outline {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath} -- In Use
        | TokenTypes {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath}
        | Occurrences {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath,token::String}
        | ThingAtPointCmd {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, line::Int, column::Int}
        | NamesInScope {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath}
        | Dependencies {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String]}
        | Components {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String]}
        | GetBuildFlags {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath}
        | GenerateUsage {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], returnAll:: Bool, cabalComponent::String}

        | GetFiles {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String] } -- In use
    deriving (Show,Read,Data,Typeable)


runCmd :: (ToJSON a) => BWCmd -> StateT BuildWrapperState IO a -> IO ()
runCmd = runCmdV Normal

runCmdV:: (ToJSON a) => Verbosity -> BWCmd -> StateT BuildWrapperState IO a -> IO ()
runCmdV vb cmd f =
  evalStateT f (BuildWrapperState (tempFolder cmd) (cabalPath cmd) (cabalFile cmd) vb (cabalFlags cmd) (cabalOption cmd))
  >>= BSC.putStrLn . BS.append "build-wrapper-json:" . encode

runCmdVt
  :: Monad m =>
     Verbosity -> BWCmd -> StateT BuildWrapperState m a -> m a
runCmdVt vb cmd f =
  evalStateT f (BuildWrapperState (tempFolder cmd) (cabalPath cmd) (cabalFile cmd) vb (cabalFlags cmd) (cabalOption cmd))


ctempFolder :: TempFolder
ctempFolder  = ".dist-buildwrapper"

ccabalPath :: CabalPath
ccabalPath   = "cabal"

ccabalFile :: CabalFile
ccabalFile   = "vh-play.cabal"

ccabalFlags :: String
ccabalFlags  = ""

ccabalOption :: [String]
ccabalOption = []

sync :: IO ()
sync = runCmd syncCmd f
  where
    syncCmd = Synchronize
                          { tempFolder  = ctempFolder
                          , cabalPath   = ccabalPath
                          , cabalFile   = ccabalFile
                          , cabalFlags  = ccabalFlags
                          , cabalOption = ccabalOption
                          , force = True
                          }
    f = (synchronize True)

{-
outline :: FilePath -> IO ()
outline filePath = runCmd outlineCmd f
  where
    outlineCmd = Outline
                          { tempFolder  = ctempFolder
                          , cabalPath   = ccabalPath
                          , cabalFile   = ccabalFile
                          , cabalFlags  = ccabalFlags
                          , cabalOption = ccabalOption
                          , file = filePath
                          }
    f = (getOutline filePath)
-}

outline' :: FilePath -> IO (OpResult OutlineResult)
outline' filePath = runCmdVt Normal outlineCmd f
  where
    outlineCmd = Outline
                          { tempFolder  = ctempFolder
                          , cabalPath   = ccabalPath
                          , cabalFile   = ccabalFile
                          , cabalFlags  = ccabalFlags
                          , cabalOption = ccabalOption
                          , file = filePath
                          }
    f = (getOutline filePath)


getPage :: FilePath -> IO [OutlineDef]
getPage filePath = do
  (res, _notes) <- outline' filePath
  return (orOutline res)

-- ---------------------------------------------------------------------

getFilesBw :: BuildWrapper(OpResult [FilePath])
getFilesBw = do
        -- cf < - gets cabalFile
        (fileList,ns)<-getFilesToCopy
        return ((fileList), ns)

getFiles :: IO [FilePath]
getFiles = do
  let
    getFilesCmd = GetFiles
                          { tempFolder  = ctempFolder
                          , cabalPath   = ccabalPath
                          , cabalFile   = ccabalFile
                          , cabalFlags  = ccabalFlags
                          , cabalOption = ccabalOption
                          }
    f = (getFilesBw)
  (files,_notes) <- runCmdVt Normal getFilesCmd f
  return files

-- ---------------------------------------------------------------------

getAllPages :: IO [[OutlineDef]]
getAllPages = do
  files <- getFiles
  m1<-mapM getPage files
  return m1
