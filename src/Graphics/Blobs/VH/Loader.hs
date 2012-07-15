{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Blobs.VH.Loader where

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
        | Outline {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath}
        | TokenTypes {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath}
        | Occurrences {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath,token::String}
        | ThingAtPointCmd {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, line::Int, column::Int}
        | NamesInScope {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath}
        | Dependencies {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String]}
        | Components {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String]}
        | GetBuildFlags {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath}
        | GenerateUsage {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], returnAll:: Bool, cabalComponent::String}
    deriving (Show,Read,Data,Typeable)


tf :: TempFolder
tf=".dist-buildwrapper" &= typDir &= help "temporary folder, relative to cabal file folder"
cp :: CabalPath
cp="cabal" &= typFile &= help "location of cabal executable"
cf :: CabalFile
cf=def &= typFile &= help "cabal file"
fp :: FilePath
fp=def &= typFile &= help "relative path of file to process"
ff :: Bool
ff=def &= help "overwrite newer file"
uf :: String
uf=def &= help "user cabal flags"
co :: [String]
co=def &= help "cabal extra parameters"

v :: Verbosity
v=Normal &= help "verbosity"
wc :: WhichCabal
wc=Target &= help "which cabal file to use: original or temporary"

cc :: String
cc=def &= help "cabal component"

ra :: Bool
ra=def &= help "return all source paths"

msynchronize :: BWCmd
msynchronize = Synchronize tf cp cf uf co ff
msynchronize1 :: BWCmd
msynchronize1 = Synchronize1 tf cp cf uf co ff fp
mconfigure :: BWCmd
mconfigure = Configure tf cp cf uf co v wc
mwrite :: BWCmd
mwrite= Write tf cp cf uf co fp (def &= help "file contents")
mbuild :: BWCmd
mbuild = Build tf cp cf uf co v (def &= help "output compilation and linking result") wc
mbuild1 :: BWCmd
mbuild1 = Build1 tf cp cf uf co fp
mgetbf :: BWCmd
mgetbf = GetBuildFlags tf cp cf uf co fp
moutline :: BWCmd
moutline = Outline tf cp cf uf co fp
mtokenTypes :: BWCmd
mtokenTypes= TokenTypes tf cp cf uf co fp
moccurrences :: BWCmd
moccurrences=Occurrences tf cp cf uf co fp (def &= help "text to search occurrences of" &= name "token")
mthingAtPoint :: BWCmd
mthingAtPoint=ThingAtPointCmd tf cp cf uf co fp
        (def &= help "line" &= name "line")
        (def &= help "column" &= name "column")
mnamesInScope :: BWCmd
mnamesInScope=NamesInScope tf cp cf uf co fp
mdependencies :: BWCmd
mdependencies=Dependencies tf cp cf uf co
mcomponents :: BWCmd
mcomponents=Components tf cp cf uf co
mgenerateUsage :: BWCmd
mgenerateUsage=GenerateUsage tf cp cf uf co ra cc


runCmd :: (ToJSON a) => BWCmd -> StateT BuildWrapperState IO a -> IO ()
runCmd = runCmdV Normal

runCmdV:: (ToJSON a) => Verbosity -> BWCmd -> StateT BuildWrapperState IO a -> IO ()
runCmdV vb cmd f =
  evalStateT f (BuildWrapperState (tempFolder cmd) (cabalPath cmd) (cabalFile cmd) vb (cabalFlags cmd) (cabalOption cmd))
  >>= BSC.putStrLn . BS.append "build-wrapper-json:" . encode

ctempFolder :: TempFolder
ctempFolder  = ".dist-buildwrapper"

ccabalPath :: CabalPath
ccabalPath   = "cabal"

ccabalFile :: CabalFile
ccabalFile   = "vh-play.cabal"

ccabalFlags :: String
ccabalFlags  = ""

ccabalOption :: [a]
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

