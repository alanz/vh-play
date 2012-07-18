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

-- import qualified MonadUtils as GMU
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

{-
runCmd :: (ToJSON a) => BWCmd -> StateT BuildWrapperState IO a -> IO ()
runCmd = runCmdV Normal
-}
{-
runCmdV:: (ToJSON a) => Verbosity -> BWCmd -> StateT BuildWrapperState IO a -> IO ()
runCmdV vb cmd f =
  evalStateT f (BuildWrapperState (tempFolder cmd) (cabalPath cmd) (cabalFile cmd) vb (cabalFlags cmd) (cabalOption cmd))
  >>= BSC.putStrLn . BS.append "build-wrapper-json:" . encode
-}

runCmdVt :: Monad m => StateT BuildWrapperState m a -> m a
runCmdVt f = evalStateT f bwState

bwState  = BuildWrapperState
  {- tempFolder  = -} ctempFolder
  {- , cabalPath   = -} ccabalPath
  {- , cabalFile   = -} ccabalFile
  {- , verbosity   = -} Normal
  {- , cabalFlags  = -} ccabalFlags
  {- , cabalOpts   = -} ccabalOption





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
sync = runCmdVt (synchronize True)
  >>= BSC.putStrLn . BS.append "build-wrapper-json:" . encode


outline' :: FilePath -> IO (OpResult OutlineResult)
outline' filePath = runCmdVt (getOutline filePath)


getPage :: FilePath -> IO (String, [OutlineDef])
getPage filePath = do
  (res, _notes) <- outline' filePath
  modulename <- getModuleInfo' filePath
  return (modulename, (orOutline res))

-- ---------------------------------------------------------------------

-- | IO function to getModuleInfo
-- getModuleInfo' :: FilePath -> IO (String)
getModuleInfo' filePath = do
  let
  (info,notes) <- runCmdVt (getModuleInfo filePath)
  return info

-- | BuildWrapper function to getModuleInfo
getModuleInfo :: FilePath -> BuildWrapper(OpResult (String))
getModuleInfo filePath = do
  mi <- withGHCAST filePath getModuleInfoBw
  return $ case mi of
    (Just m,ns)->(m,ns)
    (Nothing,ns)-> ("aaNothing",ns)
  -- return mi
  -- return (Just "foo",[])


-- | Function to be passed in to withGHCAST
{-
getModuleInfoBw :: FilePath -- ^ source file path
                   -> FilePath -- ^ base directory
                   -> String  -- ^ module name
                   -> [String] -- ^  build flags
                   -> IO (String)
-}
-- getModuleInfoBw fp base_dir modul options = return modul
-- getModuleInfoBw fp base_dir modul options = return $ fp ++ base_dir ++ modul ++ (show options)
getModuleInfoBw _fp _base_dir _modul _options = do
  -- GMU.liftIO $ putStrLn "getModuleInfoBw"
  return ("foobar"::String)

-- ---------------------------------------------------------------------

getFilesBw :: BuildWrapper(OpResult [FilePath])
getFilesBw = do
        -- cf < - gets cabalFile
        (fileList,ns)<-getFilesToCopy
        return ((fileList), ns)

getFiles :: IO [FilePath]
getFiles = do
  (files,_notes) <- runCmdVt (getFilesBw)
  return files

-- ---------------------------------------------------------------------

getAllPages :: IO [(String, [OutlineDef])]
getAllPages = do
  files <- getFiles
  m1 <- mapM getPage files
  return m1

-- EOF
