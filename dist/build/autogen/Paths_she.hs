module Paths_she (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,6] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/cgb08121/.cabal/bin"
libdir     = "/Users/cgb08121/.cabal/lib/x86_64-osx-ghc-7.10.2/she-0.6-DawsUqwGsoO8pUxOoE92Ac"
datadir    = "/Users/cgb08121/.cabal/share/x86_64-osx-ghc-7.10.2/she-0.6"
libexecdir = "/Users/cgb08121/.cabal/libexec"
sysconfdir = "/Users/cgb08121/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "she_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "she_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "she_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "she_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "she_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
