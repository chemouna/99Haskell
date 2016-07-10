module Paths_99HaskellProblems (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/m.cheikhna/Documents/repos/mine/99HaskellProblems/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/bin"
libdir     = "/Users/m.cheikhna/Documents/repos/mine/99HaskellProblems/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/lib/x86_64-osx-ghc-7.10.3/99HaskellProblems-0.1.0.0-ChJG6U390BnCoFVc0okgVp"
datadir    = "/Users/m.cheikhna/Documents/repos/mine/99HaskellProblems/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/share/x86_64-osx-ghc-7.10.3/99HaskellProblems-0.1.0.0"
libexecdir = "/Users/m.cheikhna/Documents/repos/mine/99HaskellProblems/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/libexec"
sysconfdir = "/Users/m.cheikhna/Documents/repos/mine/99HaskellProblems/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "99HaskellProblems_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "99HaskellProblems_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "99HaskellProblems_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "99HaskellProblems_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "99HaskellProblems_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
