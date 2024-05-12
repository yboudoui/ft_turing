{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ft_turing (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/orion/.cabal/bin"
libdir     = "/home/orion/.cabal/lib/x86_64-linux-ghc-9.2.8/ft-turing-0.1.0.0-inplace"
dynlibdir  = "/home/orion/.cabal/lib/x86_64-linux-ghc-9.2.8"
datadir    = "/home/orion/.cabal/share/x86_64-linux-ghc-9.2.8/ft-turing-0.1.0.0"
libexecdir = "/home/orion/.cabal/libexec/x86_64-linux-ghc-9.2.8/ft-turing-0.1.0.0"
sysconfdir = "/home/orion/.cabal/etc"

getBinDir     = catchIO (getEnv "ft_turing_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ft_turing_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ft_turing_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ft_turing_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ft_turing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ft_turing_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
