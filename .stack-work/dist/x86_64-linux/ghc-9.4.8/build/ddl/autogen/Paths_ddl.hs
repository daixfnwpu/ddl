{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ddl (
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
version = Version [1,1,2] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/root/haskell/ddl/.stack-work/install/x86_64-linux/57a7de0248851dc4c3cd835091108bc781a9347c34a2c89b5ed16c06b7c96613/9.4.8/bin"
libdir     = "/root/haskell/ddl/.stack-work/install/x86_64-linux/57a7de0248851dc4c3cd835091108bc781a9347c34a2c89b5ed16c06b7c96613/9.4.8/lib/x86_64-linux-ghc-9.4.8/ddl-1.1.2-GTxCW75DI4ODRSTsDqvSoz-ddl"
dynlibdir  = "/root/haskell/ddl/.stack-work/install/x86_64-linux/57a7de0248851dc4c3cd835091108bc781a9347c34a2c89b5ed16c06b7c96613/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/root/haskell/ddl/.stack-work/install/x86_64-linux/57a7de0248851dc4c3cd835091108bc781a9347c34a2c89b5ed16c06b7c96613/9.4.8/share/x86_64-linux-ghc-9.4.8/ddl-1.1.2"
libexecdir = "/root/haskell/ddl/.stack-work/install/x86_64-linux/57a7de0248851dc4c3cd835091108bc781a9347c34a2c89b5ed16c06b7c96613/9.4.8/libexec/x86_64-linux-ghc-9.4.8/ddl-1.1.2"
sysconfdir = "/root/haskell/ddl/.stack-work/install/x86_64-linux/57a7de0248851dc4c3cd835091108bc781a9347c34a2c89b5ed16c06b7c96613/9.4.8/etc"

getBinDir     = catchIO (getEnv "ddl_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ddl_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ddl_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ddl_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ddl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ddl_sysconfdir") (\_ -> return sysconfdir)




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
