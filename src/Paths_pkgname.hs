module Paths_pkgname where

-- Dummy file for flycheck to find Paths_pkgname module

import Data.Version

import Paths_genpass as P

version :: Version
version = P.version

getBinDir :: IO FilePath
getBinDir = P.getBinDir

getLibDir :: IO FilePath
getLibDir = P.getLibDir

getDynLibDir :: IO FilePath
getDynLibDir = P.getDynLibDir

getDataDir :: IO FilePath
getDataDir = P.getDataDir

getLibexecDir :: IO FilePath
getLibexecDir = P.getLibexecDir

getSysconfDir :: IO FilePath
getSysconfDir = P.getSysconfDir
