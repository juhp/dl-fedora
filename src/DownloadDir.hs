module DownloadDir (
  setCWD,
  setDownloadDir)
where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         setCurrentDirectory)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath

setDownloadDir :: Bool -> String -> IO String
setDownloadDir dryrun subdir = do
  dlDir <- getUserDir "DOWNLOAD"
  dirExists <- doesDirectoryExist dlDir
  let isoDir = dlDir </> subdir
  isoExists <- doesDirectoryExist isoDir
  if isoExists
    then setCWD isoDir
    else
    if dirExists
    then setCWD dlDir
    else do
      if dryrun
        then return dlDir
        else do
        createDirectoryIfMissing True dlDir
        setCWD dlDir

setCWD :: FilePath -> IO FilePath
setCWD dir = do
  setCurrentDirectory dir
  return dir
