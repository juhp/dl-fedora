{-# LANGUAGE CPP #-}

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (when, unless)

import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (decodePathSegments, extractPath)

import Network.HTTP.Directory

import Options.Applicative (auto, fullDesc, header, optional, progDescDoc)
import qualified Options.Applicative.Help.Pretty as P

import Paths_fedora_img_dl (version)

import SimpleCmd (cmd_, error')
import SimpleCmdArgs

import System.Directory (doesFileExist, getPermissions, removeFile,
                         setCurrentDirectory, writable)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath (takeExtension, takeFileName, (</>), (<.>))
import System.Posix.Files (createSymbolicLink, fileSize, getFileStatus,
                           readSymbolicLink)

data FedoraEdition = Cloud
                   | Container
                   | Everything
                   | Server
                   | Silverblue
                   | Spins
                   | Workstation
 deriving (Read, Show)

main :: IO ()
main =
  let pdoc = Just $ P.text "Tool for downloading Fedora iso file images."
             P.<$$> P.text "RELEASE can be 'rawhide', 'respin', 'beta' or release version" in
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Fedora iso downloader" <> progDescDoc pdoc) $
    findISO
    <$> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> optional (strOptionWith 'm' "mirror" "HOST" "default https://download.fedoraproject.org")
    <*> strOptionalWith 'a' "arch" "ARCH" "architecture (default x86_64)" "x86_64"
    <*> optionalWith auto 'e' "edition" "EDITION" "Fedora edition (Workstation [default], Server, ...)" Workstation
    <*> strArg "RELEASE"

findISO :: Bool -> Maybe String -> String -> FedoraEdition -> String -> IO ()
findISO dryrun mhost arch edition release = do
  let (mlocn, relpath, mprefix) =
        case release of
          "rawhide" -> (Nothing, "development/rawhide", Nothing)
          "respin" -> (Just "https://dl.fedoraproject.org", "pub/alt/live-respins/", Just "F29-WORK-x86_64")
          "beta" -> (Nothing ,"releases/test/30_Beta", Nothing) -- FIXME: navigate!
          "30" -> (Nothing, "development/30", Nothing) -- FIXME: navigate!
          rel | all isDigit rel -> (Nothing, "releases" </> release, Nothing)
          _ -> error' "Unknown release"
  when (isJust mlocn && isJust mhost && mlocn /= mhost) $
    error' "Cannot specify host for this image"
  let host = fromMaybe "https://download.fedoraproject.org" $
             if isJust mlocn then mlocn else mhost
      toppath = if null ((decodePathSegments . extractPath) (B.pack host))
                then "pub/fedora/linux"
                else ""
      url = if isJust mlocn then host </> relpath else host </> toppath </> relpath </> show edition </> arch </> editionMedia edition ++ "/"
  (fileurl, remotesize) <- findURL url mprefix
  dlDir <- getUserDir "DOWNLOAD"
  unless dryrun $ do
    setCurrentDirectory dlDir
  let localfile = takeFileName fileurl
      symlink = dlDir </> T.unpack (editionPrefix edition) ++ "-" ++ arch ++ "-" ++ release ++ "-latest" <.> takeExtension fileurl
  exists <- doesFileExist localfile
  if exists
    then do
    filestatus <- getFileStatus localfile
    let localsize = fileSize filestatus
    if (Just (fromIntegral localsize) == remotesize)
      then do
      putStrLn "File already fully downloaded"
      unless dryrun $ updateSymlink localfile symlink
      else do
      unless dryrun $ do
        canwrite <- writable <$> getPermissions localfile
        unless canwrite $ error' "file does have write permission, aborting!"
        cmd_ "curl" ["-C", "-", "-O", fileurl]
        updateSymlink localfile symlink
    else do
    unless dryrun $ do
      cmd_ "curl" ["-C", "-", "-O", fileurl]
      updateSymlink localfile symlink
  where
    findURL :: String -> Maybe Text -> IO (String, Maybe Integer)
    findURL url mprefix = do
      mgr <- newManager tlsManagerSettings
      redirect <- httpRedirect mgr url
      let finalUrl = maybe url B.unpack redirect
      when (isJust redirect) $ putStr "Redirected to "
      hrefs <- httpDirectory mgr finalUrl
      let prefix = fromMaybe (editionPrefix edition) mprefix
          mfile = listToMaybe $ filter (prefix `T.isPrefixOf`) hrefs :: Maybe Text
      case mfile of
        Nothing ->
          error' $ "not found " ++ finalUrl
        Just file -> do
          putStrLn finalUrl
          let finalfile = finalUrl </> T.unpack file
          size <- httpFileSize mgr finalfile
          return (finalfile, size)

    updateSymlink :: FilePath -> FilePath -> IO ()
    updateSymlink target symlink = do
      symExists <- doesFileExist symlink
      if symExists
        then do
        linktarget <- readSymbolicLink symlink
        if linktarget /= target
          then do
          removeFile symlink
          createSymbolicLink target symlink
          else return ()
        else createSymbolicLink target symlink
      putStrLn $ unwords [symlink, "->", target]

editionPrefix :: FedoraEdition -> Text
editionPrefix Workstation = "Fedora-Workstation-Live"
editionPrefix Server = "Fedora-Server-dvd"
editionPrefix Silverblue = "Fedora-Silverblue-ostree"
editionPrefix Everything = "Fedora-Everything-netinst"
editionPrefix Container = "Fedora-Container-Base"
editionPrefix _ = error' "Edition not yet supported"

editionMedia :: FedoraEdition -> String
editionMedia Container = "images"
editionMedia _ = "iso"
