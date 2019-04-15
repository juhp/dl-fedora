{-# LANGUAGE CPP #-}

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (when, unless)

import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit, toLower)
import Data.List (intercalate)
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

import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist, getPermissions, removeFile,
                         setCurrentDirectory, writable)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath (joinPath, takeExtension, takeFileName, (</>), (<.>))
import System.Posix.Files (createSymbolicLink, fileSize, getFileStatus,
                           readSymbolicLink)

import Text.Read
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP

data FedoraEdition = Cloud
                   | Container
                   | Everything
                   | Server
                   | Silverblue
                   | Workstation
 deriving (Show, Enum, Bounded)

instance Read FedoraEdition where
  readPrec = do
    s <- look
    let e = map toLower s
        editionMap =
          map (\ ed -> (map toLower (show ed), ed)) [minBound..maxBound]
        res = lookup e editionMap
    case res of
      Nothing -> error' "unknown edition" >> RP.pfail
      Just ed -> RP.lift (R.string e) >> return ed

main :: IO ()
main =
  let pdoc = Just $ P.text "Tool for downloading Fedora iso file images."
             P.<$$> P.text "RELEASE can be 'rawhide', 'respin', 'beta' or release version" in
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Fedora iso downloader" <> progDescDoc pdoc) $
    findISO
    <$> optional (strOptionWith 'm' "mirror" "HOST" "default https://download.fedoraproject.org")
    <*> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> strOptionalWith 'a' "arch" "ARCH" "architecture (default x86_64)" "x86_64"
    <*> optionalWith auto 'e' "edition" "EDITION" "Fedora edition: workstation [default], server, ..." Workstation
    <*> strArg "RELEASE"

findISO :: Maybe String -> Bool -> String -> FedoraEdition -> String -> IO ()
findISO mhost dryrun arch edition tgtrel = do
  let (mlocn, relpath, mprefix, mrelease) =
        case tgtrel of
          "rawhide" -> (Nothing, "development/rawhide", Nothing, Just "Rawhide")
           -- FIXME: version hardcoding for respin, beta, and 30
          "respin" -> (Just "https://dl.fedoraproject.org", "pub/alt/live-respins/", Just "F29-WORK-x86_64", Nothing)
          "beta" -> (Nothing ,"releases/test/30_Beta", Nothing, Just "30_Beta") -- FIXME: hardcoding
          "30" -> (Nothing, "development/30", Nothing, Just "30") -- FIXME: hardcoding
          rel | all isDigit rel -> (Nothing, "releases" </> rel, Nothing, Just rel)
          _ -> error' "Unknown release"
  when (isJust mlocn && isJust mhost && mlocn /= mhost) $
    error' "Cannot specify host for this image"
  let host = fromMaybe "https://download.fedoraproject.org" $
             if isJust mlocn then mlocn else mhost
      toppath = if null ((decodePathSegments . extractPath) (B.pack host))
                then "pub/fedora/linux"
                else ""
      url = if isJust mlocn then host </> relpath else joinPath [host, toppath, relpath, show edition, arch, editionMedia edition <> "/"]
      prefix = fromMaybe (intercalate "-" ([editionPrefix edition, arch] <> maybeToList mrelease)) mprefix
  (fileurl, remotesize) <- findURL url prefix
  dlDir <- getUserDir "DOWNLOAD"
  if dryrun
    then do
    dirExists <- doesDirectoryExist dlDir
    when dirExists $ setCurrentDirectory dlDir
    else do
    createDirectoryIfMissing False dlDir
    setCurrentDirectory dlDir
  let localfile = takeFileName fileurl
      symlink = dlDir </> prefix <> "-latest" <.> takeExtension fileurl
  putStrLn localfile
  exists <- doesFileExist localfile
  if exists
    then do
    filestatus <- getFileStatus localfile
    let localsize = fileSize filestatus
    if Just (fromIntegral localsize) == remotesize
      then do
      putStrLn "File already fully downloaded"
      updateSymlink localfile symlink
      else do
      canwrite <- writable <$> getPermissions localfile
      unless canwrite $ error' "file does have write permission, aborting!"
      downloadFile fileurl
      updateSymlink localfile symlink
    else do
    downloadFile fileurl
    updateSymlink localfile symlink
  where
    findURL :: String -> String -> IO (String, Maybe Integer)
    findURL url prefix = do
      mgr <- newManager tlsManagerSettings
      redirect <- httpRedirect mgr url
      let finalUrl = maybe url B.unpack redirect
      when (isJust redirect) $ putStr "Redirected to "
      hrefs <- httpDirectory mgr finalUrl
      let mfile = listToMaybe $ filter (T.pack prefix `T.isPrefixOf`) hrefs :: Maybe Text
      case mfile of
        Nothing ->
          error' $ "not found " <> finalUrl
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
        when (linktarget /= target) $
          unless dryrun $ do
            removeFile symlink
            createSymbolicLink target symlink
        else unless dryrun $ createSymbolicLink target symlink
      putStrLn $ unwords [symlink, "->", target]

    downloadFile :: String -> IO ()
    downloadFile url =
      unless dryrun $ cmd_ "curl" ["-C", "-", "-O", url]

editionPrefix :: FedoraEdition -> String
editionPrefix Workstation = "Fedora-Workstation-Live"
editionPrefix Server = "Fedora-Server-dvd"
editionPrefix Silverblue = "Fedora-Silverblue-ostree"
editionPrefix Everything = "Fedora-Everything-netinst"
editionPrefix Container = "Fedora-Container-Base"
editionPrefix _ = error' "Edition not yet supported"

editionMedia :: FedoraEdition -> String
editionMedia Container = "images"
editionMedia _ = "iso"
