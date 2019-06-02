{-# LANGUAGE CPP #-}

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (when, unless)

import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit, toLower, toUpper)
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP.Types (decodePathSegments, extractPath)

import Network.HTTP.Directory

import Options.Applicative (auto, fullDesc, header, optional, progDescDoc)
import qualified Options.Applicative.Help.Pretty as P

import Paths_fedora_img_dl (version)

import SimpleCmd (cmd_, error', grep_, shell_, shellBool)
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

{-# ANN module "HLint: ignore Use camelCase" #-}
data FedoraEdition = Cloud
                   | Container
                   | Everything
                   | Server
                   | Silverblue
                   | Workstation
                   | Cinnamon
                   | KDE
                   | LXDE
                   | LXQt
                   | Mate_Compiz
                   | Soas
                   | Xfce
 deriving (Show, Enum, Bounded, Eq)

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

fedoraSpins :: [FedoraEdition]
fedoraSpins = [Cinnamon ..]

main :: IO ()
main =
  let pdoc = Just $ P.text "Tool for downloading Fedora iso file images."
             P.<$$> P.text ("RELEASE = " <> intercalate ", " ["rawhide", "respin", "beta", "or Release version"])
             P.<$$> P.text "EDITION = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map (P.text . map toLower . show) [(minBound :: FedoraEdition)..maxBound])) <> P.rbrace) in
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Fedora iso downloader" <> progDescDoc pdoc) $
    findISO
    <$> optional (strOptionWith 'm' "mirror" "HOST" "default https://download.fedoraproject.org")
    <*> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> strOptionalWith 'a' "arch" "ARCH" "architecture (default x86_64)" "x86_64"
    <*> optionalWith auto 'e' "edition" "EDITION" "Fedora edition: workstation [default]" Workstation
    <*> strArg "RELEASE"

findISO :: Maybe String -> Bool -> String -> FedoraEdition -> String -> IO ()
findISO mhost dryrun arch edition tgtrel = do
  (mlocn, relpath, mprefix, mrelease) <-
        case tgtrel of
          "rawhide" -> return (Nothing, "development/rawhide", Nothing, Just "Rawhide")
           -- FIXME: version hardcoding for respin, beta, and 30
          "respin" -> do
            let mlocn = Just "https://dl.fedoraproject.org"
            when (isJust mhost && mlocn /= mhost) $
              error' "Cannot specify host for this image"
            return (mlocn, "pub/alt/live-respins/", Just ("F29-" <> liveRespin edition <> "-x86_64"), Nothing)
          "beta" -> return (Nothing ,"releases/test/30_Beta", Nothing, Just "30_Beta") -- FIXME: hardcoding
          rel | all isDigit rel -> return (Nothing, "releases" </> rel, Nothing, Just rel)
          _ -> error' "Unknown release"
  let host = fromMaybe "https://download.fedoraproject.org" $
             if isJust mlocn then mlocn else mhost
      toppath = if null ((decodePathSegments . extractPath) (B.pack host))
                then "pub/fedora/linux"
                else ""
      url = if isJust mlocn then host </> relpath else joinPath [host, toppath, relpath, if edition `elem` fedoraSpins then "Spins" else show edition, arch, editionMedia edition <> "/"]
      prefix = fromMaybe (intercalate "-" (["Fedora", show edition, editionType edition, arch] <> maybeToList mrelease)) mprefix
  (fileurl, remotesize, mchecksum) <- findURL url prefix (tgtrel == "respin")
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
  downloadFile fileurl remotesize localfile
  fileChecksum mchecksum
  updateSymlink localfile symlink
  where
    findURL :: String -> String -> Bool -> IO (String, Maybe Integer, Maybe String)
    findURL url prefix chksumPrefix = do
      mgr <- httpManager
      redirect <- httpRedirect mgr url
      let finalUrl = maybe url B.unpack redirect
      hrefs <- httpDirectory mgr finalUrl
      let mfile = listToMaybe $ filter (T.pack prefix `T.isPrefixOf`) hrefs :: Maybe Text
          mchecksum = listToMaybe $ filter ((if chksumPrefix then T.isPrefixOf else T.isSuffixOf) (T.pack "CHECKSUM")) hrefs
      case mfile of
        Nothing ->
          error' $ "not found " <> finalUrl
        Just file -> do
          let finalfile = finalUrl </> T.unpack file
          putStrLn finalfile
          size <- httpFileSize mgr finalfile
          return (finalfile, size, (finalUrl </>) . T.unpack <$> mchecksum)

    updateSymlink :: FilePath -> FilePath -> IO ()
    updateSymlink target symlink =
      unless dryrun $ do
      symExists <- doesFileExist symlink
      if symExists
        then do
        linktarget <- readSymbolicLink symlink
        when (linktarget /= target) $ do
            removeFile symlink
            createSymbolicLink target symlink
            putStrLn $ unwords [symlink, "->", target]
        else do
        createSymbolicLink target symlink
        putStrLn $ unwords [symlink, "->", target]

    downloadFile :: String -> Maybe Integer -> String -> IO ()
    downloadFile url remotesize localfile = do
      exists <- doesFileExist localfile
      if exists
        then do
        localsize <- fileSize <$> getFileStatus localfile
        if Just (fromIntegral localsize) == remotesize
          then
          putStrLn "File already fully downloaded"
          else do
          canwrite <- writable <$> getPermissions localfile
          unless canwrite $ error' "file does have write permission, aborting!"
          unless dryrun $
            cmd_ "curl" ["-C", "-", "-O", url]
        else
        unless dryrun $
        cmd_ "curl" ["-C", "-", "-O", url]

    fileChecksum :: Maybe FilePath -> IO ()
    fileChecksum mchecksum =
      case mchecksum of
        Nothing -> return ()
        Just url -> do
          let checksum = takeFileName url
          exists <- doesFileExist checksum
          unless exists $
            cmd_ "curl" ["-C", "-", "-s", "-S", "-O", url]
          pgp <- grep_ "PGP" checksum
          gpgchk <- if pgp then do
            havekeys <- shellBool "gpg --list-keys | grep -q @fedoraproject.org"
            unless havekeys $
              putStrLn "https://fedoramagazine.org/verify-fedora-iso-file/"
            return $ if havekeys then ["gpg -q |"] else []
            else return []
          let shasum = if "CHECKSUM512" `isPrefixOf` checksum
                       then "sha512sum" else "sha256sum"
          shell_ $ unwords $ ["cat", checksum, "|"] ++ gpgchk ++ [shasum, "-c --ignore-missing"]

editionType :: FedoraEdition -> String
editionType Server = "dvd"
editionType Silverblue = "ostree"
editionType Everything = "netinst"
editionType Container = "Base"
editionType _ = "Live"

editionMedia :: FedoraEdition -> String
editionMedia Container = "images"
editionMedia _ = "iso"

liveRespin :: FedoraEdition -> String
liveRespin = take 4 . map toUpper . show

