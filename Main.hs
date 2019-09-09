{-# LANGUAGE CPP #-}

import Control.Applicative ((<|>)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
  , (<$>), (<*>)
#endif
  )
import Control.Monad (when, unless)

import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit, toLower, toUpper)
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T

import Network.HTTP.Directory

import Options.Applicative (fullDesc, header, progDescDoc)
import qualified Options.Applicative.Help.Pretty as P

import Paths_dl_fedora (version)

import SimpleCmd (cmd_, error', grep_, pipe_, pipeBool, pipeFile_)
import SimpleCmdArgs

import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist, getPermissions, removeFile,
                         setCurrentDirectory, writable)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath (dropFileName, joinPath, takeExtension, takeFileName, (<.>))
import System.Posix.Files (createSymbolicLink, fileSize, getFileStatus,
                           readSymbolicLink)

import Text.Read
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Regex.Posix

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
                   | MATE_Compiz
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

dlFpo, downloadFpo :: String
dlFpo = "https://dl.fedoraproject.org/pub"
downloadFpo = "https://download.fedoraproject.org/pub"

main :: IO ()
main = do
  let pdoc = Just $ P.vcat
             [ P.text "Tool for downloading Fedora iso file images.",
               P.text ("RELEASE = " <> intercalate ", " ["rawhide", "respin", "test", "or release number"]),
               P.text "EDITION = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map (P.text . map toLower . show) [(minBound :: FedoraEdition)..maxBound])) <> P.rbrace),
               P.text "",
               P.text "See <https://fedoraproject.org/wiki/Infrastructure/MirrorManager>",
               P.text "and also <https://fedoramagazine.org/verify-fedora-iso-file>."
             ]
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Fedora iso downloader" <> progDescDoc pdoc) $
    findISO
    <$> switchWith 'g' "gpg-keys" "Import Fedora GPG keys for verifying checksum file"
    <*> switchWith 'C' "no-checksum" "Do not check checksum"
    <*> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> mirrorOpt
    <*> strOptionalWith 'a' "arch" "ARCH" "Architecture [default: x86_64]" "x86_64"
    <*> optionalWith auto 'e' "edition" "EDITION" "Fedora edition [default: workstation]" Workstation
    <*> strArg "RELEASE"
  where
    mirrorOpt :: Parser String
    mirrorOpt =
      flagWith' dlFpo 'd' "dl" "Use dl.fedoraproject.org" <|>
      strOptionalWith 'm' "mirror" "HOST" "Mirror url for /pub [default https://download.fedoraproject.org/pub]" downloadFpo

findISO :: Bool -> Bool -> Bool -> String -> String -> FedoraEdition -> String -> IO ()
findISO gpg nochecksum dryrun mirror arch edition tgtrel = do
  mgr <- httpManager
  (fileurl, filenamePrefix, (masterUrl,masterSize), mchecksum) <- findURL mgr
  dlDir <- getUserDir "DOWNLOAD"
  if dryrun
    then do
    dirExists <- doesDirectoryExist dlDir
    when dirExists $ setCurrentDirectory dlDir
    else do
    createDirectoryIfMissing False dlDir
    setCurrentDirectory dlDir
  let localfile = takeFileName fileurl
  done <- downloadFile mgr fileurl (masterUrl,masterSize) localfile
  when (done && not nochecksum) $ fileChecksum mchecksum
  let symlink = filenamePrefix <> "-latest" <.> takeExtension fileurl
  updateSymlink dryrun localfile symlink
  where
    -- urlpath, fileprefix, (master,size), checksum
    findURL :: Manager -> IO (String, String, (String,Maybe Integer), Maybe String)
    findURL mgr = do
      (path,mrelease) <- urlPathMRel mgr
      -- use http-directory trailing (0.1.6)
      let masterDir = dlFpo </> path <> "/"
      hrefs <- httpDirectory mgr masterDir
      let prefixPat = makeFilePrefix mrelease
          selector = if '*' `elem` prefixPat then (=~ prefixPat) else (prefixPat `isPrefixOf`)
          mfile = listToMaybe $ filter selector $ map T.unpack hrefs
          mchecksum = listToMaybe $ filter ((if tgtrel == "respin" then T.isPrefixOf else T.isSuffixOf) (T.pack "CHECKSUM")) hrefs
      case mfile of
        Nothing ->
          error' $ "no match for " <> prefixPat <> " in " <> masterDir
        Just file -> do
          let masterUrl = masterDir </> file
          size <- httpFileSize mgr masterUrl
          -- use http-directory trailing (0.1.6)
          finalurl <- if mirror == dlFpo then return masterUrl
            else if mirror /= downloadFpo then return $ mirror </> path
            else do
            redir <- httpRedirect mgr $ mirror </> path </> file
            case redir of
              Nothing -> error' $ mirror </> path </> file <> " redirect failed"
              Just u -> do
                let url = B.unpack u
                exists <- httpExists mgr url
                if exists then return url
                  else return masterUrl
          let finalDir = dropFileName finalurl
              prefix = if '*' `elem` prefixPat
                       then file =~ prefixPat
                       else prefixPat
          putStrLn finalurl
          return (finalurl, prefix, (masterUrl,size), (finalDir </>) . T.unpack <$> mchecksum)

    -- avoid import of Manager until http-directory-0.1.6
    urlPathMRel :: Manager -> IO (String, Maybe String)
    urlPathMRel mgr = do
      let subdir =
            if edition `elem` fedoraSpins
            then joinPath ["Spins", arch, "iso"]
            else joinPath [show edition, arch, editionMedia edition]
      case tgtrel of
        "respin" -> return ("alt/live-respins", Nothing)
        "rawhide" -> return ("fedora/linux/development/rawhide" </> subdir, Just "Rawhide")
        "test" -> checkTestRel mgr subdir
        rel | all isDigit rel -> checkReleased mgr rel subdir
        _ -> error' "Unknown release"

    checkTestRel :: Manager -> FilePath -> IO (FilePath, Maybe String)
    checkTestRel mgr subdir = do
      let path = "fedora/linux" </> "releases/test"
          url = dlFpo </> path
      -- use http-directory-0.1.6 removeTrailing
      rels <- map (T.unpack . T.dropWhileEnd (== '/')) <$> httpDirectory mgr url
      let mrel = listToMaybe rels
      return (path </> fromMaybe (error' ("test release not found in " <> url)) mrel </> subdir, mrel)

    checkReleased :: Manager -> FilePath -> FilePath -> IO (FilePath, Maybe String)
    checkReleased mgr rel subdir = do
      let dir = "fedora/linux/releases"
          url = dlFpo </> dir
      exists <- httpExists mgr $ url </> rel
      if exists then return (dir </> rel </> subdir, Just rel)
        else do
        let dir' = "fedora/linux/development"
            url' = dlFpo </> dir'
        exists' <- httpExists mgr $ url' </> rel
        if exists' then return (dir' </> rel </> subdir, Just rel)
          else error' "release not found in releases/ or development/"

    makeFilePrefix :: Maybe String -> String
    makeFilePrefix mrelease =
      if tgtrel == "respin" then "F[1-9][0-9]*-" <> liveRespin edition <> "-x86_64"
      else
        let showRel r = if last r == '/' then init r else r
            rel = maybeToList (showRel <$> mrelease)
            middle =
              if edition `elem` [Cloud, Container]
              then rel ++ [".*" <> arch]
              else arch : rel
        in
          intercalate "-" (["Fedora", show edition, editionType edition] ++ middle)

    downloadFile :: Manager -> String -> (String, Maybe Integer) -> String -> IO Bool
    downloadFile mgr url (masterUrl,masterSize) localfile = do
      exists <- doesFileExist localfile
      if exists
        then do
        localsize <- fileSize <$> getFileStatus localfile
        if Just (fromIntegral localsize) == masterSize
          then do
          putStrLn "File already fully downloaded"
          return True
          else do
          canwrite <- writable <$> getPermissions localfile
          unless canwrite $ error' "file does have write permission, aborting!"
          if dryrun
            then do
            putStrLn "Local filesize differs from master"
            return False
            else do
            when (url /= masterUrl) $ do
              mirrorSize <- httpFileSize mgr url
              unless (mirrorSize == masterSize) $
                putStrLn "Warning!  Mirror filesize differs from master file"
            cmd_ "curl" ["-C", "-", "-O", url]
            return True
        else
        if dryrun then return False
        else do
          cmd_ "curl" ["-C", "-", "-O", url]
          return True

    fileChecksum :: Maybe FilePath -> IO ()
    fileChecksum mchecksum =
      case mchecksum of
        Nothing -> return ()
        Just url -> do
          let checksum = takeFileName url
          exists <- doesFileExist checksum
          putStrLn ""
          unless exists $
            cmd_ "curl" ["-C", "-", "-s", "-S", "-O", url]
          pgp <- grep_ "PGP" checksum
          when (gpg && pgp) $ do
            havekey <- checkForFedoraKeys
            unless havekey $ do
              putStrLn "Importing Fedora GPG keys:\n"
              -- https://fedoramagazine.org/verify-fedora-iso-file/
              pipe_ ("curl",["-s", "-S", "https://getfedora.org/static/fedora.gpg"]) ("gpg",["--import"])
              putStrLn ""
          chkgpg <- if pgp
            then checkForFedoraKeys
            else return False
          let shasum = if "CHECKSUM512" `isPrefixOf` checksum
                       then "sha512sum" else "sha256sum"
          if chkgpg then do
            putStrLn $ "Running gpg verify and " <> shasum <> ":"
            pipeFile_ checksum ("gpg",["-q"]) (shasum, ["-c", "--ignore-missing"])
            else do
            putStrLn $ "Running " <> shasum <> ":"
            cmd_ shasum ["-c", "--ignore-missing", checksum]

    checkForFedoraKeys :: IO Bool
    checkForFedoraKeys =
      pipeBool ("gpg",["--list-keys"]) ("grep", ["-q", " Fedora .*(" <> tgtrel <> ").*@fedoraproject.org>"])

updateSymlink :: Bool -> FilePath -> FilePath -> IO ()
updateSymlink dryrun target symlink =
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


editionType :: FedoraEdition -> String
editionType Server = "dvd"
editionType Silverblue = "ostree"
editionType Everything = "netinst"
editionType Cloud = "Base"
editionType Container = "Base"
editionType _ = "Live"

editionMedia :: FedoraEdition -> String
editionMedia Cloud = "images"
editionMedia Container = "images"
editionMedia _ = "iso"

liveRespin :: FedoraEdition -> String
liveRespin = take 4 . map toUpper . show

infixr 5 </>
(</>) :: String -> String -> String
"" </> s = s
s </> "" = s
s </> t | last s == '/' = init s </> t
        | head t == '/' = s </> tail t
s </> t = s <> "/" <> t
