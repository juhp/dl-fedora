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
import qualified Data.Text as T

import Network.HTTP.Directory

import Options.Applicative (fullDesc, header, progDescDoc)
import qualified Options.Applicative.Help.Pretty as P

import Paths_fedora_img_dl (version)

import SimpleCmd (cmd_, error', grep_, pipe_, pipeBool, pipeFile_)
import SimpleCmdArgs

import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist, getPermissions, removeFile,
                         setCurrentDirectory, writable)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath (joinPath, takeExtension, takeFileName, (<.>))
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
main = do
  let pdoc = Just $ P.text "Tool for downloading Fedora iso file images."
             P.<$$> P.text ("RELEASE = " <> intercalate ", " ["rawhide", "devel", "respin", "test", "or Release version"])
             P.<$$> P.text "EDITION = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map (P.text . map toLower . show) [(minBound :: FedoraEdition)..maxBound])) <> P.rbrace)
             P.<$$> P.text "See also <https://fedoramagazine.org/verify-fedora-iso-file>."
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Fedora iso downloader" <> progDescDoc pdoc) $
    findISO
    <$> switchWith 'g' "gpg-keys" "Import Fedora GPG keys for verifying checksum file"
    <*> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> optional (strOptionWith 'm' "mirror" "HOST" "default https://download.fedoraproject.org")
    <*> strOptionalWith 'a' "arch" "ARCH" "architecture (default x86_64)" "x86_64"
    <*> optionalWith auto 'e' "edition" "EDITION" "Fedora edition: workstation [default]" Workstation
    <*> strArg "RELEASE"

findISO :: Bool -> Bool -> Maybe String -> String -> FedoraEdition -> String -> IO ()
findISO gpg dryrun mhost arch edition tgtrel = do
  (fileurl, prefix, remotesize, mchecksum) <- findURL
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
  done <- downloadFile fileurl remotesize localfile
  when done $ fileChecksum mchecksum
  updateSymlink localfile symlink
  where
    -- (top,path,mfile)
    urlPath :: (String,String,Maybe String)
    urlPath =
      if tgtrel == "respin"
      then ("", "",
            Just ("F[1-9][0-9]*-" <> liveRespin edition <> "-x86_64"))
      else
      let dirpath =
            case tgtrel of
              "rawhide" -> "development"
              "devel" -> "development"
              "test" -> "releases/test"
              rel | all isDigit rel -> "releases"
              _ -> error' "Unknown release"
          -- dirglob = tgtrel `elem` ["devel","test"]
      in ("linux" </> dirpath,
          if edition `elem` fedoraSpins then "Spins" else joinPath [show edition, arch, editionMedia edition <> "/"], Nothing)

    -- url, fileprefix, size, checksum
    findURL :: IO (String, String, Maybe Integer, Maybe String)
    findURL = do
      mgr <- httpManager
      let (top,path,mprefix) = urlPath
      topurl <- if tgtrel == "respin"
                then return "https://dl.fedoraproject.org/pub/alt/live-respins/"
                else case mhost of
                       Just host -> return host
                       Nothing -> do
                         let dl = "https://download.fedoraproject.org/"
                         redirect <- httpRedirect mgr dl
                         case redirect of
                           Nothing -> error "redirct to mirror failed"
                           Just u -> return $ B.unpack u </> top
      mreldir <-
        case tgtrel of
          "rawhide" -> return $ Just "rawhide"
          rel | all isDigit rel -> return $ Just rel
          "respin" -> return Nothing
          -- test and devel branch
          _ -> do
            rels <- httpDirectory mgr topurl
            let mrel = listToMaybe $
                       filter (not . (T.pack "rawhide" `T.isPrefixOf`)) rels
            return $ case mrel of
                       Nothing -> error "release not found"
                       Just _ -> T.unpack <$> mrel
      let finalUrl = topurl </> fromMaybe "" mreldir </> path
      hrefs <- httpDirectory mgr finalUrl
      let showRel "rawhide" = "Rawhide"
          showRel r = if last r == '/' then init r else r
          prefixPat = fromMaybe (intercalate "-" (["Fedora", show edition, editionType edition, arch] <> maybeToList (showRel <$> mreldir))) mprefix
          selector = if '*' `elem` prefixPat then (=~ prefixPat) else (prefixPat `isPrefixOf`)
          mfile = listToMaybe $ filter selector $ map T.unpack hrefs
          mchecksum = listToMaybe $ filter ((if tgtrel == "respin" then T.isPrefixOf else T.isSuffixOf) (T.pack "CHECKSUM")) hrefs
      case mfile of
        Nothing ->
          error' $ "no match for " <> finalUrl
        Just file -> do
          let finalfile = finalUrl </> file
          putStrLn finalfile
          let prefix = if '*' `elem` prefixPat
                       then file =~ prefixPat
                       else prefixPat
          size <- httpFileSize mgr finalfile
          return (finalfile, prefix, size, (finalUrl </>) . T.unpack <$> mchecksum)

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

    downloadFile :: String -> Maybe Integer -> String -> IO Bool
    downloadFile url remotesize localfile = do
      exists <- doesFileExist localfile
      if exists
        then do
        localsize <- fileSize <$> getFileStatus localfile
        if Just (fromIntegral localsize) == remotesize
          then do
          putStrLn "File already fully downloaded"
          return True
          else do
          canwrite <- writable <$> getPermissions localfile
          unless canwrite $ error' "file does have write permission, aborting!"
          if dryrun then return False
            else do
            cmd_ "curl" ["-C", "-", "-O", url]
            return True
        else
        if dryrun then return False
        else do
          cmd_ "curl" ["-C", "-", "-O", url]
          return False

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
      pipeBool ("gpg",["--list-keys"]) ("grep", ["-q", " Fedora .*(" ++ tgtrel ++ ").*@fedoraproject.org>"])

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

infixr 5 </>
(</>) :: String -> String -> String
"" </> s = s
s </> "" = s
s </> t | last s == '/' = init s </> t
        | head t == '/' = s </> tail t
s </> t = s ++ "/" ++ t
