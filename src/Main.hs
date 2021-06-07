{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,13,0)
import Control.Applicative ((<|>)
#if !MIN_VERSION_base(4,8,0)
  , (<$>), (<*>)
#endif
  )
import Data.Semigroup ((<>))
#endif

import Control.Monad.Extra

import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.List.Extra
import Data.Maybe
import qualified Data.Text as T
import Data.Time.LocalTime (utcToLocalZonedTime)

import Network.HTTP.Client (managerResponseTimeout, newManager,
                            responseTimeoutNone)
import Network.HTTP.Client.TLS
import Network.HTTP.Directory

import Options.Applicative (fullDesc, header, progDescDoc)
import qualified Options.Applicative.Help.Pretty as P

import Paths_dl_fedora (version)

import SimpleCmd (cmd_, cmdN, error', grep_, pipe_, pipeBool, pipeFile_,
                  removePrefix)
import SimpleCmdArgs

import System.Directory (createDirectory, doesDirectoryExist, doesFileExist,
                         findExecutable, getPermissions,
                         listDirectory, removeFile, withCurrentDirectory,
                         writable)
import System.FilePath (dropFileName, joinPath, takeExtension, takeFileName,
                        (</>), (<.>))
import System.Posix.Files (createSymbolicLink, fileSize, getFileStatus,
                           readSymbolicLink)

import Text.Read
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Regex.Posix

import DownloadDir

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
                   | MATE
                   | Soas
                   | Xfce
                   | I3
 deriving (Show, Enum, Bounded, Eq)

showEdition :: FedoraEdition -> String
showEdition MATE = "MATE_Compiz"
showEdition I3 = "i3"
showEdition e = show e

lowerEdition :: FedoraEdition -> String
lowerEdition = lower . show

instance Read FedoraEdition where
  readPrec = do
    s <- look
    let e = lower s
        editionMap =
          map (\ ed -> (lowerEdition ed, ed)) [minBound..maxBound]
        res = lookup e editionMap
    case res of
      Nothing -> error' "unknown edition" >> RP.pfail
      Just ed -> RP.lift (R.string e) >> return ed

type URL = String

fedoraSpins :: [FedoraEdition]
fedoraSpins = [Cinnamon ..]

data CheckSum = AutoCheckSum | NoCheckSum | CheckSum
  deriving Eq

dlFpo, downloadFpo, kojiPkgs, odcsFpo :: String
dlFpo = "https://dl.fedoraproject.org/pub"
downloadFpo = "https://download.fedoraproject.org/pub"
kojiPkgs = "https://kojipkgs.fedoraproject.org/compose"
odcsFpo = "https://odcs.fedoraproject.org/composes"

main :: IO ()
main = do
  let pdoc = Just $ P.vcat
             [ P.text "Tool for downloading Fedora iso file images.",
               P.text ("RELEASE = " <> intercalate ", " ["release number", "respin", "rawhide", "test (Beta)", "stage (RC)", "eln", "or koji"]),
               P.text "EDITION = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map (P.text . lowerEdition) [(minBound :: FedoraEdition)..maxBound])) <> P.rbrace) <> P.text " [default: workstation]" ,
               P.text "",
               P.text "See <https://fedoraproject.org/wiki/Infrastructure/MirrorManager>",
               P.text "and also <https://fedoramagazine.org/verify-fedora-iso-file>."
             ]
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Fedora iso downloader" <> progDescDoc pdoc) $
    program
    <$> switchWith 'g' "gpg-keys" "Import Fedora GPG keys for verifying checksum file"
    <*> checkSumOpts
    <*> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> switchWith 'T' "no-http-timeout" "Do not timeout for http response"
    <*> switchWith 'l' "local" "Show current local image via symlink"
    <*> switchWith 'r' "run" "Boot image in Qemu"
    <*> switchWith 'R' "replace" "Delete old image after downloading new one"
    <*> optional mirrorOpt
    <*> strOptionalWith 'a' "arch" "ARCH" "Architecture [default: x86_64]" "x86_64"
    <*> strArg "RELEASE"
    <*> (fromMaybe Workstation <$> optional (argumentWith auto "EDITION"))
  where
    mirrorOpt :: Parser String
    mirrorOpt =
      flagWith' dlFpo 'd' "dl" "Use dl.fedoraproject.org" <|>
      strOptionWith 'm' "mirror" "HOST" ("Mirror url for /pub [default " ++ downloadFpo ++ "]")

    checkSumOpts :: Parser CheckSum
    checkSumOpts =
      flagWith' NoCheckSum 'C' "no-checksum" "Do not check checksum" <|>
      flagWith AutoCheckSum CheckSum 'c' "checksum" "Do checksum even if already downloaded"

program :: Bool -> CheckSum -> Bool -> Bool -> Bool -> Bool -> Bool -> Maybe String -> String -> String -> FedoraEdition -> IO ()
program gpg checksum dryrun notimeout local run removeold mmirror arch tgtrel edition = do
  let mirror =
        case mmirror of
          Nothing | tgtrel == "koji" -> kojiPkgs
          Nothing | tgtrel == "eln" -> odcsFpo
          Nothing -> downloadFpo
          Just _ | tgtrel == "koji" -> error' "Cannot specify mirror for koji"
          Just m -> m
  showdestdir <- setDownloadDir dryrun "iso"
  mgr <- if notimeout
         then newManager (tlsManagerSettings {managerResponseTimeout = responseTimeoutNone})
         else httpManager
  if local
    then do
    symlink <- if dryrun
      then do
      filePrefix <- getFilePrefix showdestdir
      -- FIXME support non-iso
      return $ filePrefix <> (if tgtrel == "eln" then "-" <> arch else "") <> "-latest" <.> "iso"
      else do
      (fileurl, filenamePrefix, (_masterUrl,_masterSize), _mchecksum, _done) <- findURL mgr mirror showdestdir
      putStrLn $ "Newest " ++ takeFileName fileurl ++ "\n"
      return $ filenamePrefix <> (if tgtrel == "eln" then "-" <> arch else "") <> "-latest" <.> takeExtension fileurl
    if run
      then bootImage symlink showdestdir
      else showSymlink symlink showdestdir
    else do
    (fileurl, filenamePrefix, (masterUrl,masterSize), mchecksum, done) <- findURL mgr mirror showdestdir
    let symlink = filenamePrefix <> (if tgtrel == "eln" then "-" <> arch else "") <> "-latest" <.> takeExtension fileurl
    downloadFile done mgr fileurl (masterUrl,masterSize) >>= fileChecksum mgr mchecksum showdestdir
    unless dryrun $ do
      let localfile = takeFileName fileurl
      updateSymlink localfile symlink showdestdir
      when run $ bootImage localfile showdestdir
  where
    -- urlpath, fileprefix, (master,size), checksum, downloaded
    findURL :: Manager -> String -> String -> IO (URL, String, (URL,Maybe Integer), Maybe String, Bool)
    findURL mgr mirror showdestdir = do
      (path,mrelease) <- urlPathMRel mgr
      -- use http-directory trailingSlash (0.1.7)
      let masterDir =
            (case tgtrel of
               "koji" -> kojiPkgs
               "eln" -> odcsFpo
               _ -> dlFpo) +/+ path <> "/"
      hrefs <- httpDirectory mgr masterDir
      let prefixPat = makeFilePrefix mrelease
          selector = if '*' `elem` prefixPat then (=~ prefixPat) else (prefixPat `isPrefixOf`)
          mfile = find selector $ map T.unpack hrefs
          mchecksum = find ((if tgtrel == "respin" then T.isPrefixOf else T.isSuffixOf) (T.pack "CHECKSUM")) hrefs
      case mfile of
        Nothing ->
          error' $ "no match for " <> prefixPat <> " in " <> masterDir
        Just file -> do
          let prefix = if '*' `elem` prefixPat
                       then (file =~ prefixPat) ++ if arch `isInfixOf` prefixPat then "" else arch
                       else prefixPat
              masterUrl = masterDir +/+ file
          masterSize <- httpFileSize mgr masterUrl
          (finalurl, already) <- do
            let localfile = takeFileName masterUrl
            exists <- doesFileExist localfile
            if exists
              then do
              done <- checkLocalFileSize localfile masterSize showdestdir
              if done
                then return (masterUrl,True)
                else do
                unlessM (writable <$> getPermissions localfile) $
                  error' $ localfile <> " does have write permission, aborting!"
                findMirror masterUrl path file
              else findMirror masterUrl path file
          mlocaltime <- httpTimestamp masterUrl
          unless (run && already || local) $
            maybe (return ()) putStrLn $ showMSize masterSize <> showMDate mlocaltime
          let finalDir = dropFileName finalurl
          return (finalurl, prefix, (masterUrl,masterSize), (finalDir +/+) . T.unpack <$> mchecksum, already)
        where
          httpTimestamp url = do
            mUtc <- httpLastModified mgr url
            case mUtc of
              Nothing -> return Nothing
              Just u -> Just <$> utcToLocalZonedTime u

          showMSize = fmap (\ s -> "size " <> show s <> " ")
          showMDate = fmap (\ s -> "(" <> show s <> ")")

          findMirror masterUrl path file = do
            url <-
              if mirror `elem` [dlFpo,kojiPkgs,odcsFpo] then return masterUrl
                else
                if mirror /= downloadFpo then return $ mirror +/+ path +/+ file
                else do
                  redir <- httpRedirect mgr $ mirror +/+ path +/+ file
                  case redir of
                    Nothing -> error' $ mirror +/+ path +/+ file <> " redirect failed"
                    Just u -> do
                      let url = B.unpack u
                      exists <- httpExists mgr url
                      if exists then return url
                        else return masterUrl
            return (url,False)

    getFilePrefix :: String -> IO String
    getFilePrefix showdestdir = do
      let prefixPat = makeFilePrefix getRelease
          selector = if '*' `elem` prefixPat then (=~ prefixPat) else (prefixPat `isPrefixOf`)
      -- FIXME filter to symlinks only
      files <- listDirectory "."
      case find selector files of
        Nothing -> return $ if '*' `elem` prefixPat
          then error' $ "no match for " <> prefixPat <> " in " <> showdestdir
          else prefixPat
        Just file -> do
          let prefix = if '*' `elem` prefixPat
                       then (file =~ prefixPat) ++ if arch `isInfixOf` prefixPat then "" else arch
                       else prefixPat
          return prefix

    getRelease :: Maybe String
    getRelease =
      case tgtrel of
        "rawhide" -> Just "Rawhide"
        "respin" -> Nothing
        "eln" -> Nothing
        rel | all isDigit rel -> Just rel
        _ -> error' $ tgtrel ++ " is unsupported with --dryrun"

    checkLocalFileSize localfile masterSize showdestdir = do
      localsize <- toInteger . fileSize <$> getFileStatus localfile
      if Just localsize == masterSize
        then do
        when (not run && takeExtension localfile == ".iso") $
          putStrLn $ showdestdir </> localfile
        return True
        else do
        let showsize =
              case masterSize of
                Nothing -> show localsize
                Just ms -> show (100 * localsize `div` ms) <> "%"
        putStrLn $ "File " <> showsize <> " downloaded"
        return False

    urlPathMRel :: Manager -> IO (FilePath, Maybe String)
    urlPathMRel mgr = do
      let subdir =
            if edition `elem` fedoraSpins
            then joinPath ["Spins", arch, "iso"]
            else joinPath [showEdition edition, arch, editionMedia edition]
      case tgtrel of
        "respin" -> return ("alt/live-respins", Nothing)
        "rawhide" -> return ("fedora/linux/development/rawhide" +/+ subdir, Just "Rawhide")
        "test" -> testRelease mgr subdir
        "stage" -> stageRelease mgr subdir
        "eln" -> return ("production/latest-Fedora-ELN/compose" +/+ "Everything" +/+ arch +/+ "iso", Nothing)
        "koji" -> kojiCompose mgr subdir
        rel | all isDigit rel -> released mgr rel subdir
        _ -> error' "Unknown release"

    testRelease :: Manager -> FilePath -> IO (FilePath, Maybe String)
    testRelease mgr subdir = do
      let path = "fedora/linux" +/+ "releases/test"
          url = dlFpo +/+ path
      rels <- map (T.unpack . noTrailingSlash) <$> httpDirectory mgr url
      let mrel = listToMaybe rels
      return (path +/+ fromMaybe (error' ("test release not found in " <> url)) mrel +/+ subdir, mrel)

    stageRelease :: Manager -> FilePath -> IO (FilePath, Maybe String)
    stageRelease mgr subdir = do
      let path = "alt/stage"
          url = dlFpo +/+ path
      -- use http-directory-0.1.7 noTrailingSlash
      rels <- reverse . map (T.unpack . noTrailingSlash) <$> httpDirectory mgr url
      let mrel = listToMaybe rels
      return (path +/+ fromMaybe (error' ("staged release not found in " <> url)) mrel +/+ subdir, takeWhile (/= '_') <$> mrel)

    kojiCompose :: Manager -> FilePath -> IO (FilePath, Maybe String)
    kojiCompose mgr subdir = do
      let path = "branched"
          url = kojiPkgs +/+ path
          prefix = "latest-Fedora-"
      latest <- filter (prefix `isPrefixOf`) . map T.unpack <$> httpDirectory mgr url
      let mlatest = listToMaybe latest
      return (path +/+ fromMaybe (error' ("koji branched latest dir not not found in " <> url)) mlatest +/+ "compose" +/+ subdir, removePrefix prefix <$> mlatest)

    -- use https://admin.fedoraproject.org/pkgdb/api/collections ?
    released :: Manager -> FilePath -> FilePath -> IO (FilePath, Maybe String)
    released mgr rel subdir = do
      let dir = "fedora/linux/releases"
          url = dlFpo +/+ dir
      exists <- httpExists mgr $ url +/+ rel
      if exists then return (dir +/+ rel +/+ subdir, Just rel)
        else do
        let dir' = "fedora/linux/development"
            url' = dlFpo +/+ dir'
        exists' <- httpExists mgr $ url' +/+ rel
        if exists' then return (dir' +/+ rel +/+ subdir, Just rel)
          else error' "release not found in releases/ or development/"

    makeFilePrefix :: Maybe String -> String
    makeFilePrefix mrelease =
      case tgtrel of
        "respin" -> "F[1-9][0-9]*-" <> liveRespin edition <> "-x86_64" <> "-LIVE"
        "eln" -> "Fedora-ELN-Rawhide"
        _ ->
          let showRel r = if last r == '/' then init r else r
              rel = maybeToList (showRel <$> mrelease)
              middle =
                if edition `elem` [Cloud, Container]
                then rel ++ [".*" <> arch]
                else arch : rel
          in
            intercalate "-" (["Fedora", showEdition edition, editionType edition] ++ middle)

    downloadFile :: Bool -> Manager -> URL -> (URL, Maybe Integer)
                 -> IO (Maybe Bool)
    downloadFile done mgr url (masterUrl,masterSize) =
      if done
        then return (Just False)
        else do
        when (url /= masterUrl) $ do
          mirrorSize <- httpFileSize mgr url
          unless (mirrorSize == masterSize) $
            putStrLn "Warning!  Mirror filesize differs from master file"
        unless local $ putStrLn url
        if dryrun || local then return Nothing
          else do
          cmd_ "curl" ["-C", "-", "-O", url]
          return (Just True)

    fileChecksum :: Manager -> Maybe URL -> String -> Maybe Bool -> IO ()
    fileChecksum _ Nothing _ _ = return ()
    fileChecksum mgr (Just url) showdestdir mneedChecksum =
      when ((mneedChecksum == Just True && checksum /= NoCheckSum) || (isJust mneedChecksum && checksum == CheckSum)) $ do
        let checksumdir = ".dl-fedora-checksums"
            checksumfile = checksumdir </> takeFileName url
        exists <- do
          dirExists <- doesDirectoryExist checksumdir
          if dirExists
            then checkChecksumfile mgr url checksumfile showdestdir
            else createDirectory checksumdir >> return False
        putStrLn ""
        unless exists $
          whenM (httpExists mgr url) $
          withCurrentDirectory checksumdir $
          cmd_ "curl" ["-C", "-", "-s", "-S", "-O", url]
        haveChksum <- doesFileExist checksumfile
        if not haveChksum
          then putStrLn "No checksum file found"
          else do
          pgp <- grep_ "PGP" checksumfile
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
          let shasum = if "CHECKSUM512" `isPrefixOf` takeFileName checksumfile
                       then "sha512sum" else "sha256sum"
          if chkgpg then do
            putStrLn $ "Running gpg verify and " <> shasum <> ":"
            pipeFile_ checksumfile ("gpg",["-q"]) (shasum, ["-c", "--ignore-missing"])
            else do
            putStrLn $ "Running " <> shasum <> ":"
            cmd_ shasum ["-c", "--ignore-missing", checksumfile]

    checkChecksumfile :: Manager -> URL -> FilePath -> String -> IO Bool
    checkChecksumfile mgr url checksumfile showdestdir = do
      exists <- doesFileExist checksumfile
      if not exists then return False
        else do
        masterSize <- httpFileSize mgr url
        ok <- checkLocalFileSize checksumfile masterSize showdestdir
        unless ok $ error' "Checksum file filesize mismatch"
        return ok

    checkForFedoraKeys :: IO Bool
    checkForFedoraKeys =
      pipeBool ("gpg",["--list-keys"]) ("grep", ["-q", " Fedora .*(" <> tgtrel <> ").*@fedoraproject.org>"])

    updateSymlink :: FilePath -> FilePath -> FilePath -> IO ()
    updateSymlink target symlink showdestdir = do
      mmsymlinkTarget <- do
        havefile <- doesFileExist symlink
        if havefile
          then Just . Just <$> readSymbolicLink symlink
          else do
          -- check for broken symlink
          dirfiles <- listDirectory "."
          return $ if symlink `elem` dirfiles then Just Nothing else Nothing
      case mmsymlinkTarget of
        Nothing -> makeSymlink
        Just Nothing -> do
          removeFile symlink
          makeSymlink
        Just (Just symlinktarget) -> do
          when (symlinktarget /= target) $ do
            when removeold $ removeFile symlinktarget
            removeFile symlink
            makeSymlink
      where
        makeSymlink = do
          putStrLn ""
          createSymbolicLink target symlink
          putStrLn $ unwords [showdestdir </> symlink, "->", target]

    showSymlink :: FilePath -> FilePath -> IO ()
    showSymlink symlink showdestdir = do
      msymlinkTarget <- do
        havefile <- doesFileExist symlink
        if havefile
          then Just <$> readSymbolicLink symlink
          else return Nothing
      case msymlinkTarget of
        Just symlinktarget -> putStrLn $ showdestdir </> symlinktarget
        _ -> return ()

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
liveRespin = take 4 . upper . showEdition

bootImage :: FilePath -> String -> IO ()
bootImage img showdir = do
  let fileopts =
        case takeExtension img of
          ".iso" -> ["-boot", "d", "-cdrom"]
          _ -> []
  mQemu <- findExecutable "qemu-kvm"
  case mQemu of
    Just qemu -> do
      let args = ["-m", "2048", "-rtc", "base=localtime"] ++ fileopts
      cmdN qemu (args ++ [showdir </> img])
      cmd_ qemu (args ++ [img])
    Nothing -> error' "Need qemu to run image"

#if !MIN_VERSION_http_directory(0,1,6)
noTrailingSlash :: T.Text -> T.Text
noTrailingSlash = T.dropWhileEnd (== '/')
#endif

-- from next http-directory or http-query
infixr 5 +/+
(+/+) :: String -> String -> String
"" +/+ s = s
s +/+ "" = s
s +/+ t | last s == '/' = init s +/+ t
        | head t == '/' = s +/+ tail t
s +/+ t = s ++ "/" ++ t
