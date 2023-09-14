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
import Data.Time (UTCTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToZonedTime, TimeZone)

import Network.HTTP.Client (managerResponseTimeout, newManager,
                            responseTimeoutNone)
import Network.HTTP.Client.TLS
import Network.HTTP.Directory

import Options.Applicative (fullDesc, header, progDescDoc)

import Paths_dl_fedora (version)

import SimpleCmd (cmd, cmd_, cmdN, error', grep_, pipe_, pipeBool, pipeFile_,
                  sudoLog, warning, (+-+))
import SimpleCmdArgs
import SimplePrompt (yesNo)

import System.Directory (createDirectory, doesDirectoryExist, doesFileExist,
                         findExecutable, getPermissions,
                         listDirectory, removeFile, withCurrentDirectory,
                         writable)
import System.FilePath (dropFileName, joinPath, takeExtension, takeFileName,
                        (</>), (<.>))
import System.Posix.Files (createSymbolicLink, fileSize, getFileStatus,
                           readSymbolicLink)
import System.Posix.User (getLoginName)

import Text.Read
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Regex.Posix
import qualified Text.PrettyPrint.ANSI.Leijen as P

import DownloadDir

data FedoraEdition = Cloud
                   | Container
                   | Everything
                   | Server
                   | Workstation
                   | Budgie  -- first spin: used below
                   | Cinnamon
                   | I3
                   | KDE
                   | LXDE
                   | LXQt
                   | MATE
                   | SoaS
                   | Sway
                   | Xfce  -- last spin: used below
                   | Silverblue
                   | Kinoite
                   | Onyx
                   | Sericea
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
          ("sb", Silverblue) :
          map (\ ed -> (lowerEdition ed, ed)) [minBound..maxBound]
        res = lookup e editionMap
    case res of
      Nothing -> error' "unknown edition" >> RP.pfail
      Just ed -> RP.lift (R.string e) >> return ed

type URL = String

fedoraSpins :: [FedoraEdition]
fedoraSpins = [Budgie .. Xfce]

data CheckSum = AutoCheckSum | NoCheckSum | CheckSum
  deriving Eq

dlFpo, downloadFpo, kojiPkgs, odcsFpo, csComposes, odcsStream :: String
dlFpo = "https://dl.fedoraproject.org/pub"
downloadFpo = "https://download.fedoraproject.org/pub"
kojiPkgs = "https://kojipkgs.fedoraproject.org/compose"
odcsFpo = "https://odcs.fedoraproject.org/composes"
csComposes = "https://composes.stream.centos.org"
odcsStream = "https://odcs.stream.centos.org"

data Mirror = DlFpo | UseMirror | KojiFpo | Mirror String | DefaultLatest
  deriving Eq

data CentosChannel = CSProduction | CSTest | CSDevelopment
  deriving Eq

showChannel :: CentosChannel -> String
showChannel CSProduction = "production"
showChannel CSTest = "test"
showChannel CSDevelopment = "development"

data Mode = ModeDownload | ModeCheck | ModeLocal

main :: IO ()
main = do
  let pdoc = Just $ P.vcat
             [ P.text "Tool for downloading Fedora iso file images.",
               P.text ("RELEASE = " <> intercalate ", " ["release number", "respin", "rawhide", "test (Beta)", "stage (RC)", "eln", "c8s", "c9s"]),
               P.text "EDITION = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map (P.text . lowerEdition) [(minBound :: FedoraEdition)..maxBound])) <> P.rbrace) <> P.text " [default: workstation]" ,
               P.text "",
               P.text "See <https://fedoraproject.org/wiki/Infrastructure/MirrorManager>",
               P.text "and also <https://fedoramagazine.org/verify-fedora-iso-file>."
             ]
  sysarch <- readArch <$> cmd "rpm" ["--eval", "%{_arch}"]
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Fedora iso downloader" <> progDescDoc pdoc) $
    program
    <$> switchWith 'g' "gpg-keys" "Import Fedora GPG keys for verifying checksum file"
    <*> checkSumOpts
    <*> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> switchLongWith "debug" "Debug output"
    <*> switchWith 'T' "no-http-timeout" "Do not timeout for http response"
    <*> (flagWith' ModeCheck 'c' "check" "check if newer image available" <|>
         flagWith ModeDownload ModeLocal 'l' "local" "Show current local image via symlink")
    <*> switchWith 'r' "run" "Boot image in Qemu"
    <*> switchWith 'R' "replace" "Delete previous snapshot image after downloading latest one"
    <*> mirrorOpt
    <*> (flagLongWith' CSDevelopment "cs-devel" "Use centos-stream development compose" <|>
         flagLongWith CSProduction CSTest "cs-test" "Use centos-stream test compose (default is production)")
    <*> (optionWith (eitherReader eitherArch) 'a' "arch" "ARCH" ("Specify arch [default:" +-+ showArch sysarch ++ "]") <|> pure sysarch)
    <*> strArg "RELEASE"
    <*> (fromMaybe Workstation <$> optional (argumentWith auto "EDITION"))
  where
    mirrorOpt :: Parser Mirror
    mirrorOpt =
      flagWith' DefaultLatest 'L' "latest" "Get latest image either from mirror or dl.fp.o if newer" <|>
      flagWith' DlFpo 'd' "dl" "Use dl.fedoraproject.org (dl.fp.o)" <|>
      flagWith' KojiFpo 'k' "koji" "Use koji.fedoraproject.org" <|>
      Mirror <$> strOptionWith 'm' "mirror" "URL" ("Mirror url for /pub [default " ++ downloadFpo ++ "]") <|>
      pure UseMirror

    checkSumOpts :: Parser CheckSum
    checkSumOpts =
      flagLongWith' NoCheckSum "no-checksum" "Do not check checksum" <|>
      flagLongWith AutoCheckSum CheckSum "checksum" "Do checksum even if already downloaded"

data Primary = Primary {primaryUrl :: String,
                        primarySize :: Maybe Integer,
                        primaryTime :: Maybe UTCTime}

program :: Bool -> CheckSum -> Bool -> Bool -> Bool -> Mode -> Bool -> Bool
        -> Mirror -> CentosChannel -> Arch -> String -> FedoraEdition
        -> IO ()
program gpg checksum dryrun debug notimeout mode run removeold mirror channel arch tgtrel edition = do
  let mirrorUrl =
        case mirror of
          Mirror m -> m
          KojiFpo -> kojiPkgs
          DlFpo -> dlFpo
          _ -- UseMirror or DefaultLatest
            | tgtrel == "eln" -> odcsFpo
            | tgtrel `elem` ["c8s","c9s"] -> csComposes
            | otherwise -> downloadFpo
  showdestdir <- setDownloadDir dryrun "iso"
  when debug $ putStrLn showdestdir
  mgr <- if notimeout
         then newManager (tlsManagerSettings {managerResponseTimeout = responseTimeoutNone})
         else httpManager
  case mode of
    ModeCheck -> do
      (fileurl, filenamePrefix, _prime, _mchecksum, done) <-
        findURL mgr mirrorUrl showdestdir True
      if done
        then do
        putStrLn $ "Local:" +-+ takeFileName fileurl +-+ "is latest"
        when debug $
          putStrLn fileurl
        else do
        let symlink = filenamePrefix <> (if tgtrel == "eln" then "-" <> showArch arch else "") <> "-latest" <.> takeExtension fileurl
        mtarget <- derefSymlink symlink
        case mtarget of
          Just target -> do
            if target == takeFileName fileurl
              then putStrLn $ "Latest:" +-+ target +-+ "(locally incomplete)"
              else do
              putStrLn $ "Local:" +-+ target
              putStrLn $ "Newer:" +-+ takeFileName fileurl
          Nothing -> putStrLn $ "Available:" +-+ takeFileName fileurl
    ModeLocal -> do
      symlink <-
        if dryrun
        then do
        filePrefix <- getFilePrefix showdestdir
        -- FIXME support non-iso
        return $ filePrefix <> (if tgtrel == "eln" then "-" <> showArch arch else "") <> "-latest" <.> "iso"
        else do
        (fileurl, filenamePrefix, prime, _mchecksum, _done) <-
          findURL mgr mirrorUrl showdestdir False
        tz <- getCurrentTimeZone
        putStrLn $ unwords ["Newest:", takeFileName fileurl, renderTime tz (primaryTime prime)]
        putStrLn fileurl
        return $ filenamePrefix <> (if tgtrel == "eln" then "-" <> showArch arch else "") <> "-latest" <.> takeExtension fileurl
      if run
        then bootImage symlink showdestdir
        else do
        mtarget <- derefSymlink symlink
        whenJust mtarget $ \target ->
          putStrLn $ "Local: " ++ showdestdir </> target
    ModeDownload -> do
      (fileurl, filenamePrefix, prime, mchecksum, done) <-
        findURL mgr mirrorUrl showdestdir False
      let symlink = filenamePrefix <> (if tgtrel == "eln" then "-" <> showArch arch else "") <> "-latest" <.> takeExtension fileurl
      downloadFile dryrun debug done mgr fileurl prime showdestdir >>=
        fileChecksum mgr mchecksum showdestdir
      unless dryrun $ do
        let localfile = takeFileName fileurl
        updateSymlink localfile symlink showdestdir
        when run $ bootImage localfile showdestdir
  where
    findURL :: Manager -> String -> String -> Bool
            -- urlpath, fileprefix, primary, checksum, downloaded
            -> IO (URL, String, Primary, Maybe String, Bool)
    findURL mgr mirrorUrl showdestdir quiet = do
      (path,mrelease) <- urlPathMRel mgr
      -- use http-directory trailingSlash (0.1.7)
      let primaryDir =
            case tgtrel of
              "koji" -> kojiPkgs
              "eln" -> odcsFpo
              "c8s" -> odcsStream
              "c9s" -> odcsStream
              _ -> if mirror `elem` [DefaultLatest, DlFpo]
                   then dlFpo
                   else downloadFpo
            +/+ path <> "/"
      hrefs <- httpDirectory mgr primaryDir
      let prefixPat = makeFilePrefix mrelease
          selector = if '*' `elem` prefixPat then (=~ prefixPat) else (prefixPat `isPrefixOf`)
          mfile = find selector $ map T.unpack hrefs
          mchecksum = find ((if tgtrel == "respin" then T.isPrefixOf else T.isSuffixOf) (T.pack "CHECKSUM")) hrefs
      case mfile of
        Nothing ->
          error' $ "no match for " <> prefixPat <> " in " <> primaryDir
        Just file -> do
          let prefix = if '*' `elem` prefixPat
                       then (file =~ prefixPat) ++ if showArch arch `isInfixOf` prefixPat then "" else showArch arch
                       else prefixPat
              primeUrl = primaryDir +/+ file
          (primeSize,primeTime) <- httpFileSizeTime mgr primeUrl
          (finalurl, already) <- do
            let localfile = takeFileName primeUrl
            exists <- doesFileExist localfile
            if exists
              then do
              done <- checkLocalFileSize localfile primeSize primeTime showdestdir quiet
              if done
                then return (primeUrl,True)
                else do
                unlessM (writable <$> getPermissions localfile) $ do
                  putStrLn $ localfile <> " does not have write permission!"
                  yes <- yesNo "Fix ownership"
                  if yes
                    then do
                    user <- getLoginName
                    sudoLog "chown" [user,localfile]
                    else
                    error' $ localfile <> " does have write permission, aborting!"
                findMirror primeUrl path file
              else findMirror primeUrl path file
          let finalDir = dropFileName finalurl
          return (finalurl, prefix, Primary primeUrl primeSize primeTime,
                  (finalDir +/+) . T.unpack <$> mchecksum, already)
        where
          findMirror primeUrl path file = do
            url <-
              if mirrorUrl `elem` [dlFpo,kojiPkgs,odcsFpo]
              then return primeUrl
              else
                if mirrorUrl /= downloadFpo
                then return $ mirrorUrl +/+ path +/+ file
                else do
                  redir <- httpRedirect mgr $ mirrorUrl +/+ path +/+ file
                  case redir of
                    Nothing -> do
                      warning $ mirrorUrl +/+ path +/+ file <> " redirect failed"
                      return primeUrl
                    Just u -> do
                      let url = B.unpack u
                      exists <- httpExists mgr url
                      if exists then return url
                        else return primeUrl
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
                       then (file =~ prefixPat) ++ if showArch arch `isInfixOf` prefixPat then "" else showArch arch
                       else prefixPat
          return prefix

    getRelease :: Maybe String
    getRelease =
      case tgtrel of
        "rawhide" -> Just "Rawhide"
        "respin" -> Nothing
        "eln" -> Nothing
        "c8s" -> Nothing
        "c9s" -> Nothing
        rel | all isDigit rel -> Just rel
        _ -> error' $ tgtrel ++ " is unsupported with --dryrun"

    checkLocalFileSize :: FilePath -> Maybe Integer -> Maybe UTCTime
                       -> String -> Bool -> IO Bool
    checkLocalFileSize localfile mprimeSize mprimeTime showdestdir quiet = do
      localsize <- toInteger . fileSize <$> getFileStatus localfile
      if Just localsize == mprimeSize
        then do
        when (not quiet && not run) $ do
          tz <- getCurrentTimeZone
          -- FIXME abbreviate size?
          putStrLn $ unwords [showdestdir </> localfile, renderTime tz mprimeTime, "size " ++ show localsize ++ " okay"]
        return True
        else do
        when (isNothing mprimeSize) $
          putStrLn "original size could not be read"
        let sizepercent =
              case mprimeSize of
                Nothing -> show localsize
                Just ms -> show (100 * localsize `div` ms) <> "%"
        putStrLn $ "File " <> sizepercent <> " downloaded"
        return False

    urlPathMRel :: Manager -> IO (FilePath, Maybe String)
    urlPathMRel mgr = do
      let subdir =
            if edition `elem` fedoraSpins
            then joinPath ["Spins", showArch arch, "iso"]
            else joinPath [showEdition edition, showArch arch, editionMedia edition]
      case tgtrel of
        "respin" -> return ("alt/live-respins", Nothing)
        "rawhide" -> return ("fedora/linux/development/rawhide" +/+ subdir, Just "Rawhide")
        "test" -> testRelease mgr subdir
        "stage" -> stageRelease mgr subdir
        "eln" -> return ("production/latest-Fedora-ELN/compose" +/+ "BaseOS" +/+ showArch arch +/+ "iso", Nothing)
        "c8s" -> return ("stream-8" +/+ showChannel channel +/+ "latest-CentOS-Stream/compose" +/+ "BaseOS" +/+ showArch arch +/+ "iso", Nothing)
        "c9s" -> return (showChannel channel +/+ "latest-CentOS-Stream/compose" +/+ "BaseOS" +/+ showArch arch +/+ "iso", Nothing)
        rel | all isDigit rel -> released mgr rel subdir
        _ -> error' "Unknown release"

    testRelease :: Manager -> FilePath -> IO (FilePath, Maybe String)
    testRelease mgr subdir = do
      let path = "fedora/linux" +/+ "releases/test"
          url = dlFpo +/+ path
      rels <- map (T.unpack . noTrailingSlash) <$> httpDirectory mgr url
      let mrel = if null rels then Nothing else Just (last rels)
      return (path +/+ fromMaybe (error' ("test release not found in " <> url)) mrel +/+ subdir, mrel)

    stageRelease :: Manager -> FilePath -> IO (FilePath, Maybe String)
    stageRelease mgr subdir = do
      let path = "alt/stage"
          url = dlFpo +/+ path
      -- use http-directory-0.1.7 noTrailingSlash
      rels <- reverse . map (T.unpack . noTrailingSlash) <$> httpDirectory mgr url
      let mrel = listToMaybe rels
      return (path +/+ fromMaybe (error' ("staged release not found in " <> url)) mrel +/+ subdir, takeWhile (/= '_') <$> mrel)

    -- kojiCompose :: Manager -> FilePath -> IO (FilePath, Maybe String)
    -- kojiCompose mgr subdir = do
    --   let path = "branched"
    --       url = kojiPkgs +/+ path
    --       prefix = "latest-Fedora-"
    --   latest <- filter (prefix `isPrefixOf`) . map T.unpack <$> httpDirectory mgr url
    --   let mlatest = listToMaybe latest
    --   return (path +/+ fromMaybe (error' ("koji branched latest dir not not found in " <> url)) mlatest +/+ "compose" +/+ subdir, removePrefix prefix <$> mlatest)

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
        "eln" -> "Fedora-ELN"
        "c8s" -> "CentOS-Stream-8"
        "c9s" -> "CentOS-Stream-9"
        _ ->
          let showRel r = if last r == '/' then init r else r
              rel = maybeToList (showRel <$> mrelease)
              middle =
                if edition `elem` [Cloud, Container]
                then rel ++ [".*" <> showArch arch]
                else showArch arch : rel
          in
            intercalate "-" (["Fedora", showEdition edition, editionType edition] ++ middle)

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
            -- FIXME ignore other downloaded iso's (eg partial images error)
            cmd_ shasum ["-c", "--ignore-missing", checksumfile]

    checkChecksumfile :: Manager -> URL -> FilePath -> String -> IO Bool
    checkChecksumfile mgr url checksumfile showdestdir = do
      exists <- doesFileExist checksumfile
      if not exists then return False
        else do
        (primeSize,primeTime) <- httpFileSizeTime mgr url
        ok <- checkLocalFileSize checksumfile primeSize primeTime showdestdir True
        unless ok $ error' $ "Checksum file filesize mismatch for " ++ checksumfile
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

    derefSymlink :: FilePath -> IO (Maybe FilePath)
    derefSymlink symlink = do
      havefile <- doesFileExist symlink
      if havefile
        then Just <$> readSymbolicLink symlink
        else return Nothing

renderTime :: TimeZone -> Maybe UTCTime -> String
renderTime tz mprimeTime =
  "(" ++ maybe "" (show . utcToZonedTime tz) mprimeTime ++ ")"

downloadFile :: Bool -> Bool -> Bool -> Manager -> URL -> Primary -> String
             -> IO (Maybe Bool)
downloadFile dryrun debug done mgr url prime showdestdir = do
  putStrLn url
  if done
    then return (Just False)
    else do
    mtime <- do
      if url /= primaryUrl prime
        then do
        (mirrorSize,mirrorTime) <- httpFileSizeTime mgr url
        unless (mirrorSize == primarySize prime) $
          putStrLn "Warning!  Mirror filesize differs from primary file"
        unless (mirrorTime == primaryTime prime) $
          putStrLn "Warning!  Mirror timestamp differs from primary file"
        return mirrorTime
        else return $ primaryTime prime
    if dryrun
      then return Nothing
      else do
      tz <- getCurrentTimeZone
      putStrLn $ unwords ["downloading", takeFileName url, renderTime tz mtime, "to", showdestdir]
      let args = ["-C", "-", "-O", url]
      when debug $ putStrLn $ unwords $ "curl" : "--output-dir" : showdestdir : args
      cmd_ "curl" args
      return (Just True)

editionType :: FedoraEdition -> String
editionType Server = "dvd"
editionType Kinoite = "ostree"
editionType Onyx = "ostree"
editionType Sericea = "ostree"
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
      let args = ["-m", "2048", "-rtc", "base=localtime", "-cpu", "host"] ++ fileopts
      cmdN qemu (args ++ [showdir </> img])
      cmd_ qemu (args ++ [img])
    Nothing -> error' "Need qemu to run image"

#if !MIN_VERSION_http_directory(0,1,6)
noTrailingSlash :: T.Text -> T.Text
noTrailingSlash = T.dropWhileEnd (== '/')
#endif

#if !MIN_VERSION_http_directory(0,1,9)
infixr 5 +/+
(+/+) :: String -> String -> String
"" +/+ s = s
s +/+ "" = s
s +/+ t | last s == '/' = init s +/+ t
        | head t == '/' = s +/+ tail t
s +/+ t = s ++ "/" ++ t
#endif

-- derived from fedora-repoquery Types
data Arch = Source
          | X86_64
          | AARCH64
          | S390X
          | PPC64LE
  deriving Eq

eitherArch :: String -> Either String Arch
eitherArch s =
  case lower s of
    "source" -> Right Source
    "x86_64" -> Right X86_64
    "aarch64" -> Right AARCH64
    "s390x" -> Right S390X
    "ppc64le" -> Right PPC64LE
    _ -> Left $ "unknown arch: " ++ s

readArch :: String -> Arch
readArch =
  either error' id . eitherArch

showArch :: Arch -> String
showArch Source = "source"
showArch X86_64 = "x86_64"
showArch AARCH64 = "aarch64"
showArch S390X = "s390x"
showArch PPC64LE = "ppc64le"
