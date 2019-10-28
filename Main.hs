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
import Data.Time.LocalTime (utcToLocalZonedTime)

import Network.HTTP.Directory

import Options.Applicative (fullDesc, header, progDescDoc)
import qualified Options.Applicative.Help.Pretty as P

import Paths_dl_fedora (version)

import SimpleCmd (cmd_, cmdN, error', grep_, pipe_, pipeBool, pipeFile_)
import SimpleCmdArgs

import System.Directory (createDirectory, createDirectoryIfMissing,
                         doesDirectoryExist, doesFileExist, findExecutable,
                         getHomeDirectory, getPermissions, listDirectory,
                         removeFile, setCurrentDirectory, withCurrentDirectory,
                         writable)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath (dropFileName, isRelative , joinPath, makeRelative,
                        takeExtension, takeFileName, (<.>))
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

data CheckSum = AutoCheckSum | NoCheckSum | CheckSum
  deriving Eq

dlFpo, downloadFpo :: String
dlFpo = "https://dl.fedoraproject.org/pub"
downloadFpo = "https://download.fedoraproject.org/pub"

main :: IO ()
main = do
  let pdoc = Just $ P.vcat
             [ P.text "Tool for downloading Fedora iso file images.",
               P.text ("RELEASE = " <> intercalate ", " ["release number", "respin", "rawhide", "test", "or stage"]),
               P.text "EDITION = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map (P.text . map toLower . show) [(minBound :: FedoraEdition)..maxBound])) <> P.rbrace),
               P.text "",
               P.text "See <https://fedoraproject.org/wiki/Infrastructure/MirrorManager>",
               P.text "and also <https://fedoramagazine.org/verify-fedora-iso-file>."
             ]
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Fedora iso downloader" <> progDescDoc pdoc) $
    program
    <$> switchWith 'g' "gpg-keys" "Import Fedora GPG keys for verifying checksum file"
    <*> checkSumOpts
    <*> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> switchWith 'r' "run" "Boot image in Qemu"
    <*> mirrorOpt
    <*> strOptionalWith 'a' "arch" "ARCH" "Architecture [default: x86_64]" "x86_64"
    <*> optionalWith auto 'e' "edition" "EDITION" "Fedora edition [default: workstation]" Workstation
    <*> strArg "RELEASE"
  where
    mirrorOpt :: Parser String
    mirrorOpt =
      flagWith' dlFpo 'd' "dl" "Use dl.fedoraproject.org" <|>
      strOptionalWith 'm' "mirror" "HOST" "Mirror url for /pub [default https://download.fedoraproject.org/pub]" downloadFpo

    checkSumOpts :: Parser CheckSum
    checkSumOpts =
      flagWith' NoCheckSum 'C' "no-checksum" "Do not check checksum" <|>
      flagWith AutoCheckSum CheckSum 'c' "checksum" "Do checksum even if already downloaded"

program :: Bool -> CheckSum -> Bool -> Bool -> String -> String -> FedoraEdition -> String -> IO ()
program gpg checksum dryrun run mirror arch edition tgtrel = do
  home <- getHomeDirectory
  dlDir <- getUserDir "DOWNLOAD"
  setDownloadDir dlDir home
  mgr <- httpManager
  (fileurl, filenamePrefix, (masterUrl,masterSize), mchecksum, done) <- findURL mgr
  downloadFile done mgr fileurl (masterUrl,masterSize) >>= fileChecksum mchecksum
  unless dryrun $ do
    let localfile = takeFileName fileurl
        symlink = filenamePrefix <> "-latest" <.> takeExtension fileurl
        showdestdir =
          let path = makeRelative home dlDir in
            if isRelative path then "~" </> path else path
    updateSymlink localfile symlink showdestdir
    when run $ bootImage localfile showdestdir
  where
    setDownloadDir dlDir home = do
      dirExists <- doesDirectoryExist dlDir
      unless (dryrun || dirExists) $
        when (home == dlDir) $
          error' "HOME directory does not exist!"
      unless (dirExists || dryrun) $ createDirectoryIfMissing True dlDir
      dirExists' <- if dirExists then return True
                    else doesDirectoryExist dlDir
      when dirExists' $ setCurrentDirectory dlDir

    -- urlpath, fileprefix, (master,size), checksum, downloaded
    findURL :: Manager -> IO (String, String, (String,Maybe Integer), Maybe String, Bool)
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
          let prefix = if '*' `elem` prefixPat
                       then file =~ prefixPat
                       else prefixPat
              masterUrl = masterDir </> file
          masterSize <- httpFileSize mgr masterUrl
          mlocaltime <- do
            mUtc <- httpLastModified mgr masterUrl
            case mUtc of
              Nothing -> return Nothing
              Just u -> Just <$> utcToLocalZonedTime u
          maybe (return ()) putStrLn $ showMSize masterSize <> showMDate mlocaltime
          (finalurl, already) <- do
            let localfile = takeFileName masterUrl
            exists <- doesFileExist localfile
            if exists
              then do
              done <- checkLocalFileSize localfile masterSize
              if done
                then return (masterUrl,True)
                else do
                canwrite <- writable <$> getPermissions localfile
                unless canwrite $
                  error' $ localfile <> " does have write permission, aborting!"
                findMirror masterUrl path file
              else findMirror masterUrl path file
          let finalDir = dropFileName finalurl
          putStrLn finalurl
          return (finalurl, prefix, (masterUrl,masterSize), (finalDir </>) . T.unpack <$> mchecksum, already)
        where
          showMSize = fmap (\ s -> "size " <> show s <> " ")
          showMDate = fmap (\ s -> "(" <> show s <> ")")

          findMirror masterUrl path file = do
            url <-
              if mirror == dlFpo then return masterUrl
                else
                if mirror /= downloadFpo then return $ mirror </> path
                else do
                  redir <- httpRedirect mgr $ mirror </> path </> file
                  case redir of
                    Nothing -> error' $ mirror </> path </> file <> " redirect failed"
                    Just u -> do
                      let url = B.unpack u
                      exists <- httpExists mgr url
                      if exists then return url
                        else return masterUrl
            return (url,False)

    checkLocalFileSize localfile masterSize = do
      localsize <- toInteger . fileSize <$> getFileStatus localfile
      if Just localsize == masterSize
        then do
        putStrLn "File already fully downloaded"
        return True
        else do
        let showsize =
              case masterSize of
                Nothing -> show localsize
                Just ms -> show (100 * localsize `div` ms) <> "%"
        putStrLn $ "File " <> showsize <> " downloaded"
        return False

    urlPathMRel :: Manager -> IO (String, Maybe String)
    urlPathMRel mgr = do
      let subdir =
            if edition `elem` fedoraSpins
            then joinPath ["Spins", arch, "iso"]
            else joinPath [show edition, arch, editionMedia edition]
      case tgtrel of
        "respin" -> return ("alt/live-respins", Nothing)
        "rawhide" -> return ("fedora/linux/development/rawhide" </> subdir, Just "Rawhide")
        "test" -> testRelease mgr subdir
        "stage" -> stageRelease mgr subdir
        rel | all isDigit rel -> released mgr rel subdir
        _ -> error' "Unknown release"

    testRelease :: Manager -> FilePath -> IO (FilePath, Maybe String)
    testRelease mgr subdir = do
      let path = "fedora/linux" </> "releases/test"
          url = dlFpo </> path
      -- use http-directory-0.1.6 removeTrailing
      rels <- map (T.unpack . T.dropWhileEnd (== '/')) <$> httpDirectory mgr url
      let mrel = listToMaybe rels
      return (path </> fromMaybe (error' ("test release not found in " <> url)) mrel </> subdir, mrel)

    stageRelease :: Manager -> FilePath -> IO (FilePath, Maybe String)
    stageRelease mgr subdir = do
      let path = "alt/stage"
          url = dlFpo </> path
      -- use http-directory-0.1.6 removeTrailing
      rels <- reverse <$> map (T.unpack . T.dropWhileEnd (== '/')) <$> httpDirectory mgr url
      let mrel = listToMaybe rels
      return (path </> fromMaybe (error' ("staged release not found in " <> url)) mrel </> subdir, takeWhile (/= '_') <$> mrel)

    released :: Manager -> FilePath -> FilePath -> IO (FilePath, Maybe String)
    released mgr rel subdir = do
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
      if tgtrel == "respin" then "F[1-9][0-9]*-" <> liveRespin edition <> "-x86_64" <> "-LIVE"
      else
        let showRel r = if last r == '/' then init r else r
            rel = maybeToList (showRel <$> mrelease)
            middle =
              if edition `elem` [Cloud, Container]
              then rel ++ [".*" <> arch]
              else arch : rel
        in
          intercalate "-" (["Fedora", show edition, editionType edition] ++ middle)

    downloadFile :: Bool -> Manager -> String -> (String, Maybe Integer) -> IO Bool
    downloadFile done mgr url (masterUrl,masterSize) =
      if done
        then return False
        else do
        when (url /= masterUrl) $ do
          mirrorSize <- httpFileSize mgr url
          unless (mirrorSize == masterSize) $
            putStrLn "Warning!  Mirror filesize differs from master file"
        if dryrun then return False
          else do
          cmd_ "curl" ["-C", "-", "-O", url]
          return True

    fileChecksum :: Maybe FilePath -> Bool -> IO ()
    fileChecksum Nothing _ = return ()
    fileChecksum (Just url) needChecksum =
      when ((needChecksum && checksum /= NoCheckSum) || checksum == CheckSum) $ do
        let checksumdir = ".dl-fedora-checksums"
            checksumfile = checksumdir </> takeFileName url
        exists <- do
          dirExists <- doesDirectoryExist checksumdir
          if dirExists then doesFileExist checksumfile
            else createDirectory checksumdir >> return False
        putStrLn ""
        unless exists $
          withCurrentDirectory checksumdir $
          cmd_ "curl" ["-C", "-", "-s", "-S", "-O", url]
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
        let shasum = if "CHECKSUM512" `isPrefixOf` checksumfile
                     then "sha512sum" else "sha256sum"
        if chkgpg then do
          putStrLn $ "Running gpg verify and " <> shasum <> ":"
          pipeFile_ checksumfile ("gpg",["-q"]) (shasum, ["-c", "--ignore-missing"])
          else do
          putStrLn $ "Running " <> shasum <> ":"
          cmd_ shasum ["-c", "--ignore-missing", checksumfile]

    checkForFedoraKeys :: IO Bool
    checkForFedoraKeys =
      pipeBool ("gpg",["--list-keys"]) ("grep", ["-q", " Fedora .*(" <> tgtrel <> ").*@fedoraproject.org>"])

updateSymlink :: FilePath -> FilePath -> String -> IO ()
updateSymlink target symlink showdestdir = do
  symExists <- do
    havefile <- doesFileExist symlink
    if havefile then return True
      else do
      -- check for broken symlink
      dirfiles <- listDirectory "."
      return $ symlink `elem` dirfiles
  if symExists
    then do
    linktarget <- readSymbolicLink symlink
    when (linktarget /= target) $ do
        removeFile symlink
        makeSymlink
    else makeSymlink
  where
    makeSymlink = do
      putStrLn ""
      createSymbolicLink target symlink
      putStrLn $ unwords [showdestdir </> symlink, "->", target]

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
