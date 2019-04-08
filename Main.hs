{-# LANGUAGE CPP #-}

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (when, unless)

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)

import Network.HTTP.Client (brConsume, hrFinalResponse, hrRedirects, newManager, parseRequest, responseBody, responseHeaders, responseOpenHistory, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (decodePathSegments, extractPath, statusCode)

import qualified Data.Text as T

import Options.Applicative (auto, fullDesc, header, optional, progDescDoc)
import qualified Options.Applicative.Help.Pretty as P
import Text.HTML.DOM (parseBSChunks)
import Text.XML.Cursor

import SimpleCmd (cmd_, error')
import SimpleCmdArgs
import Paths_fedora_img_dl (version)

import System.Directory (doesFileExist, getPermissions, removeFile,
                         setCurrentDirectory, writable)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath (takeExtension, takeFileName, (</>), (<.>))
import System.Posix.Files (createSymbolicLink, readSymbolicLink)

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
             P.<$$> P.text "RELEASE can be 'rawhide', 'branched', 'respin', 'beta' or release version" in
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
          rel | rel `elem` ["30", "branched"] -> (Nothing, "development/30", Nothing) -- FIXME: navigate!
          _ -> (Nothing, "releases" </> release, Nothing)
  when (isJust mlocn && isJust mhost && mlocn /= mhost) $
    error' "Cannot specify host for this image"
  let host = fromMaybe "https://download.fedoraproject.org" $
             if isJust mlocn then mlocn else mhost
      toppath = if null ((decodePathSegments . extractPath) (B.pack host))
                then "pub/fedora/linux"
                else ""
      url = if isJust mlocn then host </> relpath else host </> toppath </> relpath </> show edition </> arch </> editionMedia edition ++ "/"
  fileurl <- checkURL url mprefix
  putStrLn fileurl
  unless dryrun $ do
    dlDir <- getUserDir "DOWNLOAD"
    setCurrentDirectory dlDir
    let localfile = takeFileName fileurl
    exists <- doesFileExist localfile
    when exists $ do
      putStrLn "Image file already exists"
      canwrite <- writable <$> getPermissions localfile
      unless canwrite $ error' "file does have write permission, aborting!"
    cmd_ "curl" ["-C", "-", "-O", fileurl]
    let symlink = dlDir </> T.unpack (editionPrefix edition) ++ "-" ++ arch ++ "-" ++ show release ++ "-latest" <.> takeExtension fileurl
    symExists <- doesFileExist symlink
    if symExists
      then do
      lnktgt <- readSymbolicLink symlink
      unless (lnktgt == localfile) $ do
        removeFile symlink
        createSymlink localfile symlink
      else createSymlink localfile symlink
  where
    checkURL :: String -> Maybe Text -> IO String
    checkURL url mprefix = do
      req <- parseRequest url
      mgr <- newManager tlsManagerSettings
      respHist <- responseOpenHistory req mgr
      let redirect = listToMaybe . reverse $ mapMaybe (lookup "Location" . responseHeaders . snd) $ hrRedirects respHist
      let finalUrl = maybe url B.unpack redirect
      when (isJust redirect) $ putStr "Redirected to "
      let response = hrFinalResponse respHist
      -- print finalUrl
      if statusCode (responseStatus response) /= 200
        then
        error' $ show $ responseStatus response
        else do
        body <- brConsume $ responseBody response
        let doc = parseBSChunks body
            cursor = fromDocument doc
            hrefs = concatMap (attribute "href") $ cursor $// element "a"
            prefix = fromMaybe (editionPrefix edition) mprefix
            mfile = listToMaybe $ filter (prefix `T.isPrefixOf`) hrefs :: Maybe Text
        case mfile of
          Nothing -> do
            print doc
            error' $ "not found " ++ finalUrl
          Just file ->
            return $ finalUrl </> T.unpack file

    createSymlink :: FilePath -> FilePath -> IO ()
    createSymlink tgt symlink = do
      createSymbolicLink tgt symlink
      putStrLn $ symlink ++ " -> " ++ tgt

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
