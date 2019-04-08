{-# LANGUAGE NoImplicitPrelude #-}

import BasicPrelude

import qualified Data.ByteString.Char8 as B

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
--import Paths_fedora_iso_dl (version)

import System.Directory (createFileLink, doesFileExist, getPermissions,
                         getSymbolicLinkTarget, removeFile, setCurrentDirectory,
                         writable)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath (takeExtension, takeFileName)

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
  simpleCmdArgsWithMods Nothing (fullDesc <> header "Fedora iso downloader" <> progDescDoc pdoc) $
    findISO
    <$> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> strOptionalWith 'm' "mirror" "HOST" "default https://download.fedoraproject.org" "https://download.fedoraproject.org"
    <*> strOptionalWith 'a' "arch" "ARCH" "architecture (default x86_64)" "x86_64"
    <*> optionalWith auto 'e' "edition" "EDITION" "Fedora edition (Workstation [default], Server, ...)" Workstation
    <*> argumentWith auto "RELEASE"

findISO :: Bool -> String -> String -> FedoraEdition -> Int -> IO ()
findISO dryrun host arch edition release = do
  let (relpath, _released) = case release of
               30 -> ("development/30", False)
               _ -> if release > 30
                    then ("development/rawhide", False)
                    else ("releases" </> show release, True)
      toppath = if null ((decodePathSegments . extractPath) (B.pack host))
                then "pub/fedora/linux"
                else ""
      path = toppath </> relpath </> show edition </> arch </> editionMedia edition
  fileurl <- checkURL path
  putStrLn $ T.pack fileurl
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
      lnktgt <- getSymbolicLinkTarget symlink
      unless (lnktgt == localfile) $ do
        removeFile symlink
        createSymlink localfile symlink
      else createSymlink localfile symlink
  where
    checkURL :: String -> IO String
    checkURL path = do
      let url = host </> path
      req <- parseRequest url
      mgr <- newManager tlsManagerSettings
      respHist <- responseOpenHistory req mgr
      let redirect = listToMaybe . reverse $ mapMaybe (lookup "Location" . responseHeaders . snd) $ hrRedirects respHist
      let finalUrl = maybe url B.unpack redirect
      when (isJust redirect) $ putStr "Redirected to "
      let response = hrFinalResponse respHist
      if statusCode (responseStatus response) /= 200
        then
        error' $ show $ responseStatus response
        else do
        body <- brConsume $ responseBody response
        let doc = parseBSChunks body
            cursor = fromDocument doc
            hrefs = concatMap (attribute "href") $ cursor $// element "a"
            mfile = listToMaybe $ filter (editionPrefix edition `T.isPrefixOf`) hrefs :: Maybe Text
        case mfile of
          Nothing -> do
            print doc
            error' $ "not found " ++ finalUrl
          Just file ->
            return $ finalUrl </> T.unpack file

    createSymlink :: FilePath -> FilePath -> IO ()
    createSymlink tgt symlink = do
      createFileLink tgt symlink
      putStrLn $ T.pack $ symlink ++ " -> " ++ tgt

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
