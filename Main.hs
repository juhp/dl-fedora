{-# LANGUAGE NoImplicitPrelude #-}

import BasicPrelude

import Network.HTTP.Simple (httpLBS, getResponseBody, getResponseHeaders, 
                            getResponseStatusCode, parseRequest)

import qualified Data.Text as T

import Options.Applicative (auto)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

import SimpleCmdArgs
--import Paths_fedora_iso_dl (version)

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
  simpleCmdArgs Nothing "Fedora iso downloader" 
    "Tool for downloading Fedora iso file images" $
    findISO <$> 
    strOptionalWith 'a' "arch" "ARCH" "architecture (default x86_64)" "x86_64" <*> 
    optionalWith auto 'e' "edition" "EDITION" "Fedora edition (Workstation [default], Server, ...)" Workstation <*> 
    argumentWith auto "RELEASE"

findISO :: String -> FedoraEdition -> Int -> IO ()
findISO arch edition release = do
  let (relpath, released) = case release of
               30 -> ("development/30", False)
               _ -> if release > 30
                    then ("development/rawhide", False)
                    else ("releases" </> show release, True)
      toppath = "pub/fedora/linux" 
      path = toppath </> relpath </> show edition </> arch </> editionMedia edition
  unless released $ checkDir "https://dl.fedoraproject.org" path
  checkDir "https://download.fedoraproject.org" path
  where
    checkDir :: Text -> String -> IO ()
    checkDir host path = do
      let url = T.unpack host </> path
      response <- parseRequest url >>= httpLBS
      if getResponseStatusCode response /= 200
        then do
        print url
        print $ getResponseHeaders response
        else do
        let cursor = fromDocument $ parseLBS $ getResponseBody response
            hrefs = concatMap (attribute "href") $ cursor $// element "pre" &// element "a" -- &/ attribute "href"
            res = filter (editionPrefix edition `T.isPrefixOf`) hrefs :: [Text]
        case res of
          [] -> do
            putStrLn $ "not found " <> T.pack url
            mapM_ putStrLn hrefs
            print $ getResponseHeaders response
          files -> mapM_ (putStrLn . (<> " (" <> host <> ")")) files

editionPrefix :: FedoraEdition -> Text
editionPrefix Workstation = "Fedora-Workstation-Live"
editionPrefix Server = "Fedora-Server-dvd"
editionPrefix Silverblue = "Fedora-Silverblue-ostree"
editionPrefix Everything = "Fedora-Everything-netinst"
editionPrefix Container = "Fedora-Container-Base"
editionPrefix _ = error "Edition not yet supported"

editionMedia :: FedoraEdition -> String
editionMedia Container = "images"
editionMedia _ = "iso"
