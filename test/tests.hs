import Control.Monad (forM_, unless)
import Data.List (sort)
import Data.Maybe
import Numeric.Natural (Natural)
import System.Environment (lookupEnv)
import SimpleCmd

dlFedora :: Bool -> [String] -> IO ()
dlFedora ghAction args =
  putStrLn "" >> cmdLog "dl-fedora"
  ((if ghAction then ("-d" :) . ("-T" :) else id) args)

-- FIXME automate me
branched :: Int
branched = 42
current, previous :: String
current = show branched
previous = show (branched - 1)

tests :: Bool -> [[String]]
tests ghAction =
  [["-n", previous, "--checksum"]
  ,["-n", "rawhide", "silverblue"]
  ,["-c", "respin"]
  ,["-l", current]
  ,["-l", "rawhide", "-n"]
  ,["-n", previous, "kde"]
  ] ++
  if ghAction then []
  else
    [["-c", current, "silverblue"]
    ,["-T", "-n", current, "everything"]
    ,["-n", previous, "server", "--dvd", "--arch", "aarch64"]
    ,["41", "iot", "-n"]
    ,["-n", "c8s"]
    ,["-n", "c9s", "--dvd"]
    ,["-n", "c10s"]
    ,["-c", "eln"]
    ,["-n", "c9s-live"]
    ,["-n", "c10s-live"]
    ,["-d", "-T", "-n", "40", "--all-editions"]
    ,["-d", "-T", "-n", "41", "--all-editions"]
    ,["-d", "-T", "-n", "42", "--all-editions"]
    ,["-d", "-T", "-n", "43", "--all-editions"]
    ,["-d", "-T", "-n", "respin", "--all-editions"]
    ,["-T", "-n", "c9s-live", "--all-editions"]
    ,["-T", "-n", "c10s-live", "--all-editions"]
    ]

allEditions :: Natural -> [String]
allEditions 9 = allEditions 10 ++ ["cinnamon", "mate", "xfce"]
allEditions 10 = ["workstation", "kde", "max", "min"]
allEditions rel = ["cloud","container","everything","server","workstation","budgie","cinnamon","i3","kde","lxde","lxqt","mate","soas","sway","xfce","silverblue","kinoite","onyx","sericea"] ++
  ["cosmic" | rel >= 42] ++ ["kdemobile" | rel >= 41] ++ ["miracle" | rel >= 41] ++ ["iot" | rel < 42]

listEditions :: Natural -> IO ()
listEditions n = do
  let args = ["--list", show n ++ if n < 11 then "-live" else ""]
  cmdN "dl-fedora" args
  es <- sort . words <$> cmd "dl-fedora" args
  let aes = sort $ allEditions n
  unless (es == aes) $
    error' $ show n +-+ "editions unmatched:\n" ++ show es ++ "\n" ++ show aes

main :: IO ()
main = do
  forM_ [40..43] listEditions
  forM_ [9..10] listEditions
  ghAction <- isJust <$> lookupEnv "GITHUB_ACTIONS"
  let cases = tests ghAction
  mapM_ (dlFedora ghAction) cases
  putStrLn $ show (length cases) ++ " tests ran"
