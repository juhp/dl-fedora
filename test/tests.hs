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
    ,["-d", "-T", "-n"] ++ allEditions 40
    ,["-d", "-T", "-n"] ++ allEditions 41
    ,["-d", "-T", "-n"] ++ allEditions 42
    ,["-d", "-T", "-n"] ++ allEditions 43
    ,["-n", "c9s-live"]
    ,["-n", "c10s-live"]
    ]

allEditions :: Natural -> [String]
allEditions rel = show rel : ["cloud","container","everything","server","workstation","budgie","cinnamon","i3","kde","lxde","lxqt","mate","soas","sway","xfce","silverblue","kinoite","onyx","sericea"] ++
  ["cosmic" | rel >= 42] ++ ["kdemobile" | rel >= 41] ++ ["miracle" | rel >= 41] ++ ["iot" | rel < 42]

main :: IO ()
main = do
  ghAction <- isJust <$> lookupEnv "GITHUB_ACTIONS"
  let cases = tests ghAction
  mapM_ (dlFedora ghAction) cases
  putStrLn $ show (length cases) ++ " tests ran"
