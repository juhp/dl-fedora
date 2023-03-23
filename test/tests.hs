import Data.Maybe
import System.Environment (lookupEnv)
import SimpleCmd

dlFedora :: Bool -> [String] -> IO ()
dlFedora ghAction args =
  putStrLn "" >> cmdLog "dl-fedora"
  ((if ghAction then ("-T" :) else id) args)

-- FIXME automate me
branched :: Int
branched = 38
current, previous :: String
current = show branched
previous = show (branched - 1)

tests :: Bool -> [[String]]
tests ghAction =
  [["-n", previous, "-c"]
  ,["-n", "rawhide", "silverblue"]
  ,["-n", "respin"]
  ,["-l", current]
  ,["-l", "rawhide", "-n"]
  ] ++
  if ghAction then []
  else
    [["-n", current, "silverblue"]
    ,["-n", previous, "kde"]
    ,["-T", "-n", current, "everything"]
    ,["-n", previous, "server", "--arch", "aarch64"]
    ]

main :: IO ()
main = do
  ghAction <- isJust <$> lookupEnv "GITHUB_ACTIONS"
  let cases = tests ghAction
  mapM_ (dlFedora ghAction) cases
  putStrLn $ show (length cases) ++ " tests ran"
