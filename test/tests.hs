import Data.Maybe
import System.Environment (lookupEnv)
import SimpleCmd

dlFedora :: Bool -> [String] -> IO ()
dlFedora ghAction args =
  putStrLn "" >> cmdLog "dl-fedora"
  ((if ghAction then ("-T" :) else id) args)

tests :: Bool -> [[String]]
tests ghAction =
  [["-n", "33", "-c"]
  ,["-n", "rawhide", "silverblue"]
  ,["-n", "respin"]
  ,["-l", "34"]
  ,["-l", "rawhide", "-n"]
  ] ++
  if ghAction then []
  else
    [["-n", "34", "silverblue"]
    ,["-n", "32", "kde"]
    ,["-T", "-n", "33", "everything"]
    ,["-n", "33", "server", "--arch", "aarch64"]
    ]

main :: IO ()
main = do
  ghAction <- isJust <$> lookupEnv "GITHUB_ACTIONS"
  let cases = tests ghAction
  mapM_ (dlFedora ghAction) cases
  putStrLn $ show cases ++ " tests run"
