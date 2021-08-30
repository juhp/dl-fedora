import Data.Maybe
import System.Environment (lookupEnv)
import SimpleCmd

dlFedora :: Bool -> [String] -> IO ()
dlFedora ghAction args =
  putStrLn "" >> cmdLog "dl-fedora"
  ((if ghAction then ("-T" :) else id) args)

tests :: Bool -> [[String]]
tests ghAction =
  [["-n", "34", "-c"]
  ,["-n", "rawhide", "silverblue"]
  ,["-n", "respin"]
  ,["-l", "35"]
  ,["-l", "rawhide", "-n"]
  ] ++
  if ghAction then []
  else
    [["-n", "35", "silverblue"]
    ,["-n", "33", "kde"]
    ,["-T", "-n", "34", "everything"]
    ,["-n", "34", "server", "--arch", "aarch64"]
    ]

main :: IO ()
main = do
  ghAction <- isJust <$> lookupEnv "GITHUB_ACTIONS"
  let cases = tests ghAction
  mapM_ (dlFedora ghAction) cases
  putStrLn $ show cases ++ " tests run"
