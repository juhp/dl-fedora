import Data.Maybe
import System.Environment (lookupEnv)
import SimpleCmd

dlFedora :: Bool -> [String] -> IO ()
dlFedora ghAction args =
  putStrLn "" >> cmdLog "dl-fedora"
  ((if ghAction then ("-T" :) else id) args)

tests :: Bool -> [[String]]
tests ghAction =
  [["-n", "35", "-c"]
  ,["-n", "rawhide", "silverblue"]
  ,["-n", "respin"]
  ,["-l", "36"]
  ,["-l", "rawhide", "-n"]
  ] ++
  if ghAction then []
  else
    [["-n", "36", "silverblue"]
    ,["-n", "34", "kde"]
    ,["-T", "-n", "35", "everything"]
    ,["-n", "35", "server", "--arch", "aarch64"]
    ]

main :: IO ()
main = do
  ghAction <- isJust <$> lookupEnv "GITHUB_ACTIONS"
  let cases = tests ghAction
  mapM_ (dlFedora ghAction) cases
  putStrLn $ show (length cases) ++ " tests ran"
