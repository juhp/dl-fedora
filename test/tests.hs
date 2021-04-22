import Data.Maybe
import System.Environment (lookupEnv)
import SimpleCmd

dlFedora :: [String] -> IO ()
dlFedora args =
  putStrLn "" >> cmdLog "dl-fedora" ("-T" : args)

tests :: Maybe String -> [[String]]
tests mGhAction =
  [["-n", "33", "-c"]
  ,["-n", "rawhide", "silverblue"]
  ,["-n", "respin"]
  ,["-l", "34"]
  ,["-l", "rawhide", "-n"]
  ] ++
  if isJust mGhAction then []
  else
    [["-n", "34", "silverblue"]
    ,["-n", "32", "kde"]
    ,["-n", "33", "everything"]
    ,["-n", "33", "server", "--arch", "aarch64"]
    ]

main :: IO ()
main = do
  mGhAction <- lookupEnv "GITHUB_ACTIONS"
  let cases = tests mGhAction
  mapM_ dlFedora cases
  putStrLn $ show cases ++ " tests run"
