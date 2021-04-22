import SimpleCmd

dlFedora :: [String] -> IO ()
dlFedora args =
  putStrLn "" >> cmdLog "dl-fedora" args

tests :: [[String]]
tests =
  [["-n", "33", "-c"]
  ,["-n", "rawhide", "silverblue"]
  ,["-n", "34", "silverblue"]
  ,["-n", "respin"]
  ,["-n", "32", "kde"]
  ,["-n", "33", "everything"]
  ,["-n", "33", "server", "--arch", "aarch64"]
  ,["-l", "34"]
  ,["-l", "rawhide", "-n"]
  ]

main :: IO ()
main = do
  mapM_ dlFedora tests
  putStrLn $ show (length tests) ++ " tests run"
