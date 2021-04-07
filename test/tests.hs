import SimpleCmd

dlFedora :: [String] -> IO ()
dlFedora args =
  putStrLn "" >> cmdLog "dl-fedora" args

tests :: [[String]]
tests =
  [["-n", "33", "-c"]
  ,["-n", "rawhide", "-e", "silverblue"]
  ,["-n", "34", "-e", "silverblue"]
  ,["-n", "respin"]
  ,["-n", "32", "-e", "kde"]
  ,["-n", "33", "-e", "everything"]
  ,["-n", "33", "-e", "server", "--arch", "aarch64"]
  ,["-l", "34"]
  ]

main :: IO ()
main = do
  mapM_ dlFedora tests
  putStrLn $ show (length tests) ++ " tests run"
