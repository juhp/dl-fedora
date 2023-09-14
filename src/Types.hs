module Types where


data Mode = ModeCheck
          -- (Dryrun, Run)
          | ModeLocal Bool Bool
          | ModeDownload Bool Bool
