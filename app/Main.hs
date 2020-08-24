module Main where

import           Merge                 (merge)
import           OptArg                (Flag (..), options)
import           Split                 (split)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO


main :: IO ()
main = do
  args <- getArgs
  parseArgs args


parseArgs :: [String] -> IO ()
parseArgs argv = do
  case parse argv of
    (o, n, [])
       | Help    `elem` o    -> help
       | Version `elem` o    -> putStrLn "Haskell utils for ccar reconcile -- version 0.1"
       | "merge" `elem` n    -> merge o n
       | "split" `elem` n    -> split o n
       | otherwise           -> help
    ([], n, _)               -> help
    (o, [], _)               -> help
    (_, _, errs)             -> die errs
 where
   parse argv = getOpt Permute options argv
   header     = "Usage: utils [-h] [-v] [--dir directory] [--file filename] [--co settle_co] merge | split\n\
                \     * utils --dir d:\\jprs\\data\\vectis\\ --co NP merge\n\
                \     * utils --dir ..\\..\\data\\vectis\\ --file VectisReport.csv split\n"
   info       = usageInfo header options
   dump       = hPutStrLn stderr
   die errs   = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
   help       = dump info                  >> exitWith ExitSuccess

