{-# LANGUAGE QuasiQuotes #-}

module Merge (merge) where

import           Data.Char           (toUpper)
import           OptArg              (Flag (..))
import           System.Directory
import           System.Exit
import           System.IO
import           Text.RE.TDFA.String

defaultDir   = "../../data/vectis/"

getDir :: [Flag] -> String
getDir []               = defaultDir
getDir ((DirName x):xs) = x
getDir (_:xs)           = getDir xs

getCo  :: [Flag] -> String
getCo []                = ""
getCo ((SettleCo x):xs) = x
getCo (_:xs)            = getCo xs

attachHeader s = header ++ s
                 where header = "\"Settle Batch\",\"Reference No\",\"Settle Co\",\"Doc No\",\"PNR\",\"Sale Date\",\"Orig Sale\",\"Sale Ccy\",\"Sale Amt\",\"Local Amt\",\"Type\",\"Card\",\"Authority\",\"Expires\",\"Agent\",\"Agency Type\",\"External Ref.\"\n"

merge :: [Flag] -> [String] -> IO ()  -- ex) merge [DirName "../../data/vectis/", Settleco "NP"] ["merge"]
merge (xs) (ys) = do
  let settleCo = map toUpper $ getCo xs
  if settleCo == ""
    then hPutStrLn stderr "settleCo must be provided" >> exitWith (ExitFailure 1)
    else return ()

  let dir = getDir xs
  setCurrentDirectory dir
  dirFiles <- getDirectoryContents "."
  re1 <- compileRegex $ "^" ++ settleCo ++ "[0-9]{4}.*csv$"
  let workFiles = filter (\x -> matched $ x ?=~ re1) dirFiles

  putStrLn $ show workFiles
  newText <- mapM transformF workFiles
  writeFile (settleCo ++ "_merged.csv") (attachHeader $ concat newText)
  putStrLn $ "Merge completed ... : total " ++ (show (length workFiles)) ++ " files --> " ++ settleCo ++ "_merged.csv"

transformF :: String -> IO String
transformF f = do
  contents <- readFile f
  let contents' = unlines $ map addFields headerRemovedLines
                  where refBatch = matches $ f *=~ [re|[A-Z0-9]+|]
                        batch = head refBatch
                        ref = last refBatch
                        addFields x = "\"" ++ batch ++ "\",\"" ++ ref ++ "\"," ++ x
                        headerRemovedLines = tail $ lines contents
  return contents'

