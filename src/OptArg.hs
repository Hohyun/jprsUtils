-- |

module OptArg (Flag (..), options) where

import           System.Console.GetOpt

data Flag = Help | Version | DirName String | FileName String | SettleCo String
            deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"]     (NoArg Help)
                   "Show this help message",
            Option ['v'] ["version"]  (NoArg Version)
                   "Show version number",
            Option [] ["dir"]  (ReqArg (\s -> DirName  s) "directory")
                   "Directory name contains files to be processed (default ..\\..\\data\\vectis\\)",
            Option [] ["file"] (ReqArg (\s -> FileName s) "filename")
                   "Filename to be processed",
            Option [] ["co"]   (ReqArg (\s -> SettleCo s) "settle_co")
                   "Settle card company code (eg. BC, CN, NP, NK...)" ]

