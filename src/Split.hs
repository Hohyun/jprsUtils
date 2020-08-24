-- |

module Split (split) where

import           Data.Char           (toUpper)
import           OptArg              (Flag (..))
import           System.Directory
import           System.Exit
import           System.IO
import           Text.RE.TDFA.String

split :: [Flag] -> [String] -> IO ()
split = undefined
