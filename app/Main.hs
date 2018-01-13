module Main where

{-- External Packages --}
import Control.Monad (when)
import Options.Applicative
import Data.Semigroup
import System.Console.GetOpt
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit
import System.FilePath (isValid)

{-- Internal Packages --}
import Lib

data Options = Options
  { path :: FilePath
  , wordLength :: Int
  , count :: Int
  } deriving (Show)

{-
defaultOptions :: Options
defaultOptions = Options
  { path = defaultPath
  , wordLength = 17
  , count = 1
  }
  -}

defaultPath = ['.'] :: FilePath
wordsFilePath = "/usr/share/dict/words" :: FilePath
wordsFilePath2 = "/usr/share/dict/words" :: FilePath
files = [wordsFilePath, wordsFilePath2] :: [FilePath] -- A directory

main :: IO ()
main = do -- wordFrequency files minWordLength minCount
  args <- getArgs
  print args

-- parseArgs :: IO ([String], Int)
