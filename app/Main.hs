module Main where

-- External Packages
import Control.Monad (unless, when)
import Options.Applicative
import Data.Semigroup ((<>))
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit

-- Internal Packages
import Lib

data Options = Options
  { path :: FilePath
  , length :: Int
  , frequency :: Int
  , top10 :: Bool
  } deriving (Show)

-- wordsFilePath = "/usr/share/dict/words" :: FilePath

main :: IO ()
main = entryPoint =<< execParser options
  where
    options = info (parseOptions <**> helper)
      (  fullDesc
      <> progDesc "Count the frequency of words in a file or directory."
      <> header "word-frequency - counting manually is overrated"
      )

-- TODO: A better name
entryPoint :: Options -> IO ()
entryPoint (Options path length frequency top) = do
  pathType <- getPathType path
  -- TODO: Better error message on invalid paths
  when (pathType == Invalid) exitFailure
  wordFrequency path pathType length frequency top
  exitSuccess

getPathType :: FilePath -> IO PathType
getPathType path = do
  directoryExists <- doesDirectoryExist path
  fileExists <- doesFileExist path
  if directoryExists
  then pure Directory
  else if fileExists
       then pure File
       else pure Invalid

parseOptions :: Parser Options
parseOptions = Options
  <$> argument str
    (  metavar "TARGET"
    <> help "The file or directory to count words for."
    )
  <*> option auto
    (  long "length"
    <> short 'l'
    <> value 4 -- includes 97% of all English words
    <> showDefault
    <> metavar "INT"
    <> help "The minimum length of words to include. It's often useful to omit prepositions."
    )
  <*> option auto
    (  long "frequency"
    <> short 'f'
    <> value 1
    <> metavar "INT"
    <> help "The minimum number of occurences a word must have to be included."
    )
  <*> switch
    (  long "top"
    <> help "Display only the top 10 results."
    )
