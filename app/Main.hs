module Main where

{- TODO:
    implement Top 10 functionality
    Handle recursing directories
-}

{-- External Packages --}
import Control.Monad (unless)
import Options.Applicative
import Data.Semigroup ((<>))
import System.Console.GetOpt
import System.Exit
import System.FilePath (isValid)

{-- Internal Packages --}
import Lib

data Options = Options
  { path :: FilePath
  , length :: Int
  , count :: Int
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
entryPoint (Options path length count top) = do
  unless (isValid path) exitFailure
  wordFrequency files length count
  where
    files = [path] -- TODO: Handle recursing directories into a [FilePath]

parseOptions :: Parser Options
parseOptions = Options
  <$> strOption -- See if you can use this as the default arg if no flags are passed in
    (  long "path"
    <> short 'p'
    <> metavar "TARGET"
    <> help "The file or directory to count words for."
    )
  <*> option auto
    (  long "length"
    <> short 'l'
    <> value 4 -- includes 97% of all English words
    <> showDefault
    <> metavar "INT"
    <> help "The minimum length of words to include."
    )
  <*> option auto
    (  long "count"
    <> short 'c'
    <> value 1
    <> metavar "INT"
    <> help "The minimum number of occurences a word must have to be included."
    )
  <*> switch
    (  long "top"
    <> help "Display only the top 10 results."
    )
