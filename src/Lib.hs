module Lib
    ( wordFrequency
    ) where

import System.IO (hGetContents, IOMode(..), withFile)
import Data.HashMap.Strict (fromListWith)

data Path = Directory | File

wordsFilePath = "/usr/share/dict/words" :: FilePath

wordFrequency :: IO ()
wordFrequency = do
  withFile wordsFilePath ReadMode (\handle -> do
    contents <- hGetContents handle
    putStrLn contents
    putStrLn $ lines contents
    putStrLn $ fromListWith (+) (map (\x -> (x, 1)) (lines contents))
                                  )
