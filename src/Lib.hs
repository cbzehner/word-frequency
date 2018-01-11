module Lib
    ( wordFrequency
    ) where

import qualified Data.HashMap.Strict as HashMap
import System.IO (hGetContents, IOMode(..), withFile)

data Path = Directory | File

wordsFilePath = "/usr/share/dict/words" :: FilePath

wordFrequency :: IO ()
wordFrequency = do
  withFile wordsFilePath ReadMode (\handle -> do
    contents <- hGetContents handle
    -- TODO: use filterWithKey to filter both short words and low word counts
    putStrLn $ unlines $ map (\(k, v) -> k ++ ": " ++ show v) $ HashMap.toList $
      HashMap.fromListWith (+) (map (\x -> (x, 1)) (lines contents))
                                  )
