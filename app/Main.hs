module Main where

import Lib

wordsFilePath = "/usr/share/dict/words" :: FilePath
wordsFilePath2 = "/usr/share/dict/words" :: FilePath
files = [wordsFilePath, wordsFilePath2] :: [FilePath] -- A directory
minWordLength = 17
minCount = 1

main :: IO ()
main = wordFrequency files minWordLength minCount
