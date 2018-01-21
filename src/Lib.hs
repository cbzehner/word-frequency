module Lib
    ( wordFrequency
    ) where

import Control.Monad (forM)
import qualified Data.HashMap.Strict as HM

import System.Directory.Recursive (findAllFileNames)

{-- TODO:
   Order results by count for printing in DESC order
   Support Top results only
   Write tests and integrate with Stack
   Test on Windows (developed on OSX)
   Convert to ByteString instead of String (and profile)
-}

-- | Get the count of words in a file or recursively in a directory.
wordFrequency :: FilePath -> Int -> Int -> Bool -> IO ()
wordFrequency path wordLength frequency top = do
  files <- findAllFileNames path
  filesContents <- forM files readFile
  printWords . backToTuples . filterWordsMap wordLength frequency $
    createWordsMap filesContents
  where
    printWords tuples = putStrLn $ unlines tuples
    backToTuples words = map tupleToStr $ HM.toList words
    createWordsMap filesText = wordsCountMap $ lines $ concat filesText

-- | Take in a list of words and transform it into a HashMap of words to counts.
wordsCountMap :: [String] -> HM.HashMap String Int
wordsCountMap list = HM.fromListWith (+) $ map (\x -> (x, 1)) list

-- | Filter a HashMap of words by key length and count of values.
filterWordsMap :: Int -> Int -> HM.HashMap String Int -> HM.HashMap String Int
filterWordsMap wordLength frequency =
  HM.filterWithKey (\k v -> length k >= wordLength && v >= frequency)

-- | A helper function to `Show` the tuple representation as a string.
tupleToStr :: (Show v) => (String, v) -> String
tupleToStr (k, v) = k ++ ": " ++ show v

