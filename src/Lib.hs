module Lib
    ( wordFrequency
    ) where

import Control.Monad (join)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import System.IO (hGetContents, IOMode(..), withFile)
import Data.Hashable (Hashable)

import Files

{-- TODO:
   Order results by count for printing in DESC order
   Support Top results only
-}
wordFrequency :: FilePath -> Int -> Int -> Bool -> IO ()
wordFrequency path wordLength frequency top = do
  files <- getFilesRecursively path
  wordsMap <- readWordsMaps files
  putStrLn $ unlines $ map tupleToStr $ HM.toList $
    filterWordsMap wordLength frequency wordsMap
  {-
  withFile (head files) ReadMode (\handle -> do
    contents <- hGetContents handle
    putStrLn $ unlines $ map tupleToStr $ HM.toList $
      filterWordsMap wordLength frequency $
      fileToWordsMap (lines contents))
    -}
  {-
  putStrLn $ unlines $ map tupleToStr $ HM.toList $
    filterWordsMap wordLength frequency $ readWordsMaps files
    -}


unionsWith :: (Eq k, Hashable k) => (v -> v -> v) -> [HM.HashMap k v] -> HM.HashMap k v
unionsWith f = L.foldl' (HM.unionWith f) HM.empty

-- | Take in a list of words and transform it into a HashMap of words to counts.
fileToWordsMap :: [String] -> HM.HashMap String Int
fileToWordsMap list = HM.fromListWith (+) $ map (\x -> (x, 1)) list

-- | Filter a HashMap of words by key length and count of values.
filterWordsMap :: Int -> Int -> HM.HashMap String Int -> HM.HashMap String Int
filterWordsMap wordLength frequency =
  HM.filterWithKey (\k v -> length k >= wordLength && v >= frequency)

-- | Fetch a HashMap of words to counts for a list of FilePaths.
readWordsMaps :: [FilePath] -> IO (HM.HashMap String Int)
readWordsMaps files = do
  wordsMaps <- mapM readWordsMap files
  pure $ unionsWith (+) wordsMaps

-- | Read a file and return a HashMap of the count of words in the file.
readWordsMap :: FilePath -> IO (HM.HashMap String Int)
readWordsMap file = withFile file ReadMode (\handle -> do
    contents <- hGetContents handle
    pure $ fileToWordsMap (lines contents))

-- | A helper function to `Show` the tuple representation as a string.
tupleToStr :: (Show v) => (String, v) -> String
tupleToStr (k, v) = k ++ ": " ++ show v

