module Lib
    ( wordFrequency
    ) where

import qualified Data.HashMap.Strict as HM
import System.IO (hGetContents, IOMode(..), withFile)

data Path = Directory | File

{-- TODO:
   Support multiple files
   Order results by count for printing in DESC order
-}
wordFrequency :: [FilePath] -> Int -> Int -> IO ()
wordFrequency files wordLength frequency =
  withFile (head files) ReadMode (\handle -> do
    contents <- hGetContents handle
    putStrLn $ unlines $ map tupleToStr $ HM.toList $
      filterWordsMap wordLength frequency $
      fileToWordsMap (lines contents)
                                 )

tupleToStr :: (Show v) => (String, v) -> String
tupleToStr (k, v) = k ++ ": " ++ show v

fileToWordsMap :: [String] -> HM.HashMap String Int
fileToWordsMap list = HM.fromListWith (+) $ map (\x -> (x, 1)) list

filterWordsMap :: Int -> Int -> HM.HashMap String Int -> HM.HashMap String Int
filterWordsMap wordLength frequency =
  HM.filterWithKey (\k v -> length k >= wordLength && v >= frequency)
