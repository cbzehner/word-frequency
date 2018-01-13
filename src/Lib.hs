module Lib
    ( wordFrequency
    ) where

import Control.Monad (when)
import qualified Data.HashMap.Strict as HM
import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO (hGetContents, IOMode(..), withFile)
import System.Exit

data PathType = Directory | File | Invalid
  deriving (Eq, Ord, Show, Enum)

{-- TODO:
   Support multiple files inside a directory
   Order results by count for printing in DESC order
   Support Top results only
-}
wordFrequency :: FilePath -> Int -> Int -> Bool -> IO ()
wordFrequency path wordLength frequency top = do
  files <- getFiles path
  withFile (head files) ReadMode (\handle -> do
    contents <- hGetContents handle
    putStrLn $ unlines $ map tupleToStr $ HM.toList $
      filterWordsMap wordLength frequency $
      fileToWordsMap (lines contents)
                                 )
getFiles :: FilePath -> IO [FilePath]
getFiles path = do
  pathType <- getPathType path
  case pathType of
    Invalid -> exitFailure -- TODO: Better error message on invalid paths
    File -> pure [path]
    Directory -> pure [path, path] -- TODO: Write a recursive funciton to get directory contents

getPathType :: FilePath -> IO PathType
getPathType path = do
  directoryExists <- doesDirectoryExist path
  fileExists <- doesFileExist path
  if directoryExists
  then pure Directory
  else if fileExists
       then pure File
       else pure Invalid

tupleToStr :: (Show v) => (String, v) -> String
tupleToStr (k, v) = k ++ ": " ++ show v

fileToWordsMap :: [String] -> HM.HashMap String Int
fileToWordsMap list = HM.fromListWith (+) $ map (\x -> (x, 1)) list

filterWordsMap :: Int -> Int -> HM.HashMap String Int -> HM.HashMap String Int
filterWordsMap wordLength frequency =
  HM.filterWithKey (\k v -> length k >= wordLength && v >= frequency)
