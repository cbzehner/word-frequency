module Lib
    ( wordFrequency
    ) where

import qualified Data.HashMap.Strict as HashMap
import System.IO (hGetContents, IOMode(..), withFile)

data Path = Directory | File

wordFrequency :: [FilePath] -> Int -> Int -> IO ()
wordFrequency files minWordLength minCount = do
  withFile (head files) ReadMode (\handle -> do
    contents <- hGetContents handle
    -- TODO: order probably matters here. Maybe an ordered rather than
    -- unordered data structure will make more sense? Maybe just order after
    -- filtering?
    putStrLn $ unlines $
      map (\(k, v) -> k ++ ": " ++ show v) $ HashMap.toList $
      HashMap.filterWithKey (\k v -> length k >= minWordLength && v >= minCount) $
      HashMap.fromListWith (+) (map (\x -> (x, 1)) (lines contents))

                                  )
