module System.Directory.Recursive
  ( findAllFileNames
  ) where

-- TODO:
-- Look at Real World Haskell suggestions for how to improve this.
-- Look into switching from a home rolled solution to directory-tree

import Control.Monad (join, mapM)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , listDirectory
  )
import System.Exit
import System.FilePath (combine, joinPath, splitPath)

data PathType = Directory | File | Invalid
  deriving (Eq, Ord, Show, Enum)

findAllFileNames :: FilePath -> IO [FilePath]
findAllFileNames path = do
  absolutePath <- expandTilde path
  pathType <- getPathType absolutePath
  case pathType of
    Invalid -> exitFailure -- TODO: Better error message on invalid paths
    File -> pure [path]
    Directory -> fmap concat $ join $
      mapM findAllFileNames <$> getFilesFromDirectory path

getFilesFromDirectory :: FilePath -> IO [FilePath]
getFilesFromDirectory path = map (combine path) <$> listDirectory path

getPathType :: FilePath -> IO PathType
getPathType path = do
  directoryExists <- doesDirectoryExist path
  fileExists <- doesFileExist path
  if directoryExists
  then pure Directory
  else if fileExists
       then pure File
       else pure Invalid

expandTilde :: FilePath -> IO FilePath
expandTilde path = if head (splitPath path) == "~/"
   then do
     home <- getHomeDirectory
     pure $ joinPath $ home : tail (splitPath path)
   else pure path
