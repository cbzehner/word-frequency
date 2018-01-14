module Files
  ( getFilesRecursively
  ) where

import Control.Monad (join, mapM)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit
import System.FilePath (combine)

data PathType = Directory | File | Invalid
  deriving (Eq, Ord, Show, Enum)

getFilesRecursively :: FilePath -> IO [FilePath]
getFilesRecursively path = do
  pathType <- getPathType path
  case pathType of
    Invalid -> exitFailure -- TODO: Better error message on invalid paths
    File -> pure [path]
    Directory -> fmap concat $ join $ mapM getFilesRecursively <$> getFilesFromDirectory path

getFilesFromDirectory :: FilePath -> IO [FilePath]
getFilesFromDirectory path = map (combine path) <$> listDirectory path

getPathType path = do
  directoryExists <- doesDirectoryExist path
  fileExists <- doesFileExist path
  if directoryExists
  then pure Directory
  else if fileExists
       then pure File
       else pure Invalid
