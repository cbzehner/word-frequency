module System.Directory.Recursive
  ( findAllFileNames
  ) where

-- TODO:
-- Look at Real World Haskell suggestions for how to improve this.
-- Look into switching from a home rolled solution to directory-tree
-- Filter out dotfiles (folders and files starting with ".")

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

-- | Get all filenames recursively from a given path or directory
-- TODO: Add test for cases like "", "~", "~/", "/" and relative and abs paths.
findAllFileNames :: FilePath -> IO [FilePath]
findAllFileNames path = do
  absolutePath <- expandTilde path
  pathType <- getPathType absolutePath
  case pathType of
    Invalid -> exitFailure -- TODO: Better error message on invalid paths
    File -> pure [absolutePath]
    Directory -> fmap concat $ join $
      mapM findAllFileNames <$> getFilesFromDirectory absolutePath

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

-- | Expand the ~ in a file path with the current user's home directory.
-- TODO: Add test for Empty FilePath and relative and absolute paths. Unicode?
expandTilde :: FilePath -> IO FilePath
expandTilde path = if head (splitPath path) == "~/"
   then do
     home <- getHomeDirectory
     pure $ joinPath $ home : tail (splitPath path)
   else pure path
