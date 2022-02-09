--
{-# LANGUAGE ViewPatterns #-}

module Find where

import Control.Monad
import qualified Data.List as DL
import qualified GHC.Show as Term
import System.Directory (doesDirectoryExist)
import qualified System.Directory as Dir
import qualified Terminal as Term

-- Recursive listing of all files present in the supplied path
listRecursively :: FilePath -> IO ()
listRecursively path = do
  files <- Dir.listDirectory path
  -- remove trailing / if it AlreadyExists
  let ppath = removeTrail path
        where
          removeTrail (reverse -> ('/' : xs)) = xs
          removeTrail xs = xs

  let ffiles = map ((++) $ ppath ++ "/") files
  dirs <- filterM doesDirectoryExist ffiles

  -- First list all the files and then list directories
  -- \\ is the complement operation on lists
  let comp = ffiles DL.\\ dirs

  case not $ null comp of
    True -> mapM_ putStrLn comp
    _ -> return ()
  -- Recursively display the directory contents now
  mapM_ listRecursively dirs

-- lists directory in blue and other file in yellow
listFilesFolders :: FilePath -> IO ()
listFilesFolders path = do
  files <- Dir.listDirectory path

  let fpath = removeTrail path
        where
          removeTrail (reverse -> ('/' : xs)) = reverse xs
          removeTrail xs = xs

  let ffiles = map ((++) $ fpath ++ "/") files

  -- A lot of zipping dropping going on, but lets not think about how expensive those operations are
  let filesZip = zip files ffiles
  -- Remove directories and list only files
  -- Use the extended path to check for their existence
  dirs <- filterM (\(_, b) -> Dir.doesDirectoryExist b) filesZip
  -- putStrLn (show $ length dirs)
  -- print them blues and fles as yellows for now
  let filteredFiles = map fst dirs

  mapM_ (putStrLn . Term.putString Term.Blue) filteredFiles
  mapM_ (putStrLn . Term.putString Term.Red) (files DL.\\ filteredFiles)

-- returns list of files and folders to the caller inside IO Monad
getFilesAndFolders :: FilePath -> IO ([String], [String])
getFilesAndFolders path = do
  files <- Dir.listDirectory path

  let fpath = removeTrail path
        where
          removeTrail (reverse -> ('/' : xs)) = reverse xs
          removeTrail xs = xs

  let ffiles = map ((++) $ fpath ++ "/") files

  -- mapM_ putStrLn ffiles
  -- A lot of zipping dropping going on, but lets not think about how expensive those operations are
  let filesZip = zip files ffiles
  -- Remove directories and list only files
  -- Use the extended path to check for their existence
  dirs <- filterM (\(_, b) -> Dir.doesDirectoryExist b) filesZip

  let filteredFiles = map fst dirs
  return (filteredFiles, files DL.\\ filteredFiles)
