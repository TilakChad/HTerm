module FileMatch where

import qualified System.Info as OS

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Control.Exception
import Control.Monad
import Control.Applicative

import Glob
import Terminal as Term
-- returns content of the given directory
listmatch :: String -> IO [String]
listmatch "~" = do
              home <- getHomeDirectory
              getDirectoryContents home

listmatch dirName = do
                if null dirName then
                  do
                  currentDir <- getCurrentDirectory
                  getDirectoryContents currentDir
                else
                  handle ((\_ -> return [])::IOException -> IO [String]) $ do
                  getDirectoryContents dirName

-- Now iterate over all the files and check if the regex glob matches
-- takes pattern and path and returns all the strings
fmatch x y = globmatch x y

-- lmao .. I don't know what's going on here but it worked .. just don't know how yet
filematch :: String -> FilePath -> IO [String]
filematch pattern path = filematch' pattern <$> listmatch path

-- simple function that works with plain strings
filematch' :: String -> [String] -> [String]
filematch' pat = filter (`fmatch` pat)

lsh :: String -> FilePath -> IO ()
lsh pattern path = do
  file <- filematch pattern path
  mapM_ (putStrLn . Term.putString Term.Green) file


interpretPath :: String -> IO String
interpretPath path = do
                  home <- getHomeDirectory
                  -- noice, check prefix using our string matching function here
                  return (Glob.substrmatchLeft home path)
                    -- Replace that substring by ~

-- cat,mat,bat,rat,sat
splitOnChar :: Char -> String -> [String]
splitOnChar ch str = splitIt ch str
                    where
                      splitIt _ []   = []
                      splitIt ch str =
                        let
                          mem = takeWhile (/= ch) str
                        in
                          mem : splitIt ch (drop (length mem + 1) str)

homeDir :: IO String
homeDir = getHomeDirectory
