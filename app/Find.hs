-- |
{-# LANGUAGE ViewPatterns #-}
module Find where

import System.Directory
import Control.Monad
import qualified Data.List as DL

-- Recursive listing of all files present in the supplied path
listRecursively :: FilePath -> IO ()
listRecursively path = do
                files <- listDirectory path
                -- remove trailing / if it AlreadyExists
                let ppath = removeTrail path
                            where
                              removeTrail (reverse -> ('/':xs)) = xs
                              removeTrail xs = xs

                let ffiles = map ((++) $ ppath ++ "/") files
                dirs  <- filterM doesDirectoryExist ffiles

                -- First list all the files and then list directories
                -- \\ is the complement operation on lists
                let comp = ffiles DL.\\ dirs

                case not $ null comp of
                  True -> mapM_ putStrLn comp
                  _    -> return ()
                -- Recursively display the directory contents now
                mapM_ listRecursively dirs
