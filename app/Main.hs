module Main where

import Find as F
import qualified Terminal as Term
import qualified Glob as GlobPattern
import qualified FileMatch as FMatch
import qualified Data.List as DL
import qualified Glob

import Control.Concurrent
import Control.Monad
import System.Process
import System.IO
import Data.List
import Data.Map as Map(keys,lookup)

data Content     = File String | Folder String | Empty

readKey :: IO [Char]
readKey = reverse <$> readKey' ""
  where
    readKey' str = do
      char   <- getChar
      remain <- hReady stdin
      if remain then
        readKey' (char:str)
        else
        return (char:str)



main :: IO ()
main = do
  -- Let use haskell lazyness for reading keyevents
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False

  -- Virtual Environment
  -- get key asynchronously and try to interpret it, right...


  -- Toward some terminal emulation thingy now
  let currentPath = "./"
  Term.drawLayout currentPath ""

  -- start of the infinite loop .. Don't know how to quit this loop yet .. but
  Term.changeCursor Term.SteadyBar
  renderTerminal currentPath "" True

update :: String -> Char -> String
update acc ch = acc ++ [ch]

findmatch :: FilePath -> String -> IO Content
findmatch path "" = return Empty
findmatch path buffer = do
            (dirs,files) <- F.getFilesAndFolders path
            -- List files and then folders
            let ffound = filter (`GlobPattern.globmatch` (buffer ++ "*")) files
            let dfound = filter (`GlobPattern.globmatch` (buffer ++ "*")) dirs

            return $ if null ffound && null dfound then Empty
                     else
                     if null ffound then
                       Folder $ head dfound
                       else
                       File $ head ffound


listContents :: String -> String -> IO String
listContents path buffer = do
                        let word = words buffer
                        if length word > 1 && head word == "list" then
                          do
                            FMatch.lsh (word !! 1) path
                            return ""
                          else
                          do
                            listFilesFolders path
                            return buffer
                        

tabAction :: String -> String -> Content -> IO ()
tabAction path buffer Empty = do
                            -- before that, see if it matches list command
                            putStrLn ""
                            nbuffer <- listContents path buffer
                            -- listFilesFolders path
                            renderTerminal path nbuffer True

tabAction path _ (File file) = do
                            putStrLn $ "\nExecuting file " ++ file -- TODO :: Execute the file
                            -- find the process first
                            -- Take the file extension
                            let extension = last $ FMatch.splitOnChar '.' file
                            -- Loop over all the available keys of the executor table
                            let executorKey  = filter (extension `elem`) (Map.keys Term.executionMap)

                            -- let executor = Map.lookup (head executorKey) Term.executionMap
                            -- map putStrLn executorKey
                            processAction executorKey $ path ++ file
                            -- (_,_,_,handle) <- createProcess (proc "emacs" ["-nw", path ++ file])
                            -- waitForProcess handle
                            return ()


tabAction path _ (Folder folder) = -- change the directory
                                renderTerminal (path ++ folder ++ "/") "" True

processAction :: [[String]] -> String -> IO ()
processAction  []    _   = putStrLn "No program to execute such file"

processAction (x:_) file = execute (Map.lookup x Term.executionMap)
                            where
                              execute Nothing        = putStrLn "No program to execute such file"
                              execute (Just program) = do
                                                  let argsLook = Map.lookup program Term.argMap
                                                  (_,_,_,handle) <- createProcess (proc program $ programArgs argsLook ++ [file])
                                                  _ <- waitForProcess handle
                                                  return ()
programArgs :: Maybe [String] -> [String]
programArgs Nothing  = []
programArgs (Just a) = a

parentPath :: String -> String
parentPath path = mconcat (mappend strs ["/"])
                  where
                    split = FMatch.splitOnChar '/' path
                    strs  = Data.List.intersperse "/" $ (reverse . drop 1. reverse) split

renderTerminal :: String -> String -> Bool -> IO ()
renderTerminal path buffer bRender = do
      isready <- hReady stdin
      if isready then
        do
          keyPress <- readKey
          let key = Term.fromStr keyPress
--          print keyPress
          case key of
            Term.Key (Term.Char '/')  -> -- See if last content is ../, then switch back one directory back
                                      do
                                        if ".." `isSuffixOf` buffer then
                                           renderTerminal (parentPath path) "" True
                                         else if "~" `isSuffixOf` buffer then
                                          do
                                            home <- FMatch.homeDir
                                            renderTerminal (home++"/")  "" True
                                          else
                                           renderTerminal path (buffer ++ "/" ) True

            Term.Key (Term.Char ch)   -> renderTerminal path (buffer++[ch]) True -- with each key press fetch data from the local repository and show them in various coloring ... later
            Term.Key (Term.BackSpace) -> renderTerminal path (reverse .drop 1. reverse $  buffer) True
            Term.Key (Term.Esc)       -> return ()
            Term.Key (Term.Tab)       -> -- read from the current buffer and execute task depending on that
                                       do
                                        str <- renderSearch path buffer
                                        tabAction path buffer str

            _ -> renderTerminal path buffer False
        else
          do
            case bRender of
                   True -> do
                       putStr ("\ESC[2K \CR" ++ Term.putString Term.Red path ++ " " ++ buffer)
                       renderPath path buffer
                       renderSearch path buffer
                       return ()
                   _    -> return ()
            threadDelay 10000
            renderTerminal path buffer False


renderResult :: String -> Content -> IO ()
renderResult _             Empty = return ()
renderResult buffer (File str)   = (putStr . Term.putString Term.Cyan $ str DL.\\ buffer)  >> Term.moveCursorBack (length $ str DL.\\ buffer)
renderResult buffer (Folder str) = (putStr . Term.putString Term.Cyan $ str DL.\\ buffer)  >> Term.moveCursorBack (length $ str DL.\\ buffer)


renderSearch :: FilePath -> String -> IO Content
-- In the current directory find all the files which matches with current buffer
renderSearch path buffer = do
            -- (files,dirs) <- F.getFilesAndFolders path
            -- List files and then folders
            -- let found = filter (`GlobPattern.globmatch` (buffer ++ "*")) files
            found <- findmatch path buffer
            -- Take the current buffer, found string and render the remaining character in gray
            -- if not.null $ found then
            --   putStrLn . Term.putString Term.Gray $ head found DL.\\ buffer
            --   else
            --  return ()

            -- unless (null found || null buffer)
            --    $ putStr . Term.putString Term.Cyan $ found DL.\\ buffer
            unless (null buffer)
              $ renderResult buffer found
            return found
            -- mapM_ (putStrLn . Term.putString Term.Blue) $ filter (`GlobPattern.globmatch` (buffer++"*")) files
            -- mapM_ (putStrLn . Term.putString Term.Red)  $ filter (`GlobPattern.globmatch` (buffer++"*")) dirs
renderPath :: FilePath -> String -> IO ()
renderPath path buffer = do
              -- if file path is home, replace suffix with ~
        home <- FMatch.homeDir
        if home `isPrefixOf` path then
          putStr ("\ESC[2K \CR" ++ Term.putString Term.Blue ("~" ++ Glob.substrmatchLeft home path) ++ ">> " ++ buffer)
          else
            putStr ("\ESC[2K \CR" ++ Term.putString Term.Blue path ++ ">> " ++ buffer)
