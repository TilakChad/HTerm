-- | Some color thingy and basic terminal navigation
module Terminal where

import Prelude hiding (Left,Right)
import qualified Data.Map as Map

-- Only for linux now
data Color = Default
        | Red
        | Green
        | Yellow
        | Magenta
        | Blue
        | White
        | Gray
        | Cyan deriving (Ord,Enum,Eq)

instance Show Color where
  show Red     = "\ESC[31m"
  show Green   = "\ESC[32m"
  show Yellow  = "\ESC[33m"
  show Blue    = "\ESC[34m"
  show Default = "\ESC[0m"
  show Gray    = "\ESC[90m"
  show Cyan    = "\ESC[96m"
  show _       = "\ESC[0m"

putString :: Color -> String -> String
putString color str = (show color) ++ str ++ (show Default)

data Keys' = Up | Down | Left | Right | Esc | None | Enter | Tab | BackSpace | Char Char deriving (Show,Ord,Eq)
data Keys  = Key Keys' deriving (Ord,Eq)

-- let's delay this whole parser
-- instance Read Keys where

-- and go with simple function parsing
fromStr :: String -> Keys
fromStr str =
  case str of
       "\ESC[A" -> Key Up
       "\ESC[B" -> Key Down
       "\ESC[C" -> Key Right
       "\ESC[D" -> Key Left
       -- ch:xs  -> Key ch
       "\n"     -> Key Enter
       "\ESC"   -> Key Esc
       "\t"     -> Key Tab
       "\DEL"   -> Key BackSpace
       (ch:xs)  -> Key (Char ch)
       otherwise-> Key None

instance Show Keys where
   show (Key ch) = show ch

--  This much for Haskell today .. let's move toward Rust

-- Now generate terminal layout for our virtual environments

-- Maybe I need to dive into state monads for these things
-- currentpath >> current buffer contents
--                Option1         Option2
--                Option3         Option4

-- use only when something changes
clearTerminal :: IO ()
clearTerminal = return ()

drawLayout :: String -> String -> IO ()
drawLayout path _ = do
  clearTerminal
  putStr (putString Green path)

extension :: [[String]]
-- ig, at this point, it would be simple to use plain array
extension = [["c","cpp","hs","rs","txt"],["png","jpg","jpeg"], ["pdf"], ["html","js","css"]]

program :: [String]
program = ["emacs", "eog", "evince", "google-chrome"]

args :: [[String]]
args = [["-nw"],[],[],[],[]]

executionMap :: Map.Map [String] String
executionMap = Map.fromList (zip extension program)

argMap :: Map.Map String [String]
argMap = Map.fromList (zip program args)


data CursorControl = CursorLeft | CursorRight | CursorUp | CursorDown

data CursorShape = BlinkBlock | SteadyBlock | BlinkUnderline | SteadyUnderLine | BlinkBar | SteadyBar | CursorDefault

instance Show CursorShape where
  show BlinkBlock       = "\ESC[1 q"
  show SteadyBlock      = "\ESC[2 q"
  show BlinkUnderline   = "\ESC[3 q"
  show SteadyUnderLine  = "\ESC[4 q"
  show BlinkBar         = "\ESC[5 q"
  show SteadyBar        = "\ESC[6 q"
  show _                = "\ESC[0 q"

changeCursor :: CursorShape -> IO ()
changeCursor cs = putStr $ show cs

moveCursorBack :: Int -> IO ()
moveCursorBack n = putStr $ "\ESC[" ++ show n ++ "D"
