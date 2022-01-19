-- | Some color thingy and basic terminal navigation
module Terminal where

import Prelude hiding (Left,Right)
-- Only for linux now
data Color = Default
        | Red
        | Green
        | Yellow
        | Magenta
        | Blue
        | White
        | Cyan deriving (Ord,Enum,Eq)

instance Show Color where
  show Red     = "\ESC[31m"
  show Green   = "\ESC[32m"
  show Yellow  = "\ESC[33m"
  show Default = "\ESC[0m"
  show _       = "\ESC[0m"

putString :: String -> Color -> String
putString str color = (show color) ++ str ++ (show Default)

data Keys' = Up | Down | Left | Right | Esc | None | Char Char deriving (Show,Ord,Eq)
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
       (ch:xs)  -> Key (Char ch)
       otherwise-> Key None

instance Show Keys where
   show (Key ch) = show ch
