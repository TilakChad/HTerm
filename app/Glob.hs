-- A utility that will match glob patterns with the provided strings .. not using TDFA
-- A little background on globbing patterns (not the regex ones)
-- \*  -> matches any number of characters in the strings
-- \?  -> matches any single character
-- \[...] -> matches a single character inside it
-- \[?-?] -> matches a range of character
-- \[!...] -> doesnot match one character specified

module Glob where

-- guess, I should first implement KMP pattern matchng algorithm or maybe not yet

simplematch :: String -> String -> Bool
simplematch str pattern =
  fnmatch str pattern
  where
    fnmatch [] []    = True
    fnmatch xs []    = False
    fnmatch [] ['*'] = True
    fnmatch [] ys    = False
    fnmatch (x:xs)(y:ys) | x == y    = True && fnmatch xs ys
                         | y == '?'  = True && fnmatch xs ys
                         | y == '*'  = anymatch (x:xs) ys
                         | otherwise = False

-- Takes the remaining portion of the first string and try to match with remaining portion of second string
anymatch :: String -> String -> Bool
anymatch str []         = True
anymatch []  pat        = False
anymatch (x:xs) (y:ys)  | x == y    = simplematch xs ys
                        | otherwise = anymatch xs (y:ys)

-- Check recursive suffix function
-- Again it boils down to the same function as above but differently
-- Matching Kleene star is a pain
issuffix :: String -> String -> Bool
issuffix substr str = False
