-- A utility that will match glob patterns with the provided strings .. not using TDFA
-- A little background on globbing patterns (not the regex ones)
-- \*  -> matches any number of characters in the strings
-- \?  -> matches any single character
-- \[...] -> matches a single character inside it
-- \[?-?] -> matches a range of character
-- \[!...] -> doesnot match one character specified

module Glob where

-- guess, I should first implement KMP pattern matchng algorithm or maybe not yet
substrmatchLeft :: String -> String -> String
substrmatchLeft substr str = if globmatch str (substr ++ "*") then
                                drop (length substr) str
                               else
                                str

  
globmatch :: String -> String -> Bool
globmatch str pattern =
  fnmatch str pattern
  where
    fnmatch [] []    = True
    fnmatch xs []    = False
    fnmatch [] ['*'] = True
    fnmatch [] ys    = False
    fnmatch (x:xs)(y:ys) | x == y    = True && fnmatch xs ys
                         | y == '?'  = True && fnmatch xs ys
                         | y == '*'  = anymatch (x:xs) ys
                         | y == ']'  = error "] without opening ["
                         | y == '['  = matchclass (x:xs) (y:ys)
                         | otherwise = False

-- Takes the remaining portion of the first string and try to match with remaining portion of second string
anymatch :: String -> String -> Bool
anymatch str []         = True
anymatch []  pat        = False
anymatch (x:xs) (y:ys)  | x == y    = globmatch xs ys
                        | otherwise = anymatch xs (y:ys)

-- Check recursive suffix function
-- Again it boils down to the same function as above but differently
-- Matching Kleene star is a pain
matchclass :: String -> String -> Bool
matchclass (x:xs) pat =
  -- lets try implementing range matching now
  -- let (c,d) = ormatch (x:xs) pat
  -- in
  --   c && simplematch xs d
  -- where
  --   ormatch (x:xs) ('[':ys) = ormatch (x:xs) ys
  --   ormatch (x:xs) (']':ys) = (False,ys)
  --   ormatch (x:xs) (y  :ys) =
  --     let (a,b) = ormatch (x:xs) ys
  --     in
  --     (x == y || a, b)

  --   ormatch (x:xs) []       = error "Invalid braces .. no opening [ found"
  --   -- Below two cases won't likely to occur
  --   ormatch [] []           = (False,[])
  --   ormatch [] xs           = (False,[])

  --  Another implementation of the above matchclass function
  -- Since only one character is supposed to be match or not match, we can simplify it to
  let (c,d) = (classmatch (x:xs) pat) x
  in
    c && globmatch xs d

-- --            Internal parameters
rangematch :: Int -> Int -> String -> Char -> (Bool,String)
rangematch start end remstr ch = ( (\x -> x >= start && x <= end) $ fromEnum ch, remstr)

notmatch   :: String -> String -> Char -> (Bool,String)
notmatch   noStr remStr ch = (not $ elem ch noStr,remStr)

ormatch    :: String -> String -> Char -> (Bool,String)
ormatch    matStr remStr ch = (elem ch matStr, remStr)

-- lets first determine the type of character class ...
-- i.e check whether it is range, or or not
classmatch :: String -> String -> (Char -> (Bool,String))
classmatch (x:xs) ('[':ys) =
  determine ys
  where
    determine (x:'-':y:']':ys) = rangematch (fromEnum x) (fromEnum y) ys
    determine (x:xs)           = let (buf,rem) = findStringUntil ']' xs
                                 in
                                   -- Say no to error checking
                                   -- Trust the user
                                   if x == '!' then
                                     notmatch buf rem
                                     else
                                     ormatch (x:buf) rem

classmatch _      _        = error "Not a character class"


-- Helper function
findStringUntil :: Char -> String -> (String,String)
findStringUntil _  [] = ([],[])
findStringUntil x  (y:ys) | x == y    = ([],ys) --In case only one character matched, return the same character as string
                          | otherwise =
                            let (a,b) = findStringUntil x ys
                            in
                                (y:a,b)
