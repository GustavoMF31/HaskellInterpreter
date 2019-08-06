module Funcs (exclusiveBreak, trackParenthesis, removeParenthesis,
              isInParenthesis, carefulSplit, isComment, isNumber, split,
              removeStartingTab, getCodeLine) where


import qualified Data.Map as M
import Data.Char (isDigit)
import Types

isInParenthesis xs = head xs == '(' && last xs == ')'
isNumber xs = all isDigit xs
removeParenthesis xs = init (tail xs)


getCodeLine :: String -> CodeLine
getCodeLine rawline = CodeLine tag (carefulSplit ' ' args) []
    where (tag, args) = exclusiveBreak (==' ') rawline


exclusiveBreak :: (a -> Bool) -> [a] -> ([a], [a])
exclusiveBreak f xs = (fst parts, tail (snd parts))
    where parts = break f xs


trackParenthesis :: Int -> Char -> Int
trackParenthesis x ')' = x - 1
trackParenthesis x '(' = x + 1
trackParenthesis x _   = x


cSplitter :: [String] -> (Char, Bool) -> [String]
cSplitter xs charT
    | snd charT = [] : xs
    | otherwise = ((head xs) ++ [fst charT]) : (tail xs)


carefulSplit :: Char -> String -> [String]
carefulSplit char xs = reverse $ foldl cSplitter [[]] charsToSplit
    where parenthesisMap = scanl trackParenthesis 0 xs
          whereToSplit   = zipWith (\a b -> a == char && b == 0) xs parenthesisMap
          charsToSplit   = zip xs whereToSplit

--https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y)
    where (x,y) = span (/= d) s


isComment :: String -> Bool
isComment "" = True
isComment (x:y:ys) = x == '/' && y == '/'


removeStartingTab :: CodeLine -> CodeLine
removeStartingTab line = CodeLine (tail $ tag line) (args line) (dLines line)