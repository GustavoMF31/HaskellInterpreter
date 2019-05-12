module Parser (parse) where

import Types
import Funcs (removeStartingTab, getCodeLine, isComment)

parse :: String -> [CodeLine]
parse xs = reverse $ foldl parser [] sParsed
    where sParsed = simpleParse xs


parser :: [CodeLine] -> CodeLine -> [CodeLine]
parser pastLines currentLine
    | tag currentLine !! 0 == '\t' = x':xs
    | otherwise = currentLine : pastLines
    where x' = CodeLine (tag x) (args x) (dLines x ++ [removeStartingTab currentLine])
          (x:xs) = pastLines


simpleParse = map getCodeLine . filter (not . isComment) . lines