module Parser (parse) where

import Types
import Funcs (removeStartingTab, getCodeLine, isComment)

parse :: String -> [CodeLine]
parse xs = reverse $ foldl parser [] sParsed
    where sParsed = simpleParse xs


parser :: [CodeLine] -> CodeLine -> [CodeLine]
parser pastLines currentLine
    -- If the currentLine starts with a tab, add it to the last line's dependent lines list
    | tag currentLine !! 0 == '\t' = x':xs
    -- Otherwise, just add it to the list of lines
    | otherwise = currentLine : pastLines
    -- x' is the last CodeLine with the current line appended to the dependent lines list
    where x' = CodeLine (tag x) (args x) (dLines x ++ [removeStartingTab currentLine])
          (x:xs) = pastLines


removeComments :: [String] -> [String]
removeComments = filter (not . isComment)

simpleParse = (map getCodeLine) . removeComments . lines