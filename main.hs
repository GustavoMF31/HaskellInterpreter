import qualified Data.Map as M
import Exec (execute)
import Parser (parse)

main = do
    content <- readFile "htests.txt"
    print $ execute (parse content)