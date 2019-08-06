import Exec (execute)
import Parser (parse)

main = do
    content <- readFile "htests.txt"
    print $ execute (parse content)