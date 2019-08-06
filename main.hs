import Exec (execute)
import Parser (parse)

run = execute . parse


main = do
    content <- readFile "htests.txt"
    print $ run content