import Exec (execute)
import Parser (parse)

run code = do
    execute $ parse code
    return ()


main = do
    content <- readFile "htests.txt"
    run content