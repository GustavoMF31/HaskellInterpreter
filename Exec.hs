module Exec (execute, contextExecute) where

import qualified Data.Map as M
import Types
import Eval (evaluateIfNeeded)

execute :: [CodeLine] -> IO (Context)
execute codeLines = contextExecute codeLines (return M.empty)


contextExecute :: [CodeLine] -> IO (Context) -> IO (Context)
contextExecute codeLines context = foldl executer context codeLines


executer :: IO (Context) -> CodeLine -> IO (Context)
executer ioContext codeLine = do
    context <- ioContext

    let tagFunction = tagFunc (tag codeLine)
    let arguments = map (flip evaluateIfNeeded context) (args codeLine)

    tagFunction context arguments (dLines codeLine)


varTag :: Context -> [Dynamic] -> [CodeLine] -> IO (Context)
varTag context arguments [] =
    if length arguments == 2
        then let ((DStr variableName):variableContent) = arguments in
            return $ M.insert variableName (variableContent !! 0) context
    else error "Wrong quantity of arguments for var"

varTag _ _ _ = error "Var tag should have no dependent lines"


ifTag :: Context -> [Dynamic] -> [CodeLine] -> IO (Context)
ifTag context arguments dLines =
    if length arguments == 1
        then let (DBool x) = arguments !! 0 in
            if x
                then contextExecute dLines (return context)
                else return context
    else error "Wrong quantity of arguments for if"



inputTag context arguments dLines =
    if length arguments == 2
        then let ((DStr varName) : ((DStr prompt):_)) = arguments in do
            putStr prompt
            varValue <- getLine

            return $ M.insert varName (DStr varValue) context

    else error "Wrong quantity of arguments for input"


tagFunc:: String -> (Context -> [Dynamic] -> [CodeLine] -> IO (Context))
tagFunc "var" = varTag
tagFunc "if"  = ifTag
tagFunc "input" = inputTag
tagFunc tag = error $ "Unknown tag '" ++ tag ++ "'"