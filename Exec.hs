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
varTag context arguments []
    | length arguments == 2 = return $ M.insert variableName variableContent context
    | otherwise = error "Wrong quantity of arguments for var"
    where (DStr variableName) = arguments !! 0
          variableContent = arguments !! 1
    

varTag _ _ _ = error "Var tag should have no dependent lines"


ifTag :: Context -> [Dynamic] -> [CodeLine] -> IO (Context)
ifTag context arguments dLines
    | length arguments == 1 = if condition
                then contextExecute dLines (return context)
                else return context
    | otherwise = error "Wrong quantity of arguments for if"
        where (DBool condition) = arguments !! 0


inputTag context arguments dLines
    | length arguments == 2 = do 
            putStr prompt
            varValue <- getLine

            return $ M.insert varName (DStr varValue) context

    | otherwise =  error "Wrong quantity of arguments for input"
    where (DStr varName)  = arguments !! 0
          (DStr prompt) = arguments !! 1


toType readType context arguments dlines
    | length arguments == 1 = return $ M.insert varName varAsDynamic context
    | otherwise = error "Wrong quantity of arguments for toInt"
    where (DStr varName) = arguments !! 0
          varAsString = show $ context M.! varName
          varAsDynamic = readType varAsString


readDInt   = DInt . read
readDStr   = DStr . read
readDBool  = DBool . read
readDFloat = DFloat . read


tagFunc:: String -> (Context -> [Dynamic] -> [CodeLine] -> IO (Context))
tagFunc "var" = varTag
tagFunc "if"  = ifTag
tagFunc "input" = inputTag

tagFunc "->int" = toType readDInt
tagFunc "->str" = toType readDStr
tagFunc "->bool" = toType readDBool
tagFunc "->float" = toType readDFloat

tagFunc tag = error $ "Unknown tag '" ++ tag ++ "'"