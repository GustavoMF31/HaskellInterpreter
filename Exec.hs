module Exec (execute, contextExecute) where

import qualified Data.Map as M
import Types
import Eval (evaluateIfNeeded)


execute :: [CodeLine] -> Context
execute codeLines = contextExecute codeLines M.empty


contextExecute :: [CodeLine] -> Context -> Context
contextExecute codeLines context = foldl executer context codeLines


executer :: Context -> CodeLine -> Context
executer context codeLine = (tagFunc (tag codeLine)) context arguments (dLines codeLine)
    where arguments = map (flip evaluateIfNeeded context) (args codeLine)


varTag :: Context -> [Dynamic] -> [CodeLine] -> Context
varTag context arguments [] = if length arguments == 2 
                           then let ((DStr variableName):variableContent) = arguments in
                           M.insert variableName (variableContent !! 0) context
                           else error "Wrong quantity of arguments for var"
varTag _ _ _ = error "Var tag should have no dependent lines"


ifTag context arguments dLines = if length arguments == 1
                                 then let (DBool x) = arguments !! 0 in
                                    if x then contextExecute dLines context
                                    else context
                                 else error "Wrong quantity of arguments for if"


tagFunc:: String -> (Context -> [Dynamic] -> [CodeLine] -> Context)
tagFunc "var" = varTag
tagFunc "if"  = ifTag
tagFunc tag = error $ "Unknown tag '" ++ tag ++ "'"