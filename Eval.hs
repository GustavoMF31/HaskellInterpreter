module Eval (evaluate, evaluateIfNeeded) where

import qualified Data.Map as M
import Types
import Funcs (isNumber, split, carefulSplit, isInParenthesis, removeParenthesis)
import Operator (operatorFunc)

operators = ["+", "-", "*", "/", ">", "<", ">=", "<=", "=", "!="]

evaluate :: String -> Context -> Dynamic
evaluate expression context
    -- If it's an integer
    | isNumber expression = DInt (read expression)
    -- If it's a float
    | ((length parts) == 2) && (isNumber p1) && (isNumber p2) = DFloat (read expression) 
    -- If it's the name of a variable
    | M.member expression context = context M.! expression
    -- If it' a bool, return it
    | expression == "+" = DBool True
    | expression == "-" = DBool False
    -- If it doesn't look like an expression, it must be a long string!
    | ((length chunks) /= 3) || (operator `notElem` operators) = DStr expression
    -- Otherwise it's an expression
    | otherwise = func arg1 arg2
    where parts = split '.' expression
          p1 = parts !! 0
          p2 = parts !! 1
          chunks   = carefulSplit ' ' expression
          rawArg1  = chunks !! 0
          operator = chunks !! 1
          rawArg2  = chunks !! 2

          func = operatorFunc operator
          arg1 = evaluateIfNeeded rawArg1 context
          arg2 = evaluateIfNeeded rawArg2 context


evaluateIfNeeded :: String -> Context -> Dynamic
evaluateIfNeeded expression context
    | isInParenthesis expression = evaluate (removeParenthesis expression) context
    | otherwise = DStr expression