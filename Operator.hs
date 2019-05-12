module Operator (operatorFunc) where

import Types
import Data.List (genericReplicate)

dAdd :: Variable -> Variable -> Variable
dAdd (IntVar x) (IntVar y) = IntVar (x + y)
dAdd (FloatVar x) (FloatVar y) = FloatVar (x + y)
dAdd (StrVar x) (StrVar y) = StrVar (x ++ y)
dAdd _ _ = error "Wrong types to add"


dSub (IntVar x) (IntVar y) = IntVar (x - y)
dSub (FloatVar x) (FloatVar y) = FloatVar (x - y)
dSub _ _ = error "Wrong types to subtract"


dMul (IntVar x) (IntVar y) = IntVar (x * y)
dMul (FloatVar x) (FloatVar y) = FloatVar (x * y)
dMul (IntVar x) (StrVar y) = StrVar (concat $ genericReplicate x y)
dMul (StrVar y) (IntVar x) = StrVar (concat $ genericReplicate x y)
dMul _ _ = error "Wrong types to multiply"


dDiv (IntVar x) (IntVar y) = FloatVar (fromIntegral x / fromIntegral y)
dDiv (FloatVar x) (FloatVar y) = FloatVar (x / y)
dDiv _ _ = error "Wrong types to divide"


bt (IntVar x) (IntVar y) = BoolVar (x > y)
bt (FloatVar x) (FloatVar y) = BoolVar (x > y)
bt _ _ = error "Wrong types to bigger than"


lt (IntVar x) (IntVar y) = BoolVar (x < y)
lt (FloatVar x) (FloatVar y) = BoolVar (x < y)
lt _ _ = error "Wrong types to less than"


bte (IntVar x) (IntVar y) = BoolVar (x >= y)
bte (FloatVar x) (FloatVar y) = BoolVar (x >= y)
bte _ _ = error "Wrong types to bigger than"


lte (IntVar x) (IntVar y) = BoolVar (x <= y)
lte (FloatVar x) (FloatVar y) = BoolVar (x <= y)
lte _ _ = error "Wrong types to less than"


equals (BoolVar x) (BoolVar y) = BoolVar (x == y)
equals (IntVar x)  (IntVar y)  = BoolVar (x == y)
equals (StrVar x)  (StrVar y)  = BoolVar (x == y)
equals (FloatVar x) (FloatVar y) = BoolVar (x == y)
equals _ _ = error "Wrong types to equals"


notEquals (BoolVar x) (BoolVar y) = BoolVar (x /= y)
notEquals (IntVar x)  (IntVar y)  = BoolVar (x /= y)
notEquals (StrVar x)  (StrVar y)  = BoolVar (x /= y)
notEquals (FloatVar x) (FloatVar y) = BoolVar (x /= y)
notEquals _ _ = error "Wrong types to notEquals"


operatorFunc :: String -> (Variable -> Variable -> Variable)
operatorFunc "+" = dAdd
operatorFunc "-" = dSub
operatorFunc "*" = dMul
operatorFunc "/" = dDiv
operatorFunc ">" = bt
operatorFunc "<" = lt
operatorFunc ">=" = bte
operatorFunc "<=" = lte
operatorFunc "=" = equals
operatorFunc "!=" = notEquals
operatorFunc op = error $ "Bad operator " ++ (show op)