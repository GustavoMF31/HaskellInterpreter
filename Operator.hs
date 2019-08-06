module Operator (operatorFunc) where

import Types
import Data.List (genericReplicate)


dAdd (DInt x) (DInt y) = DInt (x + y)
dAdd (DFloat x) (DFloat y) = DFloat (x + y)
dAdd (DStr x) (DStr y) = DStr (x ++ y)
dAdd _ _ = error "Wrong types to add"


dSub (DInt x) (DInt y) = DInt (x - y)
dSub (DFloat x) (DFloat y) = DFloat (x - y)
dSub _ _ = error "Wrong types to subtract"


dMul (DInt x) (DInt y) = DInt (x * y)
dMul (DFloat x) (DFloat y) = DFloat (x * y)
dMul (DInt x) (DStr y) = DStr (concat $ genericReplicate x y)
dMul (DStr y) (DInt x) = DStr (concat $ genericReplicate x y)
dMul _ _ = error "Wrong types to multiply"


dDiv (DInt x) (DInt y) = DFloat (fromIntegral x / fromIntegral y)
dDiv (DFloat x) (DFloat y) = DFloat (x / y)
dDiv _ _ = error "Wrong types to divide"


bt (DInt x) (DInt y) = DBool (x > y)
bt (DFloat x) (DFloat y) = DBool (x > y)
bt _ _ = error "Wrong types to bigger than"


lt (DInt x) (DInt y) = DBool (x < y)
lt (DFloat x) (DFloat y) = DBool (x < y)
lt _ _ = error "Wrong types to less than"


bte (DInt x) (DInt y) = DBool (x >= y)
bte (DFloat x) (DFloat y) = DBool (x >= y)
bte _ _ = error "Wrong types to bigger than"


lte (DInt x) (DInt y) = DBool (x <= y)
lte (DFloat x) (DFloat y) = DBool (x <= y)
lte _ _ = error "Wrong types to less than"


equals (DBool x) (DBool y) = DBool (x == y)
equals (DInt x)  (DInt y)  = DBool (x == y)
equals (DStr x)  (DStr y)  = DBool (x == y)
equals (DFloat x) (DFloat y) = DBool (x == y)
equals _ _ = error "Wrong types to equals"


notEquals (DBool x) (DBool y) = DBool (x /= y)
notEquals (DInt x)  (DInt y)  = DBool (x /= y)
notEquals (DStr x)  (DStr y)  = DBool (x /= y)
notEquals (DFloat x) (DFloat y) = DBool (x /= y)
notEquals _ _ = error "Wrong types to notEquals"


operatorFunc :: String -> (Dynamic -> Dynamic -> Dynamic)
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