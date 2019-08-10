module Types (CodeLine(..), Dynamic(..), Context) where

import qualified Data.Map as M


data CodeLine = CodeLine 
                    {
                        tag::String,
                        args::[String],
                        dLines::[CodeLine]
                    } deriving (Show, Eq)


data Dynamic = DInt Integer | DStr String | DBool Bool | DFloat Double deriving Eq
type Context = M.Map String Dynamic

instance Show Dynamic where
    show (DInt x) = show x
    show (DStr  x) = (tail . init) $ show x
    show (DBool x) = show x
    show (DFloat x) = show x