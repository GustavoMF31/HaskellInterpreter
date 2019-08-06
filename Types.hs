module Types (CodeLine(..), Dynamic(..), Context) where

import qualified Data.Map as M


data CodeLine = CodeLine 
                    {
                        tag::String,
                        args::[String],
                        dLines::[CodeLine]
                    } deriving (Show, Eq)


data Dynamic = DInt Integer | DStr String | DBool Bool | DFloat Double deriving (Show)
type Context = M.Map String Dynamic