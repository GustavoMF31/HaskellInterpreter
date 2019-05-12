module Types (CodeLine(..), Variable(..), Context) where

import qualified Data.Map as M


data CodeLine = CodeLine 
                    {
                        tag::String,
                        args::[String],
                        dLines::[CodeLine]
                    } deriving (Show, Eq)


data Variable = IntVar Integer | StrVar String | BoolVar Bool | FloatVar Double deriving (Show)
type Context = M.Map String Variable