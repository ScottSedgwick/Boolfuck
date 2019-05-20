module MiniStringFuck where

import Data.Char (chr)

data State = State 
  { cell :: Int
  , output :: String
  }

initState :: State
initState = State { cell = 0, output = [] }

myFirstInterpreter :: String -> String
myFirstInterpreter = process initState

process :: State -> String -> String
process s [] = reverse (output s)
process s (x:xs) = process (step s x) xs

step :: State -> Char -> State
step s '+' = s { cell = inc 256 (cell s) }
step s '.' = s { output = chr (cell s) : (output s) }
step s _   = s

inc :: Int -> Int -> Int
inc m x = x''
  where
    x' = x + 1
    x'' = if x' >= m then 0 else x'