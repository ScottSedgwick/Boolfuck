module Brainfuck where

import Data.Char (chr, isControl)
import Data.List.Zipper (Zipper(..), cursor, endp, fromList, left, replace, right)

-- Top level interpreter function
brainfuck :: String -> String -> String
brainfuck cs = process . buildState cs
  where
    process s | endp (commands s) = parseOutput (output s)
              | otherwise         = process (step s)
    parseOutput = map chr . reverse

-- Interpreter state
data State = State
  { commands :: Zipper Char
  , cells :: Zipper Int
  , input :: [Int]
  , output :: [Int]
  } deriving (Show)

-- State builder
buildState :: String -> String -> State
buildState cs xs = State 
  { commands = fromList cs
  , cells = Zip (repeat 0) (repeat 0)
  , input = map fromEnum xs
  , output = []
  }

-- Execute a single instruction
step :: State -> State
step s = 
  case cursor (commands s) of
    '>' -> s' { cells = right (cells s') }    -- Move the pointer to the right
    '<' -> s' { cells = left (cells s') }     -- Move the pointer to the left
    '+' -> modifyCell s' 1                    -- Increment the memory cell under the pointer
    '-' -> modifyCell s' (-1)                 -- Decrement the memory cell under the pointer
    '.' -> outputCell s'                      -- Output the character signified by the cell at the pointer
    ',' -> inputCell s'                       -- Input a character and store it in the cell at the pointer
    '[' -> jumpFwd s                          -- Jump past the matching ] if the cell under the pointer is 0
    ']' -> jumpBack s                         -- Jump back to the matching [ if the cell under the pointer is nonzero
  where
    s' = s { commands = right (commands s) }

modifyCell :: State -> Int -> State
modifyCell s i = s { cells = replace (cursor cs + i) cs }
  where
    cs = cells s

outputCell :: State -> State
outputCell s = s { output = if valid x then x : xs else xs }
  where
    x = cursor (cells s)
    xs = output s

inputCell :: State -> State
inputCell s = s { cells = replace x cs, input = xs }
  where
    cs = cells s
    (x:xs) = input s

-- Jump past the matching ] if the cell under the pointer is 0
jumpFwd :: State -> State
jumpFwd s = if cursor (cells s) == 0
            then skipFwd 0 sR
            else sR
  where
    sR = s { commands = right (commands s) }

skipFwd :: Int -> State -> State
skipFwd n s = case cursor (commands s) of
                ']' ->  if n <= 0
                        then sR
                        else skipFwd (n - 1) sR
                '[' ->  skipFwd (n + 1) sR
                _   ->  skipFwd n sR
  where
    sR = s { commands = right (commands s) }

-- Jump back to the matching [ if the cell under the pointer is nonzero 
jumpBack :: State -> State
jumpBack s = if cursor (cells s) == 0
              then sR
              else skipBack 0 sL
  where
    sL = s { commands = left (commands s) }
    sR = s { commands = right (commands s) }


skipBack :: Int -> State -> State
skipBack n s = case cursor (commands s) of
                '[' ->  if n <= 0
                        then sR
                        else skipBack (n - 1) sL
                ']' ->  skipBack (n + 1) sL
                _   ->  skipBack n sL
  where
    sL = s { commands = left (commands s) }
    sR = s { commands = right (commands s) }

valid :: Int -> Bool
valid x = (x > 0) -- && not (isControl (chr x))