module Boolfuck where

import Data.Char        (chr, ord)
import Data.List.Split  (chunksOf)
import Data.List.Zipper (Zipper(..), cursor, endp, fromList, left, replace, right)

-- Top level interpreter function
boolfuck :: String -> String -> String
boolfuck cs = process . buildState cs
  where
    process s | endp (commands s) = parseOutput (output s)
              | otherwise         = process (step s)
    parseOutput = boolsToStr . reverse . pad8
    pad8 xs = if length  xs `mod` 8 == 0 
              then xs
              else pad8 (False : xs)

-- Interpreter state
data State = State
  { commands :: Zipper Char
  , cells :: Zipper Bool
  , input :: [Bool]
  , output :: [Bool]
  } deriving (Show)

-- State builder
buildState :: String -> String -> State
buildState cs xs = State 
  { commands = fromList cs
  , cells = Zip (repeat False) (repeat False)
  , input = strToBools xs
  , output = []
  }

-- Execute a single instruction
step :: State -> State
step s = 
  case cursor (commands s) of
    '+' -> flipcell s'                        -- Flips the value of the bit under the pointer
    ',' -> readBit s'                         -- Reads a bit from the input stream, storing it under the pointer
    ';' -> writeBit s'                        -- Outputs the bit under the pointer to the output stream
    '<' -> s' { cells = left (cells s) }      -- Moves the pointer left by 1 bit
    '>' -> s' { cells = right (cells s) }     -- Moves the pointer right by 1 bit
    '[' -> jumpFwd s                          -- If the value under the pointer is 0 then skip to the corresponding ]
    ']' -> jumpBack s                         -- Jumps back to the matching [ character, if the value under the pointer is 1
    _   -> s'
  where
    s'  = s { commands = right (commands s) }

-- Flips the value of the bit under the pointer
flipcell :: State -> State
flipcell s = s { cells = replace (not (cursor (cells s))) (cells s) }

-- Reads a bit from the input stream, storing it under the pointer
readBit :: State -> State
readBit s = s { cells = replace x (cells s), input = xs }
  where
    (x:xs) = if null (input s) then [False] else input s

-- Outputs the bit under the pointer to the output stream
writeBit :: State -> State
writeBit s = s { output = cursor (cells s) : output s }

-- [ - If the value under the pointer is 0 then skip to the corresponding ], else move to next command
jumpFwd :: State -> State
jumpFwd s = if cursor (cells s)
            then sR
            else skipFwd 0 sR
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

-- ] - if the value under the pointer is 1 then skip back to the matching [, else move to the next command 
jumpBack :: State -> State
jumpBack s = if cursor (cells s)
              then skipBack 0 sL
              else sR
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

-- Convert a list of Bool to a String
boolsToStr :: [Bool] -> String
boolsToStr = map f . chunksOf 8
  where
    f     = chr . sum . zipWith g [0..7] 
    g i b = if b then 2 ^ i else 0

-- Convert a String to a List of Bool
strToBools :: String -> [Bool]
strToBools = concatMap (f . ord)
  where
    f i = g 8 i []
    g 0 _ xs = reverse xs
    g n x xs = g (n - 1) (x `div` 2) ((x `mod` 2 == 1) : xs)
