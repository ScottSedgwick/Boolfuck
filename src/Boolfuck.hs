module Boolfuck where

import Data.Char (chr, ord)

data Bit = Zero | One deriving (Show, Eq)

data State = State
  { commands :: Zipper Char
  , cells :: Zipper Bit
  , input :: [Bit]
  , output :: [Bit]
  } deriving (Show)

buildState :: String -> String -> State
buildState cs xs = State 
  { commands = fromList cs
  , cells = Zip (repeat Zero) (repeat Zero)
  , input = strToBits xs
  , output = []
  }

boolfuck :: String -> String -> String
boolfuck cs = process . buildState cs

process :: State -> String
process s | endp (commands s) = processOutput (output s)
          | otherwise         = process (step s)

processOutput :: [Bit] -> String
processOutput = bitsToStr . reverse . pad8

pad8 :: [Bit] -> [Bit]
pad8 xs = if length  xs `mod` 8 == 0 
          then xs
          else pad8 (Zero : xs)

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
flipcell s = s { cells = f (cells s) }
  where
    f x = replace (flipBit (cursor x)) x

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One  = Zero

-- Reads a bit from the input stream, storing it under the pointer
readBit :: State -> State
readBit s = s { cells = cs, input = xs }
  where
    (x:xs) = if (input s) == [] then (Zero:[]) else input s
    cs = replace x (cells s)

-- Outputs the bit under the pointer to the output stream
writeBit :: State -> State
writeBit s = s { output = cursor (cells s) : output s }

-- [ - If the value under the pointer is 0 then skip to the corresponding ], else move to next command
jumpFwd :: State -> State
jumpFwd s = if cursor (cells s) == Zero 
            then skipFwd 0 sR
            else sR
  where
    sR = s { commands = right (commands s) }

skipFwd :: Int -> State -> State
skipFwd n s = case cursor (commands s) of
                ']' -> if n <= 0
                        then sR
                        else skipFwd (n - 1) sR
                '[' -> skipFwd (n + 1) sR
                _   -> skipFwd n sR
  where
    sR = s { commands = right (commands s) }

-- ] - if the value under the pointer is 1 then skip back to the matching [, else move to the next command 
jumpBack :: State -> State
jumpBack s = if cursor (cells s) == One
              then skipBack 0 sL
              else sR
  where
    sL = s { commands = left (commands s) }
    sR = s { commands = right (commands s) }


skipBack :: Int -> State -> State
skipBack n s = case cursor (commands s) of
                '[' -> if n <= 0
                        then sR
                        else skipBack (n - 1) sL
                ']' -> skipBack (n + 1) sL
                _   -> skipBack n sL
  where
    sL = s { commands = left (commands s) }
    sR = s { commands = right (commands s) }

bitsToStr :: [Bit] -> String
bitsToStr = map (chr . bitsToByte) . splitN 8

bitsToByte :: [Bit] -> Int
bitsToByte [a,b,c,d,e,f,g,h] = bitToInt a 
                              + bitToInt b * 2 
                              + bitToInt c * 4 
                              + bitToInt d * 8 
                              + bitToInt e * 16 
                              + bitToInt f * 32 
                              + bitToInt g * 64 
                              + bitToInt h * 128
bitsToByte _ = 0

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One  = 1

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n xs = take n xs : splitN n (drop n xs)

strToBits :: String -> [Bit]
strToBits = concatMap (byteToBits . ord)

byteToBits :: Int -> [Bit]
byteToBits i = f 8 i []
  where
    f 0 _ xs = reverse xs
    f n x xs = f (n - 1) (x `div` 2) ((if (x `mod` 2 == 1) then One else Zero) : xs)

-- Everything below here is only included because the CodeWars environment does
-- not allow the import of Data.List.Zipper.
data Zipper a = Zip ![a] ![a] deriving (Eq,Show)

fromList :: [a] -> Zipper a
fromList as = Zip [] as

endp :: Zipper a -> Bool
endp   (Zip _  []) = True
endp   _           = False

cursor :: Zipper a -> a
cursor (Zip _ (a:_)) = a

left :: Zipper a -> Zipper a
left  (Zip (a:ls) rs) = Zip ls (a:rs)
left  z               = z

right :: Zipper a -> Zipper a
right (Zip ls (a:rs)) = Zip (a:ls) rs
right z               = z

replace :: a -> Zipper a -> Zipper a
replace a (Zip ls (_:rs)) = Zip ls (a:rs)
replace _ z               = z

empty :: Zipper a
empty = Zip [] []