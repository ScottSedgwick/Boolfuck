module Paintfuck where

import Data.List (intercalate, replicate)
-- import Data.List.Zipper (Zipper, start, beginp, cursor, end, endp, fromList, left, replace, right, toList)
-- import Data.Matrix (Matrix, (!), getRow, matrix, setElem)
-- import Data.Vector (Vector, toList)

data State = State 
  { sCode  :: Zipper Char
  , sData  :: Matrix Bool
  , sX     :: Int
  , sY     :: Int
  , sRows  :: Int
  , sCols  :: Int
  , sCount :: Int
  } deriving (Show)

buildState :: String -> Int -> Int -> Int -> State
buildState code iters width height = State
  { sCode  = fromList code
  , sData  = matrix height width (\_ -> False)
  , sX     = 1
  , sY     = 1
  , sRows  = height
  , sCols  = width
  , sCount = iters
  }

interpreter :: String -> Int -> Int -> Int -> String
interpreter code iters width height = printOutput (process (buildState code iters width height))

printOutput :: State -> String
printOutput s = intercalate "\r\n" ts 
  where
    ts = map toRow (mdata (sData s))
    toRow = map toC
    toC True  = '1'
    toC False = '0' 

process :: State -> State
process s = if (sCount s) <= 0 || endp (sCode s)
            then s
            else process (step s)

step :: State -> State
step s =
  case cursor (sCode s) of
    'n' ->  if (sX s) <= 1
            then s { sCount = (sCount s) - 1, sCode = right (sCode s), sX = (sRows s)  }
            else s { sCount = (sCount s) - 1, sCode = right (sCode s), sX = (sX s) - 1 }
    's' ->  if (sX s) >= (sRows s)
            then s { sCount = (sCount s) - 1, sCode = right (sCode s), sX = 1          }
            else s { sCount = (sCount s) - 1, sCode = right (sCode s), sX = (sX s) + 1 }
    'w' ->  if (sY s) <= 1
            then s { sCount = (sCount s) - 1, sCode = right (sCode s), sY = (sCols s)  }
            else s { sCount = (sCount s) - 1, sCode = right (sCode s), sY = (sY s) - 1 }
    'e' ->  if (sY s) >= (sCols s)
            then s { sCount = (sCount s) - 1, sCode = right (sCode s), sY = 1          }
            else s { sCount = (sCount s) - 1, sCode = right (sCode s), sY = (sY s) + 1 }
    '*' ->  s      { sCount = (sCount s) - 1, sCode = right (sCode s), sData = change (sX s, sY s) not (sData s) }
    '[' ->  jmpF s { sCount = (sCount s) - 1 }
    ']' ->  jmpB s { sCount = (sCount s) - 1 }
    _   ->  s      { sCode = right (sCode s) }

change :: (Int, Int) -> (a -> a) -> Matrix a -> Matrix a
change p f m = setElem (f (m ! p)) p m

value :: State -> Bool
value s = (sData s) ! (sX s, sY s)

-- Jump past matching ']' if value at current cell is 0
jmpF :: State -> State
jmpF s =  if value s
          then sR
          else skipFwd 0 sR
  where
    sR = s { sCode = right (sCode s) }

skipFwd :: Int -> State -> State
skipFwd n s = case cursor (sCode s) of
                ']' ->  if n <= 0
                        then sR
                        else skipFwd (n - 1) sR
                '[' ->  skipFwd (n + 1) sR
                _   ->  skipFwd n sR
  where
    sR = s { sCode = right (sCode s) }

-- Jump back to matching '[' if value at current cell is 1
jmpB :: State -> State
jmpB s =  if value s
          then skipBack 0 sL
          else sR
  where
    sL = s { sCode = left (sCode s) }
    sR = s { sCode = right (sCode s) }


skipBack :: Int -> State -> State
skipBack n s = case cursor (sCode s) of
                '[' ->  if n <= 0
                        then sR
                        else skipBack (n - 1) sL
                ']' ->  skipBack (n + 1) sL
                _   ->  skipBack n sL
  where
    sL = s { sCode = left (sCode s) }
    sR = s { sCode = right (sCode s) }

-- Data.List.Zipper (Zipper, beginp, cursor, end, endp, fromList, left, replace, right, start, toList)
data Zipper a = Zip ![a] ![a] deriving (Eq,Show)

beginp :: Zipper a -> Bool
beginp (Zip [] _ ) = True
beginp _           = False

endp :: Zipper a -> Bool
endp   (Zip _  []) = True
endp   _           = False

fromList :: [a] -> Zipper a
fromList as = Zip [] as

start, end :: Zipper a -> Zipper a
start (Zip ls rs) = Zip [] (reverse ls ++ rs)
end   (Zip ls rs) = Zip (reverse rs ++ ls) []

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

toList :: Zipper a -> [a]
toList (Zip ls rs) = reverse ls ++ rs

-- Data.Matrix (Matrix, (!), getRow, matrix, setElem)
data Matrix a = Matrix 
  { mdata :: [[a]]
  } deriving Show

(!) :: Matrix a -> (Int, Int) -> a
(!) m (r,c) = row !! (c - 1)
  where
    row = getRow r m

fromLists :: [[a]] -> Matrix a
fromLists xs = Matrix { mdata = xs }

getRow :: Int -> Matrix a -> [a]
getRow r m = (mdata m) !! (r - 1)

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix r c f = Matrix { mdata = replicate r row }
  where 
    row = replicate c (f (r,c))

setElem :: a -> (Int, Int) -> Matrix a -> Matrix a
setElem v (r,c) m = fromLists $ listreplace row' (r - 1) (mdata m)
  where
    row  = getRow r m
    row' = listreplace v (c - 1) row

listreplace :: a -> Int -> [a] -> [a]
listreplace v n xs = take n xs ++ (v : drop (n + 1) xs)