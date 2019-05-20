module Smallfuck where

-- import Data.List.Zipper (Zipper(..), beginp, cursor, endp, left, replace, right, toList)

data State = State
  { sTape :: Zipper Bool
  , sCode :: Zipper Char
  , sTerminated :: Bool
  }

interpreter :: String -> String -> String
interpreter code = map showBool . toList . sTape . process . buildState code

showBool :: Bool -> Char
showBool True  = '1'
showBool False = '0'

buildState :: String -> String -> State
buildState code tape = State
  { sCode = Zip [] code
  , sTape = Zip [] (map (== '1') tape)
  , sTerminated = code == []
  }

process :: State -> State
process s = if terminated
            then s
            else process (step s)
  where
    terminated = sTerminated s || endp (sCode s)

step :: State -> State
step s = 
  case cursor (sCode s) of
    '>' ->  if endp (right (sTape s))
            then s { sTerminated = True }
            else s { sTape = right (sTape s), sCode = right (sCode s) }
    '<' ->  if beginp (sTape s)
            then s { sTerminated = True }
            else s { sTape = left (sTape s), sCode = right (sCode s) }
    '*' ->  s { sTape = replace (not (cursor (sTape s))) (sTape s), sCode = right (sCode s) }
    '[' ->  jumpFwd s
    ']' ->  jumpBack s
    _   ->  s { sCode = right (sCode s) }

-- Jump past matching ']' if value at current cell is 0
jumpFwd :: State -> State
jumpFwd s = if cursor (sTape s)
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
jumpBack :: State -> State
jumpBack s = if cursor (sTape s)
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

-- Zipper implementation
data Zipper a = Zip ![a] ![a] deriving (Eq,Show)

beginp :: Zipper a -> Bool
beginp (Zip [] _ ) = True
beginp _           = False

cursor :: Zipper a -> a
cursor (Zip _ (a:_)) = a

endp :: Zipper a -> Bool
endp   (Zip _  []) = True
endp   _           = False

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