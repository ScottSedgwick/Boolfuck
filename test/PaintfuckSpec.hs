module PaintfuckSpec where

import Paintfuck

import Test.Hspec

spec :: Spec
spec = do
  describe "Utility Functions" $ do
    it "should print results" $ do
      let b = buildState "" 0 3 3
      let s = b { sData = fromLists [[True,False,False],[False,True,False],[False,False,True]] }
      printOutput s `shouldBe` "100\r\n010\r\n001"
  describe "Your PaintFuck" $ do
    it "should work for some example test cases" $ do
      -- Your interpreter should initialize all cells in the datagrid to 0
      interpreter "*e*e*e*es*es*ws*ws*w*w*w*n*n*n*ssss*s*s*s*" 0 6 9 `shouldBe` "000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000"
      -- Your interpreter should adhere to the number of iterations specified
      interpreter "*e*e*e*es*es*ws*ws*w*w*w*n*n*n*ssss*s*s*s*" 7 6 9 `shouldBe` "111100\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000"
      -- Your interpreter should traverse the 2D datagrid correctly
      interpreter "*e*e*e*es*es*ws*ws*w*w*w*n*n*n*ssss*s*s*s*" 19 6 9 `shouldBe` "111100\r\n000010\r\n000001\r\n000010\r\n000100\r\n000000\r\n000000\r\n000000\r\n000000"
      -- Your interpreter should traverse the 2D datagrid correctly for all of
      -- the 'n', 'e', 's' and 'w' commands 
      interpreter "*e*e*e*es*es*ws*ws*w*w*w*n*n*n*ssss*s*s*s*" 42 6 9 `shouldBe` "111100\r\n100010\r\n100001\r\n100010\r\n111100\r\n100000\r\n100000\r\n100000\r\n100000"
      -- "Your interpreter should terminate normally and return a
      -- representation of the final state of the 2D datagrid when all commands
      -- have been considered from left to right even if the number of
      -- iterations specified have not been fully performed"
      interpreter "*e*e*e*es*es*ws*ws*w*w*w*n*n*n*ssss*s*s*s*" 100 6 9 `shouldBe` "111100\r\n100010\r\n100001\r\n100010\r\n111100\r\n100000\r\n100000\r\n100000\r\n100000"

    describe "should ignore all non-command characters" $ do
      it "should simply ignore all letters that are not one of \"nesw\" and all punctuation that are not asterisks" $ do
        interpreter "o*e*eq*reqrqp*ppooqqeaqqsr*yqaooqqqfqarppppfffpppppygesfffffffffu*wrs*agwpffffst*w*uqrw*qyaprrrrrw*nuiiiid???ii*n*ynyy??ayd*r:rq????qq::tqaq:y???ss:rqsr?s*qs:q*?qs*tr??qst?q*r" 7 6 9 `shouldBe` "111100\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000"
      it "Your interpreter should ignore all newlines, tabs and spaces" $ do
        interpreter "*e*e*e\n\t\n\t\n\t\n\t*es  *es  *ws*w      s*w*w*w*    n*n*      n*sss               s*s*               s    *s*" 19 6 9 `shouldBe` "111100\r\n000010\r\n000001\r\n000010\r\n000100\r\n000000\r\n000000\r\n000000\r\n000000"
      it "Your interpreter should not recognise any of \",\", \".\", \"<\", \">\", \"+\", \"-\" (valid Brainfuck commands), \";\" (valid Boolfuck command), \"^\" and \"v\" (valid Befunge commands) as valid Paintfuck commands" $ do
        interpreter "*e..*,;e+*e*e-s;<<<<<>>>*,,,e--+s*w+>>>>>>>><;;s*<><<>w.>><>>><<<<><<>><^^^^vvv^v^vv^vv^v^^><>><>.s--*w;;*w>><<>*+^^v^vvv^++w*-+-;;;;+---++-+n;..*n*n++--;;+++-;*ssss.*s*s*s.*" 42 6 9 `shouldBe` "111100\r\n100010\r\n100001\r\n100010\r\n111100\r\n100000\r\n100000\r\n100000\r\n100000"
      it "Your interpreter should not treat uppercase \"NESW\" as valid commands" $ do
        interpreter "*e*eNNESNENSNESNWWEWKSDLFJMCVXNIOWE*e*es*IOWEORUWKVDSVOIRUSKVKLVes*wsIWUENNSLNDKLNSIRIOEDSKKLNSV*ws*w*w*wIOWEURNLSVM,NXVC,MSIWOEU*n*n*n*ssSSEEWWss*s*s*s*EEWWEEWEWSSSNNSWWE" 100 6 9 `shouldBe` "111100\r\n100010\r\n100001\r\n100010\r\n111100\r\n100000\r\n100000\r\n100000\r\n100000"

    describe "should initialize grids of correct size" $ do
      it "should work correctly for a grid of size 1x1" $ do
        interpreter "" 0 1 1 `shouldBe` "0"
      it "should work correctly for other square grids of side length > 1" $ do
        interpreter "" 0 6 6 `shouldBe` "000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000"
      it "should work correctly for other square grids of side length > 1" $ do
        interpreter "" 0 10 10 `shouldBe` "0000000000\r\n0000000000\r\n0000000000\r\n0000000000\r\n0000000000\r\n0000000000\r\n0000000000\r\n0000000000\r\n0000000000\r\n0000000000"
      it "should properly initialize grids of height 1" $ do
        interpreter "" 0 3 1 `shouldBe` "000"
      it "should properly initialize grids of height 1" $ do
        interpreter "" 0 15 1 `shouldBe` "000000000000000"
      it "should properly initialize grids of width 1" $ do
        interpreter "" 0 1 8 `shouldBe` "0\r\n0\r\n0\r\n0\r\n0\r\n0\r\n0\r\n0"
      it "should properly initialize grids of width 1" $ do
        interpreter "" 0 1 11 `shouldBe` "0\r\n0\r\n0\r\n0\r\n0\r\n0\r\n0\r\n0\r\n0\r\n0\r\n0"
      it "should properly initialize grids of any valid dimensions" $ do
        interpreter "" 0 15 20 `shouldBe` "000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000\r\n000000000000000"

    describe "should behave toroidally" $ do
      it "Your data grid should exhibit toroidal (wrapping) behaviour" $ do
        interpreter "eee*s*s*s*w*w*w*w*w*w*w*n*n*n*n*n*n*n*n*n*e*e*e*e*e*e*e*s*s*s*s*s*" 1000 8 10 `shouldBe` "00011000\r\n00011000\r\n00011000\r\n11111111\r\n11111111\r\n00011000\r\n00011000\r\n00011000\r\n00011000\r\n00011000"
        interpreter "eee*s*s*s*w*w*w*w*w*w*w*n*n*n*n*n*n*n*n*n*e*e*e*e*e*e*e*s*s*s*s*s*" 40 8 10 `shouldBe` "00011000\r\n00011000\r\n00011000\r\n11111111\r\n00000000\r\n00001000\r\n00001000\r\n00001000\r\n00001000\r\n00001000"
        interpreter "eee*s*s*s*w*w*w*w*w*w*w*n*n*n*n*n*n*n*n*n*e*e*e*e*e*e*e*s*s*s*s*s*" 66 8 10 `shouldBe` "00011000\r\n00011000\r\n00011000\r\n11111111\r\n11111111\r\n00011000\r\n00011000\r\n00011000\r\n00011000\r\n00011000"
      it "Your data grid should exhibit toroidal (wrapping) behaviour, and not just for one example" $ do
        interpreter "sssss*s*s*s*s*www*w*w*seee*ee*s*w*sw*sw*eee*n*es*e*" 1000 6 9 `shouldBe` "111100\r\n100010\r\n100001\r\n100010\r\n111100\r\n100000\r\n100000\r\n100000\r\n100000"


    describe "should traverse simple and nested loops" $ do
      it "Your interpreter should not enter the loop on the first iteration of this program" $ do
        interpreter "*[es*]" 1 5 6 `shouldBe` "10000\r\n00000\r\n00000\r\n00000\r\n00000\r\n00000"
      it "Your interpreter should just have executed the last command of the loop and about to approach the matching \"]\"" $ do
        interpreter "*[es*]" 5 5 6 `shouldBe` "10000\r\n01000\r\n00000\r\n00000\r\n00000\r\n00000"
      it "Your interpreter should jump to the command straight *after* the matching \"[\" on the iteration where it hits the \"]\" and *not* the matching \"[\" itself" $ do
        interpreter "*[es*]" 9 5 6 `shouldBe` "10000\r\n01000\r\n00100\r\n00000\r\n00000\r\n00000"
      it "Your interpreter should should exhibit toroidal behaviour ;)" $ do
        interpreter "*[es*]" 37 5 6 `shouldBe` "11000\r\n01100\r\n00110\r\n00011\r\n00001\r\n10000"
      it "Your interpreter should exit the loop at the correct conditions" $ do
        interpreter "*[es*]" 1000 5 6 `shouldBe` "01111\r\n11111\r\n11111\r\n11111\r\n11111\r\n11111"
      it "Your interpreter should exit the loop at the correct conditions" $ do
        interpreter "*[es*]*" 3000 5 6 `shouldBe` "11111\r\n11111\r\n11111\r\n11111\r\n11111\r\n11111"
      it "Your interpreter should also work with nested loops" $ do
        interpreter "*[s[e]*]" 5 5 5 `shouldBe` "10000\r\n10000\r\n00000\r\n00000\r\n00000"
      it "Your interpreter should also work with nested loops" $ do
        interpreter "*[s[e]*]" 9 5 5 `shouldBe` "10000\r\n10000\r\n10000\r\n00000\r\n00000"
      it "Your interpreter should also work with nested loops" $ do
        interpreter "*[s[e]*]" 23 5 5 `shouldBe` "11000\r\n10000\r\n10000\r\n10000\r\n10000"
      it "Your interpreter should also work with nested loops" $ do
        interpreter "*[s[e]*]" 39 5 5 `shouldBe` "11000\r\n11000\r\n11000\r\n11000\r\n11000"
      it "Your interpreter should also work with nested loops" $ do
        interpreter "*[s[e]*]" 49 5 5 `shouldBe` "11100\r\n11100\r\n11000\r\n11000\r\n11000"
