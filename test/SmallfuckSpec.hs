module SmallfuckSpec where

import Smallfuck

import Test.Hspec

spec :: Spec
spec = do
  describe "SmallFuck Example Tests" $ do
    it "should work for some example test cases" $ do
      -- Flips the leftmost cell of the tape
      interpreter "*" "00101100" `shouldBe` "10101100"
      -- Flips the second and third cell of the tape
      interpreter ">*>*" "00101100" `shouldBe` "01001100"
      -- Flips all the bits in the tape
      interpreter "*>*>*>*>*>*>*>*" "00101100" `shouldBe` "11010011"
      -- Flips all the bits that are initialized to 0
      interpreter "*>*>>*>>>*>*" "00101100" `shouldBe` "11111111"
      -- Goes somewhere to the right of the tape and then flips all bits that
      -- are initialized to 1, progressing leftwards through the tape 
      interpreter ">>>>>*<*<<*" "00101100" `shouldBe` "00000000"
    it "should ignore non-command characters" $ do
      -- Your interpreter should ignore all non-command characters
      interpreter "iwmlis *!BOSS 333 ^v$#@" "00101100" `shouldBe` "10101100"
      -- Your interpreter should not treat any of \"+\", \"-\", \",\", \".\" (valid brainfuck commands) and \";\" as valid command characters
      interpreter ">*>*;;;.!.,+-++--!!-!!!-" "00101100" `shouldBe` "01001100"
      -- Your interpreter should ignore all tabs, newlines and spaces
      interpreter "    *  >\n    *           >\n    \n*>*lskdfjsdklfj>*;;+--+--+++--+-+-  lskjfiom,x  \n>*sdfsdf>sdfsfsdfsdfwervbnbvn*,.,.,,.,.  >\n\n\n*" "00101100" `shouldBe` "11010011"
      -- 
      interpreter "*,,...,..,..++-->++++-*>--+>*>+++->>..,+-,*>*" "00101100" `shouldBe` "11111111"
      -- Your interpreter should not recognise any of \"n\", \"e\", \"s\", \"w\" (all valid Paintfuck commands) as valid commands
      interpreter ">>nssewww>>wwess>*<nnn*<<ee*" "00101100" `shouldBe` "00000000"
  

    describe "Out of Bounds Checks" $ do
      it "should return the final state of the tape immediately when the pointer moves too far to the right" $ do
        interpreter "*>>>*>*>>*>>>>>>>*>*>*>*>>>**>>**" "0000000000000000" `shouldBe` "1001101000000111"
      it "should immediately return the final state of the tape at the first instance where the pointer goes out of bounds to the left even if it resumes to a valid position later in the program" $ do
        interpreter "<<<*>*>*>*>*>>>*>>>>>*>*" "0000000000000000" `shouldBe` "0000000000000000"
        interpreter "*>*>*>>>*>>>>>*<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*>*>*" "11111111111111111111111111111111" `shouldBe` "00011011110111111111111111111111"
      it "should not follow through any command after the pointer goes out of bounds for the first time" $ do
        interpreter ">>*>*>*<<*<*<<*>*" "1101" `shouldBe` "1110"
  

    describe "Simple and Nested Loops" $ do
      it "should evaluate a simple non-nested loop properly" $ do
        interpreter "*[>*]" (replicate 256 '0') `shouldBe` (replicate 256 '1')
      it "should jump to the matching \"]\" when it encounters a \"[\" and the bit under the pointer is 0 " $ do
        interpreter "[>*]" (replicate 256 '0') `shouldBe` (replicate 256 '0')
      it "should jump to the matching \"]\" when it encounters a \"[\" and the bit under the pointer is 0" $ do
        interpreter "*>*>>>*>*>>>>>*>[>*]" (replicate 256 '0') `shouldBe` ("11001100001" ++ replicate 245 '0')
      it "should jump back to the matching \"[\" when it encounters a \"]\" and the bit under the pointer is nonzero " $ do
        interpreter "*>*>>>*>*>>>>>*[>*]" (replicate 256 '0') `shouldBe` ("11001100001" ++ replicate 245 '1')
      it "should also work properly with nested loops" $ do
        interpreter "*[>[*]]" (replicate 256 '0') `shouldBe` ('1' : replicate 255 '0')
        interpreter "*[>[*]]" (replicate 256 '1') `shouldBe` ('0' : replicate 255 '1')
        interpreter "[[]*>*>*>]" "000" `shouldBe` "000"
        interpreter "*>[[]*>]<*" "100" `shouldBe` "100"
        interpreter "[*>[>*>]>]" "11001" `shouldBe` "01100"
        interpreter "[>[*>*>*>]>]" "10110" `shouldBe` "10101"
  