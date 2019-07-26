module BrainfuckSpec where

import Brainfuck 
import Test.Hspec

spec :: Spec
spec =
  describe "Brainfuck Example Tests" $ do
    it "Empty test" $ do
      brainfuck "" "" `shouldBe` ""
    it "Hello World tests" $ do
      brainfuck "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." "" `shouldBe` "Hello World!\n"
      brainfuck "+[>[<->+[>+++>[+++++++++++>][]-[<]>-]]++++++++++<]>>>>>>----.<<+++.<-..+++.<-.>>>.<<.+++.------.>-.<<+.<." "" `shouldBe` "Hello World!\n"