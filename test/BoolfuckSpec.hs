module BoolfuckSpec where
  
import Boolfuck 
import Test.Hspec
import Data.Char (ord)
import Data.List.Zipper

spec :: Spec
spec = do
  describe "interpret Boolfuck" $ do
    it "Empty tests" $ do
      boolfuck "" "" `shouldBe` ""

    it "Single command tests" $ do
      boolfuck "<" "" `shouldBe` ""
      boolfuck ">" "" `shouldBe` ""
      boolfuck "+" "" `shouldBe` ""
      boolfuck "." "" `shouldBe` ""
      boolfuck ";" "" `shouldBe` "\x0000"

    it "Hello World test" $ do
      boolfuck ";;;+;+;;+;+;+;+;+;+;;+;;+;;;+;;+;+;;+;;;+;;+;+;;+;+;;;;+;+;;+;;;+;;+;+;+;;;;;;;+;+;;+;;;+;+;;;+;+;;;;+;+;;+;;+;+;;+;;;+;;;+;;+;+;;+;;;+;+;;+;;+;+;+;;;;+;+;;;+;+;+;" "" `shouldBe` "Hello, world!\n"

    it "Basic tests" $ do
      boolfuck ">,>,>,>,>,>,>,>,<<<<<<<[>]+<[+<]>>>>>>>>>[+]+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]<<<<<<<<;>;>;>;>;>;>;>;<<<<<<<,>,>,>,>,>,>,>,<<<<<<<[>]+<[+<]>>>>>>>>>[+]+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]" "Codewars\x00ff" `shouldBe` "Codewars"
      boolfuck ">,>,>,>,>,>,>,>,>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>;>;>;>;>;>;>;>;>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]>,>,>,>,>,>,>,>,>+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]" "Codewars" `shouldBe` "Codewars"
      boolfuck ">,>,>,>,>,>,>,>,>>,>,>,>,>,>,>,>,<<<<<<<<+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>>>>>>>>>>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]>[>]+<[+<]>>>>>>>>>[+]>[>]+<[+<]>>>>>>>>>[+]<<<<<<<<<<<<<<<<<<+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]>>>>>>>>>>>>>>>>>>>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]<<<<<<<<<<<<<<<<<<<<<<<<<<[>]+<[+<]>>>>>>>>>[+]>>>>>>>>>>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]<<<<<<<<<<<<<<<<<<+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]>>>>>>>>>>>>>>>>>>>;>;>;>;>;>;>;>;<<<<<<<<" "\x0008\x0009" `shouldBe` "\x0048"

    it "Encoding tests" $ do
      let bits1 = [One, Zero, Zero, Zero, Zero, One, One, Zero]
      ord 'a' `shouldBe` 97 
      byteToBits 97 `shouldBe` bits1
      strToBits "a" `shouldBe` bits1
      strToBits "aa" `shouldBe` (bits1 ++ bits1)
      bitsToByte bits1 `shouldBe` 97
      bitsToStr bits1 `shouldBe` "a"
      bitsToStr (bits1 ++ bits1) `shouldBe` "aa"

    it "State tests" $ do
      let s = buildState "" ""
      commands s `shouldBe` empty
      input s `shouldBe` []
      output s `shouldBe` []
      endp (commands s) `shouldBe` True
      bitsToStr (output s) `shouldBe` ""