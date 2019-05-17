module BoolfuckSpec where
  
import Boolfuck 
import Test.Hspec
import Data.Char        (ord)
import Data.List.Zipper (empty, endp)

spec :: Spec
spec =
  describe "interpret Boolfuck" $ do
    it "Empty tests" $
      boolfuck "" "" `shouldBe` ""

    it "Single command tests" $ do
      boolfuck "<" "" `shouldBe` ""
      boolfuck ">" "" `shouldBe` ""
      boolfuck "+" "" `shouldBe` ""
      boolfuck "." "" `shouldBe` ""
      boolfuck ";" "" `shouldBe` "\x0000"

    it "Hello World test" $
      boolfuck ";;;+;+;;+;+;+;+;+;+;;+;;+;;;+;;+;+;;+;;;+;;+;+;;+;+;;;;+;+;;+;;;+;;+;+;+;;;;;;;+;+;;+;;;+;+;;;+;+;;;;+;+;;+;;+;+;;+;;;+;;;+;;+;+;;+;;;+;+;;+;;+;+;+;;;;+;+;;;+;+;+;" "" `shouldBe` "Hello, world!\n"

    it "Basic tests" $ do
      boolfuck ">,>,>,>,>,>,>,>,<<<<<<<[>]+<[+<]>>>>>>>>>[+]+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]<<<<<<<<;>;>;>;>;>;>;>;<<<<<<<,>,>,>,>,>,>,>,<<<<<<<[>]+<[+<]>>>>>>>>>[+]+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]" "Codewars\x00ff" `shouldBe` "Codewars"
      boolfuck ">,>,>,>,>,>,>,>,>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>;>;>;>;>;>;>;>;>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]>,>,>,>,>,>,>,>,>+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]" "Codewars" `shouldBe` "Codewars"
      boolfuck ">,>,>,>,>,>,>,>,>>,>,>,>,>,>,>,>,<<<<<<<<+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>>>>>>>>>>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]>[>]+<[+<]>>>>>>>>>[+]>[>]+<[+<]>>>>>>>>>[+]<<<<<<<<<<<<<<<<<<+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]>>>>>>>>>>>>>>>>>>>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]<<<<<<<<<<<<<<<<<<<<<<<<<<[>]+<[+<]>>>>>>>>>[+]>>>>>>>>>>>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]<<<<<<<<<<<<<<<<<<+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]>>>>>>>>>>>>>>>>>>>;>;>;>;>;>;>;>;<<<<<<<<" "\x0008\x0009" `shouldBe` "\x0048"

    it "Encoding tests" $ do
      let bits1 = [True, False, False, False, False, True, True, False]
      ord 'a' `shouldBe` 97 
      strToBools  "a"   `shouldBe` bits1
      strToBools  "aa"  `shouldBe` (bits1 ++ bits1)
      boolsToStr  bits1 `shouldBe` "a"
      boolsToStr (bits1 ++ bits1) `shouldBe` "aa"

    it "State tests" $ do
      let s = buildState "" ""
      commands s `shouldBe` empty
      input s `shouldBe` []
      output s `shouldBe` []
      endp (commands s) `shouldBe` True
      boolsToStr (output s) `shouldBe` ""