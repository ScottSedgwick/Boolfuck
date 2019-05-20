import qualified MiniStringFuckSpec as MSF
import qualified SmallfuckSpec as SF
import qualified BoolfuckSpec as BF

import Test.Hspec

main :: IO ()
main = hspec $ do
  MSF.spec
  SF.spec
  BF.spec