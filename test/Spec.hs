import qualified MiniStringFuckSpec as MSF
import qualified SmallfuckSpec as SF
import qualified PaintfuckSpec as PF
import qualified BoolfuckSpec as BF
import qualified BrainfuckSpec as BRF

import Test.Hspec

main :: IO ()
main = hspec $ do
  MSF.spec
  SF.spec
  PF.spec
  BF.spec
  BRF.spec