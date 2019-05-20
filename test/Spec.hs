import qualified MiniStringFuckSpec as MSF
import qualified BoolfuckSpec as BF
import Test.Hspec

main :: IO ()
main = hspec $ do
  MSF.spec
  BF.spec