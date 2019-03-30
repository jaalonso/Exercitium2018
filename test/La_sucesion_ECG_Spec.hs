module La_sucesion_ECG_Spec (main, spec) where

import La_sucesion_ECG
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de ?fun" $
    verifica sucECG

verifica :: [Integer] -> Spec
verifica sucECG' = do
  it "e1" $
    take 20 sucECG' `shouldBe`
     [1,2,4,6,3,9,12,8,10,5,15,18,14,7,21,24,16,20,22,11]
  
