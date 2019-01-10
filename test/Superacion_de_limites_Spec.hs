module Superacion_de_limites_Spec (main, spec) where

import Superacion_de_limites
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de nuevosMaximos nuevosMinimos nRupturas" $
    verifica nuevosMaximos nuevosMinimos nRupturas

verifica :: ([Int] -> [Int])
            -> ([Int] -> [Int])
            -> ([Int] -> (Int,Int))
            -> Spec
verifica f1 f2 f3 = do
  it "e1" $
    nuevosMaximos' [7,5,9,9,4,5,4,2,5,9,12,1]  `shouldBe`  [7,9,12]
  it "e2" $
    nuevosMinimos' [7,5,9,9,4,5,4,2,5,9,12,1]  `shouldBe`  [7,5,4,2,1]
  it "e3" $
    nRupturas' [7,5,9,9,4,5,4,2,5,9,12,1]  `shouldBe`  (2,4)
  where nuevosMaximos' = f1
        nuevosMinimos' = f2
        nRupturas'     = f3

  
