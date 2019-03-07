module Descomposiciones_en_sumas_de_cuadrados_Spec (main, spec) where

import Descomposiciones_en_sumas_de_cuadrados
import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de descomposiciones" $
    verifica descomposiciones
  describe "Verificacion de descomposiciones2" $
    verifica descomposiciones2

verifica :: (Int -> [[Int]]) -> Spec
verifica descomposiciones' = do
  it "e1" $
    descomposiciones'' 4 `shouldBe`
    [[1,1,1,1]]
  it "e2" $
    descomposiciones'' 5 `shouldBe`
    []
  it "e3" $
    descomposiciones'' 7 `shouldBe`
    [[1,1,1,4],[1,1,4,1],[1,4,1,1],[4,1,1,1]]
  it "e4" $
    descomposiciones'' 10 `shouldBe`
    [[1,1,4,4],[1,4,1,4],[1,4,4,1],[4,1,1,4],[4,1,4,1],[4,4,1,1]]
  it "e5" $
    descomposiciones'' 15 `shouldBe`
    [[1,1,4,9],[1,1,9,4],[1,4,1,9],[1,4,9,1],[1,9,1,4],[1,9,4,1],
     [4,1,1,9],[4,1,9,1],[4,9,1,1],[9,1,1,4],[9,1,4,1],[9,4,1,1]]
  it "p1" $ property $
    \x -> x > 0 ==> sort (descomposiciones' x) ==
                    sort (descomposiciones2 x)
  where descomposiciones'' = sort . descomposiciones'
