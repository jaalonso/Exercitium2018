module Distancia_de_Hamming_Spec (main, spec) where

import Distancia_de_Hamming
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de distancia" $
    verifica distancia
  describe "Verificacion de distancia2" $
    verifica distancia2

verifica :: (String -> String -> Int) -> Spec
verifica f = do
  it "e1" $
    distancia0 "romano" "comino"  `shouldBe`  2
  it "e2" $
    distancia0 "romano" "camino"  `shouldBe`  3
  it "e3" $
    distancia0 "roma"   "comino"  `shouldBe`  2
  it "e4" $
    distancia0 "roma"   "camino"  `shouldBe`  3
  it "e5" $
    distancia0 "romano" "ron"     `shouldBe`  1
  it "e6" $
    distancia0 "romano" "cama"    `shouldBe`  2
  it "e7" $
    distancia0 "romano" "rama"    `shouldBe`  1
  it "p1" $ property $
    \ xs ys -> f xs ys == distancia2 xs ys
  where distancia0 = f
