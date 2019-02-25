module Ternas_euclideas_Spec (main, spec) where

import Ternas_euclideas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de ternasEuclideas esMayorDeTernaEuclidea" $
    verifica ternasEuclideas esMayorDeTernaEuclidea

verifica :: [(Integer,Integer,Integer)] -> (Integer -> Bool) -> Spec
verifica ternasEuclideas' esMayorDeTernaEuclidea' = do
  it "e1" $ 
    take 7 ternasEuclideas'
    `shouldBe` [(1,3,8),(2,4,12),(1,8,15),(3,5,16),(4,6,20),(3,8,21),(5,7,24)]
  it "e2" $ 
    esMayorDeTernaEuclidea' 20  `shouldBe`  True
  it "e3" $ 
    esMayorDeTernaEuclidea' 22  `shouldBe`  False
  it "e4" $ 
    map esMayorDeTernaEuclidea' [1..30] `shouldBe`
    map esMayorDeTernaEuclidea  [1..30]
  
