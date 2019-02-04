module Numeros_con_digitos_1_y_2_Spec (main, spec) where

import Numeros_con_digitos_1_y_2
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de numerosCon1y2 restosNumerosCon1y2" $
    verifica numerosCon1y2 restosNumerosCon1y2
  describe "Verificacion de numerosCon1y22 restosNumerosCon1y2" $
    verifica numerosCon1y22 restosNumerosCon1y2
  describe "Verificacion de numerosCon1y23 restosNumerosCon1y2" $
    verifica numerosCon1y23 restosNumerosCon1y2

verifica :: (Int -> [Int]) -> (Int -> [Int]) -> Spec
verifica f g = do
  it "e1" $
    numerosCon1y2' 2  `shouldBe`  [11,12,21,22] 
  it "e2" $
    numerosCon1y2' 3  `shouldBe`  [111,112,121,122,211,212,221,222]
  it "e3" $
    restosNumerosCon1y2' 2 `shouldBe` [3,0,1,2]
  it "e4" $
    restosNumerosCon1y2' 3 `shouldBe` [7,0,1,2,3,4,5,6]
  it "e5" $
    restosNumerosCon1y2' 4 `shouldBe` [7,8,1,2,11,12,5,6,15,0,9,10,3,4,13,14]
  where
    numerosCon1y2'       = f
    restosNumerosCon1y2' = g
  
    
