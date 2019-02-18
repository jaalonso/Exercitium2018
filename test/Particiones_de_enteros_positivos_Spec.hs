module Particiones_de_enteros_positivos_Spec (main, spec) where

import Particiones_de_enteros_positivos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de particiones" $
    verifica particiones
  describe "Verificacion de particiones2" $
    verifica particiones2

verifica :: (Int -> [[Int]]) -> Spec
verifica f = do
  it "e1" $
    particiones' 4  `shouldMatchList`
    [[4],[3,1],[2,2],[2,1,1],[1,1,1,1]]
  it "e2" $
    particiones' 5  `shouldMatchList`
    [[5],[4,1],[3,2],[3,1,1],[2,2,1],[2,1,1,1],[1,1,1,1,1]]
  where 
    particiones' = f 

  
