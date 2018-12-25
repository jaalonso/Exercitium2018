module Intercambio_de_la_primera_y_ultima_columna_de_una_matriz_Spec (main, spec) where

import Intercambio_de_la_primera_y_ultima_columna_de_una_matriz
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de intercambia" $
    verifica intercambia

verifica :: ([[Int]] -> [[Int]]) -> Spec
verifica f = do
  it "e1" $ 
   intercambia' [[8,9,7,6],[4,7,6,5],[3,2,1,8]] `shouldBe`
     [[6,9,7,8],[5,7,6,4],[8,2,1,3]]
  where intercambia' = f

  
