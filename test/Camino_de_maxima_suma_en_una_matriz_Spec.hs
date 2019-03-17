module Camino_de_maxima_suma_en_una_matriz_Spec (main, spec) where

import Camino_de_maxima_suma_en_una_matriz
import Test.Hspec
import Test.QuickCheck
import Data.Matrix

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de caminoMaxSuma" $
    verifica caminoMaxSuma1
  describe "Verificacion de caminoMaxSuma2" $
    verifica caminoMaxSuma2
  describe "Verificacion de caminoMaxSuma3" $
    verifica caminoMaxSuma3

verifica :: (Matrix Int -> [Int]) -> Spec
verifica caminoMaxSuma' = do
  it "e1" $
    caminoMaxSuma' (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
    `shouldBe` [1,7,12,8,4,9]
  it "p1" $ property $
    \m -> sum (caminoMaxSuma' m) == sum (caminoMaxSuma3 m)

  
