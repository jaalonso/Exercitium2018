module Matriz_girada_180_grados_Spec (main, spec) where

import Matriz_girada_180_grados
import Test.Hspec
import Data.Matrix 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de matrizGirada180" $
    verifica matrizGirada180
  describe "Verificacion de matrizGirada1802" $
    verifica matrizGirada180b
  describe "Verificacion de matrizGirada1803" $
    verifica matrizGirada180c

verifica :: (Matrix Int -> Matrix Int) -> Spec
verifica matrizGirada180' = do
  it "e1" $
    toLists (matrizGirada180' (fromList 4 3 [1..])) `shouldBe`
    [[12,11,10],[9,8,7],[6,5,4],[3,2,1]]
  it "e2" $  
    toLists (matrizGirada180' (fromList 3 4 [1..])) `shouldBe`
    [[12,11,10,9],[8,7,6,5],[4,3,2,1]]
  
