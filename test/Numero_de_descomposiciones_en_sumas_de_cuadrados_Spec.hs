module Numero_de_descomposiciones_en_sumas_de_cuadrados_Spec (main, spec) where

import Numero_de_descomposiciones_en_sumas_de_cuadrados
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de nDescomposiciones" $
    verifica nDescomposiciones
  describe "Verificacion de nDescomposiciones2" $
    verifica nDescomposiciones2
  describe "Verificacion de nDescomposiciones3" $
    verifica nDescomposiciones3
  describe "Verificacion de nDescomposiciones4" $
    verifica nDescomposiciones4

verifica :: (Int -> Int) -> Spec
verifica nDescomposiciones' = do
  it "e1" $
    nDescomposiciones' 4      `shouldBe`  1
  it "e2" $
    nDescomposiciones' 5      `shouldBe`  0
  it "e3" $
    nDescomposiciones' 7      `shouldBe`  4
  it "e4" $
    nDescomposiciones' 10     `shouldBe`  6
  it "e5" $
    nDescomposiciones' 15     `shouldBe`  12
  it "p1" $ property $
    \x -> x >=0 ==> nDescomposiciones' x == nDescomposiciones4 x

  
