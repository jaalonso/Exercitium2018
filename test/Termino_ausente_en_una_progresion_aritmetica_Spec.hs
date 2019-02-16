module Termino_ausente_en_una_progresion_aritmetica_Spec (main, spec) where

import Termino_ausente_en_una_progresion_aritmetica
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de ausente" $
    verifica ausente
  describe "Verificacion de ausente2" $
    verifica ausente2
  describe "Verificacion de ausente3" $
    verifica ausente3

verifica :: ([Int] -> Int) -> Spec
verifica f = do
  it "e1" $
    ausente' [3,7,9,11]        `shouldBe`  5   
  it "e2" $
    ausente' [3,5,9,11]        `shouldBe`  7   
  it "e3" $
    ausente' [3,5,7,11]        `shouldBe`  9   
  it "e4" $
    ausente' ([1..9]++[11..])  `shouldBe`  10  
  it "e5" $
    ausente' [11,7,5,3]        `shouldBe`  9   
  it "e6" $
    ausente' [11,9,5,3]        `shouldBe`  7   
  it "e7" $
    ausente' [1,5,7]           `shouldBe`  3
  it "e8" $
    ausente' [3,7,9,11]        `shouldBe`  5   
  it "e9" $
    ausente' [3,5,9,11]        `shouldBe`  7   
  it "e10" $
    ausente' [3,5,7,11]        `shouldBe`  9   
  it "e11" $
    ausente' ([1..9]++[11..])  `shouldBe`  10  
  it "e12" $
    ausente' [11,7,5,3]        `shouldBe`  9   
  it "e13" $
    ausente' [11,9,5,3]        `shouldBe`  7   
  it "e14" $
    ausente' [1,5,7]           `shouldBe`  3
  where 
    ausente' = f

  
