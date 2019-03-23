module Cambio_con_el_menor_numero_de_monedas_Spec (main, spec) where

import Cambio_con_el_menor_numero_de_monedas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de monedas" $
    verifica monedas
  describe "Verificacion de monedas2" $
    verifica monedas2
  describe "Verificacion de monedas3" $
    verifica monedas3

verifica :: ([Int] -> Int -> Maybe Int) -> Spec
verifica monedas' = do
  it "e1" $
    monedas' [1,3,4]  6                    `shouldBe`  Just 2
  it "e2" $
    monedas' [2,5,10] 3                    `shouldBe`  Nothing

  
