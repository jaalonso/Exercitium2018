module Numeros_altamente_compuestos_Spec (main, spec) where

import Numeros_altamente_compuestos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de esAltamenteCompuesto" $
    verifica esAltamenteCompuesto
  describe "Verificacion de esAltamenteCompuesto2" $
    verifica esAltamenteCompuesto2

verifica :: (Int -> Bool) -> Spec
verifica f = do
  it "e1" $
    take 12 [n | n <- [1..], esAltamenteCompuesto' n] `shouldBe`
      [1,2,4,6,12,24,36,48,60,120,180,240]
  where esAltamenteCompuesto' = f

  
