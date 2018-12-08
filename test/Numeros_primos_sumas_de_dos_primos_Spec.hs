module Numeros_primos_sumas_de_dos_primos_Spec (main, spec) where

import Numeros_primos_sumas_de_dos_primos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de primosSumaDeDosPrimos" $
    verifica primosSumaDeDosPrimos
  describe "Verificacion de primosSumaDeDosPrimos2" $
    verifica primosSumaDeDosPrimos2

verifica :: [Integer] -> Spec
verifica f = do
  it "e1" $
    take 17 primosSumaDeDosPrimos
    `shouldBe` [5,7,13,19,31,43,61,73,103,109,139,151,181,193,199,229,241]
  it "p1" $ property $
    \n -> n >= 0 ==> primosSumaDeDosPrimos !! n == primosSumaDeDosPrimos2 !! n
  where primosSumaDeDosPrimos = f
  
