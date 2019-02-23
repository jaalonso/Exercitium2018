module Numeros_primos_en_pi_Spec (main, spec) where

import Numeros_primos_en_pi
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de nOcurrenciasPrimosEnPi" $
    verifica nOcurrenciasPrimosEnPi

verifica :: (Int -> Int -> IO [Int]) -> Spec
verifica f = do
  it "e1" $ 
    nOcurrenciasPrimosEnPi' 4 20
      >>= (`shouldBe` [2,3,3,1])
  it "e2" $ 
    nOcurrenciasPrimosEnPi 10 100
      >>= (`shouldBe` [12,11,8,8,1,0,1,1,2,0])
  it "e3" $ 
    nOcurrenciasPrimosEnPi 10 (10^4)
      >>= (`shouldBe` [1021,974,1046,970,99,102,90,113,99,95])
  where nOcurrenciasPrimosEnPi' = f
  
