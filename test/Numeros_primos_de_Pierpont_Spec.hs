module Numeros_primos_de_Pierpont_Spec (main, spec) where

import Numeros_primos_de_Pierpont
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de primosPierpont" $
    verifica primosPierpont

verifica :: [Integer] -> Spec
verifica f = do
  it "e1" $
    take 20 primosPierpont' `shouldBe` 
     [2,3,5,7,13,17,19,37,73,97,109,163,193,257,433,487,577,769,1153,1297]
  where primosPierpont' = f
  
