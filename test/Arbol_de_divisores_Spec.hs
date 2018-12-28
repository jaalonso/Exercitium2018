module Arbol_de_divisores_Spec (main, spec) where

import Arbol_de_divisores
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de arbolDivisores nOcurrenciasArbolDivisores" $
    verifica arbolDivisores nOcurrenciasArbolDivisores

verifica :: (Integer -> Arbol)
            -> (Integer -> Integer -> Integer)
            -> Spec
verifica f g = do
  it "e1" $
    arbolDivisores' 30 `shouldBe`
     N 30 [N 6  [N 2 [N 1 []],N 3 [N 1 []]],
           N 10 [N 2 [N 1 []],N 5 [N 1 []]],
           N 15 [N 3 [N 1 []],N 5 [N 1 []]]]
  it "e2" $
    [nOcurrenciasArbolDivisores' k 630 | k <- [1..630], 630 `mod` k == 0]
     `shouldBe` [60,12,24,12,6,12,6,3,3,6,2,6,2,3,2,2,2,1,1,2,1,1,1,1]
  where arbolDivisores'             = f
        nOcurrenciasArbolDivisores' = g 
  
