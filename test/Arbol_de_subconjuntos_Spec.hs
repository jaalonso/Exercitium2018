module Arbol_de_subconjuntos_Spec (main, spec) where

import Arbol_de_subconjuntos
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize) 
import Test.QuickCheck
import Data.List (nub)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de arbolSubconjuntos nOcurrenciasArbolSubconjuntos" $
    verifica arbolSubconjuntos nOcurrenciasArbolSubconjuntos

verifica :: ([Int] -> Arbol)
            -> ([Int] -> [Int] -> Int)
            -> Spec
verifica f g = do
  it "e1" $
    arbolSubconjuntos' [2,5,3] `shouldBe`arbolSubconjuntos [2,5,3]    
  it "e2" $
    nOcurrenciasArbolSubconjuntos' []      [2,5,3]  `shouldBe`  6
  it "e3" $
    nOcurrenciasArbolSubconjuntos' [3]     [2,5,3]  `shouldBe`  2
  it "e4" $
    nOcurrenciasArbolSubconjuntos' [3,5]   [2,5,3]  `shouldBe`  1
  it "e5" $
    nOcurrenciasArbolSubconjuntos' [3,5,2] [2,5,3]  `shouldBe`  1
  modifyMaxSize (const 5) $ it "p1" $ property $
    \xs -> arbolSubconjuntos' xs == arbolSubconjuntos xs
  modifyMaxSize (const 5) $ it "p2" $ property $
    \n  xs -> n > 0 ==>
        let ys = nub [1 + x `mod` n | x <- xs]
            k  = length ys
            factorial m = product [1..m]
        in nOcurrenciasArbolSubconjuntos ys [1..n] ==
           factorial (n-k)
  where arbolSubconjuntos'             = f
        nOcurrenciasArbolSubconjuntos' = g
  
