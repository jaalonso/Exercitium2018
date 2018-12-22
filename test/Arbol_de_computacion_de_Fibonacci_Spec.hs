module Arbol_de_computacion_de_Fibonacci_Spec (main, spec) where

import Arbol_de_computacion_de_Fibonacci
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de arbolFib nElementosArbolFib" $
    verifica arbolFib nElementosArbolFib
  describe "Verificacion de arbolFib2 nElementosArbolFib2" $
    verifica arbolFib2 nElementosArbolFib2
  describe "Verificacion de arbolFib3 nElementosArbolFib" $
    verifica arbolFib3 nElementosArbolFib

verifica :: (Int -> Arbol) -> (Int -> Int) -> Spec
verifica f g = do
  it "e1" $
    and [f n == arbolFib3 n | n <- [0..15]]
  it "e2" $
    and [g n == nElementosArbolFib2 n | n <- [0..15]]
  
