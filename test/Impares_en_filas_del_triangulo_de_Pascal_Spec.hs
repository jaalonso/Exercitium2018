module Impares_en_filas_del_triangulo_de_Pascal_Spec (main, spec) where

import Impares_en_filas_del_triangulo_de_Pascal
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de imparesPascal nImparesPascal" $
    verifica imparesPascal nImparesPascal
  describe "Verificacion de imparesPascal nImparesPascal2" $
    verifica imparesPascal2 nImparesPascal2
  describe "Verificacion de imparesPascal nImparesPascal3" $
    verifica imparesPascal2 nImparesPascal3

verifica :: [[Integer]] -> [Int] -> Spec
verifica f g = do
  it "e1" $
    take 8 imparesPascal' `shouldBe`
    [[1],[1,1],[1,1],[1,3,3,1],[1,1],[1,5,5,1],[1,15,15,1],[1,7,21,35,35,21,7,1]]
  it "e2" $
    take 32 nImparesPascal' `shouldBe`
    [1,2,2,4,2,4,4,8,2,4,4,8,4,8,8,16,2,4,4,8,4,8,8,16,4,8,8,16,8,16,16,32]
  where
    imparesPascal'  = f          
    nImparesPascal' = g

      

