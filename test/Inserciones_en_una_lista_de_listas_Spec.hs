module Inserciones_en_una_lista_de_listas_Spec (main, spec) where

import Inserciones_en_una_lista_de_listas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de inserta" $
    verifica inserta
  describe "Verificacion de inserta2" $
    verifica inserta2

verifica :: (Int -> [[Int]] -> [[[Int]]]) -> Spec
verifica inserta' = do
  it "e1" $
    inserta' 1 [[2,3],[4],[5,6,7]] `shouldBe`
    [[[1,2,3],[4],[5,6,7]],[[2,3],[1,4],[5,6,7]],[[2,3],[4],[1,5,6,7]]]
  it "p1" $ property $
    propiedad
  where
    propiedad :: Int -> [[Int]] -> Bool
    propiedad x ys = inserta' x ys == inserta x ys
  
