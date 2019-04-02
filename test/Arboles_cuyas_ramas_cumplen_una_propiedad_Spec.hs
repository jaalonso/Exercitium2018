module Arboles_cuyas_ramas_cumplen_una_propiedad_Spec (main, spec) where

import Arboles_cuyas_ramas_cumplen_una_propiedad
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de todasDesdeAlguno" $
    verifica todasDesdeAlguno
  describe "Verificacion de todasDesdeAlguno2" $
    verifica todasDesdeAlguno2

verifica :: ((Int -> Bool) -> Arbol Int -> Bool) -> Spec
verifica todasDesdeAlguno' = do
  it "e1" $
    todasDesdeAlguno' (>0) ej1 `shouldBe` True
  it "e2" $
    todasDesdeAlguno' (>0) ej2 `shouldBe` False
  it "e3" $
    todasDesdeAlguno' (>0) ej3 `shouldBe` True
  
