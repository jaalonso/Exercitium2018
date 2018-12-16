module Elemento_del_arbol_binario_completo_segun_su_posicion_Spec (main, spec) where

import Elemento_del_arbol_binario_completo_segun_su_posicion
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de elementoEnPosicion" $
    verifica elementoEnPosicion
  describe "Verificacion de elementoEnPosicion2" $
    verifica elementoEnPosicion2

verifica :: (Posicion -> Integer) -> Spec
verifica f = do
  it "e1" $
    elementoEnPosicion' [D,I]    `shouldBe`  6
  it "e2" $
    elementoEnPosicion' [D,D]    `shouldBe`  7
  it "e3" $
    elementoEnPosicion' [I,I,D]  `shouldBe`  9
  it "e4" $
    elementoEnPosicion' []       `shouldBe`  1
  it "p1" $ property $
    \n -> n > 0 ==>
          let ps = posicionDeElemento n
          in elementoEnPosicion' ps == n
  where elementoEnPosicion' = f
  
