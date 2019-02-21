module Simplificacion_de_expresiones_booleanas_Spec (main, spec) where

import Simplificacion_de_expresiones_booleanas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de valor simplifica" $
    verifica valor simplifica

verifica :: (Expr -> Bool -> Bool) -> (Expr -> Expr) -> Spec
verifica valor' simplifica' = do
  it "e1" $
    valor' (Neg X) True           `shouldBe`  False
  it "e2" $
    valor' (Neg F) True           `shouldBe`  True
  it "e3" $
    valor' (Dis X (Neg X)) True   `shouldBe`  True
  it "e4" $
    valor' (Dis X (Neg X)) False  `shouldBe`  True
  it "e5" $
    simplifica' (Dis X (Neg (Neg X)))                      `shouldBe`  X
  it "e6" $
    simplifica' (Neg (Dis (Neg (Neg X)) F))                `shouldBe`  Neg X
  it "e7" $
    simplifica' (Dis (Neg F) F)                            `shouldBe`  V
  it "e8" $
    simplifica' (Dis (Neg V) (Neg (Dis (Neg X) F)))        `shouldBe`  X
  it "e9" $
    simplifica' (Dis (Neg V) (Neg (Dis (Neg (Neg X)) F)))  `shouldBe`  Neg X
  it "p1" $ property $
    \p i -> valor (simplifica p) i == valor p i
  
