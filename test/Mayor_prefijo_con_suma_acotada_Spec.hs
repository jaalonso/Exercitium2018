module Mayor_prefijo_con_suma_acotada_Spec (main, spec) where

import Mayor_prefijo_con_suma_acotada
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de mayorPrefijoAcotado" $
    verifica mayorPrefijoAcotado
  describe "Verificacion de mayorPrefijoAcotado2" $
    verifica mayorPrefijoAcotado2
  describe "Verificacion de mayorPrefijoAcotado3" $
    verifica mayorPrefijoAcotado3

verifica :: ([Int] -> Int -> [Int]) -> Spec
verifica f = do
  it "e1" $
    mayorPrefijoAcotado' [45,30,55,20,80,20] 75   `shouldBe`  [45,30]
  it "e2" $
    mayorPrefijoAcotado' [45,30,55,20,80,20] 140  `shouldBe`  [45,30,55]
  it "e3" $
    mayorPrefijoAcotado' [45,30,55,20,80,20] 180  `shouldBe`  [45,30,55,20]
  it "p1" $ property $
    prop_verifica mayorPrefijoAcotado'
  where mayorPrefijoAcotado' = f
  
prop_verifica :: ([Int] -> Int -> [Int])
                 -> [Int]
                 -> Int
                 -> Bool
prop_verifica f xs y =
  f xs' y' == mayorPrefijoAcotado xs' y'
  where xs' = map abs xs
        y'  = abs y
