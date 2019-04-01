module El_problema_del_numero_perdido_Spec (main, spec) where

import El_problema_del_numero_perdido
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de numeroPerdido" $
    verifica numeroPerdido
  describe "Verificacion de numeroPerdido2" $
    verifica numeroPerdido2
  describe "Verificacion de numeroPerdido3" $
    verifica numeroPerdido3

verifica :: ([Int] -> Maybe Int) -> Spec
verifica numeroPerdido' = do
  it "e1" $
    numeroPerdido' [7,6,4,3]   `shouldBe` Just 5
  it "e2" $
    numeroPerdido' [1,2,4,5,6] `shouldBe` Just 3
  it "e3" $
    numeroPerdido' [6,5..3]    `shouldBe` Nothing
  it "e4" $
    numeroPerdido' [1..6]      `shouldBe` Nothing

  
