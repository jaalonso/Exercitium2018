module El_teorema_de_Navidad_de_Fermat_Spec (main, spec) where

import El_teorema_de_Navidad_de_Fermat
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de representaciones primosImparesConRepresentacionUnica primos4nM1" $
    verifica representaciones primosImparesConRepresentacionUnica primos4nM1
  describe "Verificacion de representaciones primosImparesConRepresentacionUnica primos4nM12" $
    verifica representaciones2 primosImparesConRepresentacionUnica primos4nM1
  describe "Verificacion de representaciones primosImparesConRepresentacionUnica primos4nM13" $
    verifica representaciones3 primosImparesConRepresentacionUnica primos4nM1
  describe "Verificacion de representaciones primosImparesConRepresentacionUnica primos4nM13" $
    verifica representaciones4 primosImparesConRepresentacionUnica primos4nM1

verifica :: (Integer -> [(Integer,Integer)])
            -> [Integer]
            -> [Integer]
            -> Spec
verifica f1 f2 f3 = do
  it "e1" $
    representaciones'  20 `shouldBe`  [(2,4)] 
  it "e2" $
    representaciones'  25 `shouldBe`  [(0,5),(3,4)] 
  it "e3" $
    representaciones' 325 `shouldBe`  [(1,18),(6,17),(10,15)]
  it "e4" $ 
    take 20 primosImparesConRepresentacionUnica' `shouldBe`
     [5,13,17,29,37,41,53,61,73,89,97,101,109,113,137,149,157,173,181,193]
  it "e5" $ 
    take 20 primos4nM1' `shouldBe`
     [5,13,17,29,37,41,53,61,73,89,97,101,109,113,137,149,157,173,181,193]
  where representaciones'                    = f1
        primosImparesConRepresentacionUnica' = f2
        primos4nM1'                          = f3



  
