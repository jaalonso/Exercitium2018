module Arboles_con_n_elementos_Spec (main, spec) where

import Arboles_con_n_elementos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de arboles nArboles" $
    verifica arboles nArboles
  describe "Verificacion de arboles nArboles2" $
    verifica arboles2 nArboles2
  describe "Verificacion de arboles nArboles3" $
    verifica arboles2 nArboles3
  
verifica :: (Integer -> Int -> [Arbol Int])
         -> [Integer]
         -> Spec
verifica f g = do
   it "e1" $  
     arboles' 0 7 `shouldBe`
       []
   it "e2" $  
     arboles' 1 7 `shouldBe`
       [H 7]
   it "e3" $  
     arboles' 2 7 `shouldBe`
       []
   it "e4" $   
     arboles' 3 7 `shouldBe`
       [N 7 (H 7) (H 7)]
   it "e5" $    
     arboles' 4 7 `shouldBe`
       []
   it "e6" $    
     arboles' 5 7 `shouldBe`
       [N 7 (H 7) (N 7 (H 7) (H 7)),N 7 (N 7 (H 7) (H 7)) (H 7)]
   it "e7" $    
     arboles' 6 7 `shouldBe`
       []
   it "e8" $    
     arboles' 7 7 `shouldMatchList`
       [N 7 (H 7) (N 7 (H 7) (N 7 (H 7) (H 7))),
        N 7 (H 7) (N 7 (N 7 (H 7) (H 7)) (H 7)),
        N 7 (N 7 (H 7) (H 7)) (N 7 (H 7) (H 7)),
        N 7 (N 7 (H 7) (N 7 (H 7) (H 7))) (H 7),
        N 7 (N 7 (N 7 (H 7) (H 7)) (H 7)) (H 7)]
   it "e9" $    
     take 7 nArboles' `shouldBe`
       [1,1,2,5,14,42,132]
   where
     arboles'  = f
     nArboles' = g

     
