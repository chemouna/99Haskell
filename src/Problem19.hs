{-# LANGUAGE TemplateHaskell #-}

module NinetyNine.Problem19 where 

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All

-- Rotate a list N places to the left.
-- use length and ++

rotate :: [a] -> Int -> [a]
rotate xs n = drop n xs ++ take n xs

rotate2 :: [a] -> Int -> [a]
rotate2 xs n = [x | (x,i) <- zip xs [1..length xs], i > n] ++ take n xs 

rotate3 :: [a] -> Int -> [a]
rotate3 xs n = snd ys ++ fst ys
  where ys = splitAt n xs

rotate4 :: [a] -> Int -> [a]
rotate4 [] _ = []
rotate4 xs 0 = xs
rotate4 (x:xs) n = rotate4 (xs ++ [x]) (n-1)

rotateSpec :: Spec
rotateSpec = do
  describe "Rotate a list N places to the left." $ do
     it "Rotate a list N places to the left." $ do
       rotate ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"
     it "[With rotate2] Rotate a list N places to the left." $ do
       rotate2 ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"
     it "[With rotate3] Rotate a list N places to the left." $ do
       rotate3 ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"
     it "[With rotate4] Rotate a list N places to the left." $ do
       rotate4 ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"

-- the nth element becomes the last
-- in quickcheck how to handle properties that aren't applicable to
-- empty lists 
prop_nthEltBecomesLast (NonEmpty xs) n = last (rotate xs n) == xs !! n

return []
main = $quickCheckAll



