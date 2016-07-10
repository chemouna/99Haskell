{-# LANGUAGE TemplateHaskell #-}

bmodule NinetyNine.Problem21 where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All

-- Insert an element at a given position into a list.
-- insertAt 'X' "abcd" 2 "aXbcd"

insertAt :: a -> [a] -> Int -> [a]
insertAt v xs n = fst l ++ [v] ++ snd l
                  where l = splitAt (n-1) xs


-- splitAt 2 "abcd" -> ("ab", "cd")
--  intercalate "x" ["ab", "cd"] -> "abxcd"
-- convert a tuple to a list, how ? (the contrary of uncons)

-- use intersperse

insertAtSpec :: Spec
insertAtSpec = do
  describe "Insert an element at a given position into a list." $ do
     it "Insert an element at a given position into a list." $ do
      insertAt 'X' "abcd" 2  `shouldBe` "aXbcd"

-- the element to insert should end up in the desired position
prop_shouldBeAtItsPosition v (NonEmpty xs) (Positive n) = (insertAt v xs n) !! (n-1) == v

return []
main = $quickCheckAll

