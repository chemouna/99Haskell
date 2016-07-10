{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module NinetyNine.Problem20 where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import Data.List

-- Remove the K'th element from a list.

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt 0 xs = (Nothing, xs)
removeAt n xs | n > length xs = (Nothing, xs)
              | otherwise = (Just (xs !! (n - 1)), init (fst ys) ++ (snd ys))
                               where ys = splitAt n xs

removeAt2 :: Int -> [a] -> (a, [a])
removeAt2 n xs = (last (take n xs), init (take n xs) ++ drop n xs)

removeAt3 :: Eq a => Int -> [a] -> (a, [a])
removeAt3 n xs = (v, delete v xs)
                 where v = xs !! (n - 1)
                 -- n >= 1

removeAt4 :: Int -> [a] -> (a, [a])
removeAt4 k xs = case back of
                   [] -> error "removeAt: index too large"
                   x:rest -> (x, front ++ rest)
                 where (front, back) = splitAt (k - 1) xs

removeAt5 :: Eq a => Int -> [a] -> (a, [a])
removeAt5 n xs = (v, xs \\ [v])
                 where v = xs !! (n-1)

removeAt6 :: Eq a => Int -> [a] -> (a, [a]) -- without error checking
removeAt6 n xs = let (front, back) = splitAt n xs in (last front, init front ++ back)

removeAt7 :: Int -> [a] -> (a, [a]) -- point free style + without error checking
removeAt7 n = (\(a,b) -> (head b, a ++ tail b)) . splitAt (n-1) 

removeAt8 :: Int -> [a] -> (a, [a]) -- a recursive solution
removeAt8 1 (x:xs) = (x, xs)
removeAt8 n (x:xs) = (l, x:r)
                     where (l, r) = removeAt8 (n-1) xs

removeAtSpec :: Spec
removeAtSpec = do
  describe "Remove the K'th element from a list.." $ do
     it "Remove the K'th element from a list.." $ do
      removeAt 2 "abcd" `shouldBe` (Just 'b',"acd")
     it "[removeAt2] Remove the K'th element from a list.." $ do
      removeAt2 2 "abcd" `shouldBe` ('b',"acd")
     it "[removeAt3] Remove the K'th element from a list.." $ do
      removeAt3 2 "abcd" `shouldBe` ('b',"acd")
     it "[removeAt4] Remove the K'th element from a list.." $ do
      removeAt4 2 "abcd" `shouldBe` ('b',"acd")
     it "[removeAt5] Remove the K'th element from a list.." $ do
      removeAt5 2 "abcd" `shouldBe` ('b',"acd")
     it "[removeAt6] Remove the K'th element from a list.." $ do
      removeAt6 2 "abcd" `shouldBe` ('b',"acd")
     it "[removeAt7] Remove the K'th element from a list.." $ do
      removeAt7 2 "abcd" `shouldBe` ('b',"acd")
     it "[removeAt8] Remove the K'th element from a list.." $ do
      removeAt8 2 "abcd" `shouldBe` ('b',"acd")


-- prop_lengthFirstElement n xs = fst (removeAt n xs) == xs !! (n - 1)

prop_lengthSecondElement :: Int -> [a] -> Bool
prop_lengthSecondElement n xs = length (snd (removeAt n xs)) == length xs - 2

return []
main = $quickCheckAll


