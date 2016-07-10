{-# LANGUAGE TemplateHaskell #-}

module NinetyNine.Problem10 where

-- import NinetyNine.P9
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import Control.Arrow

import NinetyNine.Problem9

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack
encode2 = map (\x -> (length x, head x)) . group
encode3 xs = [(length x, head x) | x <- group xs]
encode4 xs = map (length &&& head) $ group xs

encode5 :: (Eq a) => [a] -> [(Int, a)]
encode5 = map ((,) <$> length <*> head) . pack

encode6 = (enc . pack)
    where enc = (foldr (\x acc -> (length x, head x) : acc) [])

encode7 xs = zip (map length l) h
    where
        l = group xs
        h = map head l

-- Test
-- hspec
encodeSpec :: Spec
encodeSpec = do
  describe "encode" $ do
    it "Run-length encoding of a list." $ do
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

    it "[With encode2] Run-length encoding of a list." $ do
        encode2 "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

    it "[With encode3] Run-length encoding of a list." $ do
        encode3 "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

    it "[With encode4] Run-length encoding of a list." $ do
        encode4 "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

    it "[With encode5] Run-length encoding of a list." $ do
        encode5 "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

    it "[With encode6] Run-length encoding of a list." $ do
        encode6 "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

    it "[With encode7] Run-length encoding of a list." $ do
        encode7 "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

-- quickcheck
-- sum of numbers on first elts in encoded list = length of list
prop_encodeLength xs = length xs == sum (map (\(x, _) -> x) $ encode xs)

-- tail element of tuple of first element of encoded list should be the same as first element of the list
-- same thing for the last
prop_encodeFirstElement (NonEmpty xs) = head xs == snd (head (encode xs))
prop_encodeLastElement (NonEmpty xs) = last xs == snd (last (encode xs))


return []
main = $quickCheckAll
