
module NinetyNine.Problem22 where


import Test.Hspec

range :: Int -> Int -> [Int]
range x y = [x..y]

range2 :: Int -> Int -> [Int]
range2 x y = take (y-x + 1) $ iterate (+1) x

range3 :: Int -> Int -> [Int]
range3 = enumFromTo

range4 :: Int -> Int -> [Int]
range4 x y
  | x > y = []
  | x == y = [x]
  | otherwise = [z | i <- [(x-1)..(y-1)], let z = i + 1]

rangeAtSpec :: Spec
rangeAtSpec = do
  describe "Create a list containing all integers within a given range." $ do 
    it "Create a range between two integers." $ do
        range 4 9 `shouldBe` [4,5,6,7,8,9] 
    it "[range2] Create a range between two integers." $ do
        range2 4 9 `shouldBe` [4,5,6,7,8,9] 
    it "[range3] Create a range between two integers." $ do
        range3 4 9 `shouldBe` [4,5,6,7,8,9] 
    it "[range4] Create a range between two integers." $ do
        range4 4 9 `shouldBe` [4,5,6,7,8,9] 



