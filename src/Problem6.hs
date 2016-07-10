module NinetyNine.Problem6 where

import Test.QuickCheck
import Test.QuickCheck.All

import Control.Applicative

-- solution 1
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

-- solution 2
isPalindrome2 :: Eq a => [a] -> Bool
isPalindrome2 []  = True
isPalindrome2 [_] = True
isPalindrome2 xs  = (head xs) == (last xs) && (isPalindrome2 $ init $ tail xs)

-- solution 3
isPalindrome3 :: (Eq a) => [a] -> Bool
isPalindrome3 = (==) Control.Applicative.<*> reverse

-- solution 4
-- isPalindrome4 :: (Eq a) => [a] -> Bool
-- isPalindrome4 = Control.Monad.liftM2 (==) id reverse

-- solution 5
palindrome5 :: (Eq a) => [a] -> Bool
palindrome5 xs = foldr (&&) True $ zipWith (==) xs (reverse xs)

-- solution 6
palindrome6 xs = and $ zipWith (==) xs (reverse xs)

-- solution 7
-- isPalindrome7 = (uncurry (==) . (id &&& reverse))

-- solution 8
isPalindrome8 list = f_part == reverse s_part
  where
    len = length list
    half_len = len `div` 2
    (f_part, s_part') = splitAt half_len list
    s_part = drop (len `mod` 2) s_part' -- ??

-- solution 9
isPalindrome9 list = take half_len list == reverse (drop (half_len + (len `mod` 2))
                                                    list)
  where
    len = length list
    half_len = len `div` 2

-- solution 10
isPalindrome10 :: (Eq a) => [a] -> Bool
isPalindrome10 xs = foldl (\acc (a,b) -> if a == b then acc else False) True input
  where
    input = zip xs (reverse xs)

-- Test
-- if something is palindrome -> its reverse is palindrome too
prop_isPalindromeReverse xs = isPalindrome xs == isPalindrome (reverse xs)

prop_isPalindrome xs = reverse xs == xs ==> isPalindrome xs == True

prop_isNotPalindrome xs = reverse xs /= xs ==> isPalindrome xs == False
