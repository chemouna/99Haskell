module NinetyNine.Problem2 where

myButLast :: [a] -> a
myButLast [] = error "Can't get the before last element of an empty list"
myButLast [x] = error "Can't get the before last element of a one element list"
myButLast (x:xs) =
  if length xs == 1 then x
  else myButLast xs

myButLast2 x = reverse x !! 1

myButLast3 = last . init

myButLast4 = head . tail . reverse
