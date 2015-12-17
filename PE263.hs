{-
Problem 263

Consider the number 6. The divisors of 6 are: 1,2,3 and 6.

Every number from 1 up to and including 6 can be written as a sum of distinct divisors of 6:
1=1, 2=2, 3=1+2, 4=1+3, 5=2+3, 6=6.

A number n is called a practical number if every number from 1 up to and including n can be expressed as a sum of distinct divisors of n.

A pair of consecutive prime numbers with a difference of six is called a sexy pair (since "sex" is the Latin word for "six"). The first sexy pair is (23, 29).

We may occasionally find a triple-pair, which means three consecutive sexy prime pairs, such that the second member of each pair is the first member of the next pair.

We shall call a number n such that :

    (n-9, n-3), (n-3,n+3), (n+3, n+9) form a triple-pair, and
    the numbers n-8, n-4, n, n+4 and n+8 are all practical, 

an engineers’ paradise.

Find the sum of the first four engineers’ paradises.

-}


import Control.Applicative
import Data.List

doublePair :: Integer -> Bool
doublePair n = and [z | x <- divisorList, let y = (n `mod` x), let z = if  y /= 0  &&  y /= (-6) `mod` x && y /=(-12) `mod` x && noPrimesBetween then True else False]
   where noPrimesBetween = and $ fmap (divBy) $ filter odd ([(n+1)..(n+5)] ++ [(n+7)..(n+11)])
         divBy m = or $  fmap (((==) 0) . mod m) divisorList 
         divisorList = listOfPossibleDivisors n  12

sNC :: Integer -> Bool
sNC n = and [z | x <- divisorList, let y = (n `mod` x), let z = if  y /= 0  &&  y /= (-6) `mod` x && noPrimesBetween then True else False]
   where noPrimesBetween = and $ fmap (divBy)  $ filter odd [(n+1)..(n+5)]
         divBy m = or $  fmap (((==) 0) . mod m) divisorList 
         divisorList = listOfPossibleDivisors n 6

listOfPossibleDivisors :: Integer -> Double -> [Integer]
listOfPossibleDivisors n m = 2 : (fmap ((+1).(*2)) [1.. (truncate . (flip (/) 2). (subtract 1) . sqrt . (+ m) $ fromIntegral n)])

-- around 9x slower than listOfPossibleDivisors
c :: Integer -> [Integer]
c n = [x | x <- [3.. (truncate . sqrt . (+9) $ fromIntegral n)], odd x]

prime_factors n =
  case factors of
    [] -> [n]
    _  -> factors ++ prime_factors (n `div` (head factors))
  where factors  = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. truncate. sqrt $ fromIntegral n]

--alternate, possibly faster method
prime_factors' n = prime_factors'' n 2

prime_factors'' n m =
  case factors m of
    [] -> [n]
    _  -> factor : prime_factors'' (n `div` factor) factor
  where factors m = take 1 $ filter (\x -> (n `mod` x) == 0) [m .. truncate. sqrt $ fromIntegral n]
        factor = head $ factors m


factors n = frequency (prime_factors n)

--given two lists of equal length
-- first list is exponents of corresponding element of list 2
-- zipwith

exponentiate a b = zipWith (^) a b

--for exponentVary use <*> on elements of exponentList
-- [1..n] and [1..m] can be used


frequency :: Ord a => [a] -> [(a, Int)] 
frequency list = map (\l -> (head l, length l)) (group (sort list))

exponentList list = map length (group (sort list))

baseList list = map head (group (sort list))



engineerChecker n = undefined




















