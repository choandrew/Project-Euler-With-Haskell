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

triplePair :: Integer -> Bool
triplePair n = and [z | x <- divisorList, let y = (n `mod` x), let z = if  y /= 0  &&  y /= (-6) `mod` x && y /=(-12) `mod` x && noPrimesBetween then True else False]
   where noPrimesBetween = and $ fmap (divBy) $ filter odd ([(n+1)..(n+5)] ++ [(n+7)..(n+11)])
         divBy m = or $  fmap (((==) 0) . mod m) divisorList 
         divisorList = listOfPossibleDivisors n  

sNC :: Integer -> Bool
sNC n = and [z | x <- divisorList, let y = (n `mod` x), let z = if  y /= 0  &&  y /= (-6) `mod` x && noPrimesBetween then True else False]
   where noPrimesBetween = and $ fmap (divBy)  $ filter odd [(n+1)..(n+5)]
         divBy m = or $  fmap (((==) 0) . mod m) divisorList 
         divisorList = listOfPossibleDivisors n  

divBy m l= or $  fmap (((==) 0) . mod m) l



listOfPossibleDivisors :: Integer -> [Integer]
listOfPossibleDivisors n = 2 : (fmap ((+1).(*2)) [1.. (truncate . (flip (/) 2). (subtract 1) . sqrt . (+12) $ fromIntegral n)])


-- around 9x slower than listOfPossibleDivisors
c :: Integer -> [Integer]
c n = [x | x <- [3.. (truncate . sqrt . (+9) $ fromIntegral n)], odd x]


engineerChecker n   = undefined
