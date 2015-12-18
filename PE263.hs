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

sNC :: Integer -> Bool
sNC n = and [z | x <- divisorList, let y = (n `mod` x), let z = if  y /= 0  &&  y /= (-6) `mod` x && noPrimesBetween then True else False]
   where noPrimesBetween = and $ fmap (divBy)  $ filter odd [(n+1)..(n+5)]
         divBy m = or $  fmap (((==) 0) . mod m) divisorList 
         divisorList = listOfPossibleDivisors n

listOfPossibleDivisors :: Integer -> [Integer]
listOfPossibleDivisors n = 2 : (fmap ((+1).(*2)) [1.. (truncate . (flip (/) 2). (subtract 1) . sqrt . (+ 6) $ fromIntegral n)])

primeFactorize n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactorize (n `div` (head factors))
  where factors  = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. truncate. sqrt $ fromIntegral n]

--gets frequency of occurences of elements of a list
--[(element, number of times it appears)]
frequency :: Ord a => [a] -> [(a, Integer)] 
frequency list = map (\l -> (head l, toInteger $ length l)) (group (sort list))


primeFactors = frequency . primeFactorize

pNC = practicalChecker . reverse. primeFactors

practicalChecker :: [(Integer, Integer)] -> Bool
practicalChecker []     = True
practicalChecker (a:as) = and $ (fst a <= (1 + sumOfDivisors as))  : practicalChecker as : []

sumOfDivisors :: [(Integer, Integer)] -> Integer
sumOfDivisors []     = 1
sumOfDivisors (a:as) = (*) ((^) (fst a) $ (snd a) + 1 ) $ sumOfDivisors as

checkEP :: Integer -> Bool
checkEP n = and [sNC (n-9), pNC (n-8), sNC (n-3), pNC (n-4), pNC n, sNC (n+3), pNC (n+4), pNC (n+8)]

findSN n =
   case sNC (n+1) of
     True  -> (n+1)
     False -> findSN (n+1)

findEP n =
   case checkEP (n+1) of
     True  -> (n+1)
     False -> findEP (n+1)

main = putStrLn $ show $ take 5 $ iterate findEP 1



--everything below this line I did not use



doublePair :: Integer -> Bool
doublePair n = and [z | x <- divisorList, let y = (n `mod` x), let z = if  y /= 0  &&  y /= (-6) `mod` x && y /=(-12) `mod` x && noPrimesBetween then True else False]
   where noPrimesBetween = and $ fmap (divBy) $ filter odd ([(n+1)..(n+5)] ++ [(n+7)..(n+11)])
         divBy m = or $  fmap (((==) 0) . mod m) divisorList 
         divisorList = listOfPossibleDivisors n


-- around 9x slower than listOfPossibleDivisors
c :: Integer -> [Integer]
c n = [x | x <- [3.. (truncate . sqrt . (+9) $ fromIntegral n)], odd x]


--alternate, method of prime factorization
--I benchmarked this and it's 30% slower on average
--there are some cases when it's faster (like when n = prime ^ very high number), but they are negligable
prime_factors' n = prime_factors'' n 2

prime_factors'' n m =
  case factors m of
    [] -> [n]
    _  -> factor : prime_factors'' (n `div` factor) factor
  where factors m = take 1 $ filter (\x -> (n `mod` x) == 0) [m .. truncate. sqrt $ fromIntegral n]
        factor = head $ factors m


-- lists out factors of a number
factors :: Integer -> [Integer]
factors n = sort$ exponentiate $ exponentVary (fmap fst primeFE) (fmap snd primeFE)
   where primeFE = primeFactors n


--first argument is bases, second argument is exponents
-- outputs list of list of possible combinations of base-exponenet combinations 
-- for example, exponentVary [2,3] [3,3] = [[1,2,4,8], [1,3,9,27]]
exponentVary :: [Integer] -> [Integer] -> [[Integer]]
exponentVary []   _    = []
exponentVary _    []   = []
exponentVary (b:bs) (e:es) = ((^) <$> [b] <*> [0..e]) : (exponentVary bs es)

exponentiate :: [[Integer]] -> [Integer]
exponentiate [] = [1]
exponentiate (a:as) = (*) <$> a <*> exponentiate as
