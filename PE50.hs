{-
The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?

-}

import Benchmarker
import Control.Monad

divisors :: Integer -> [Integer]
divisors n = 2 : filter odd [3.. ceiling . sqrt $ fromIntegral n]

isP :: Integer -> Bool
isP n = if (n == 2) then True else 
  case factors of
    []  -> True
    _   -> False
  where factors  = (take 1) . filter (\x -> (n `mod` x) == 0) $ divisors n

listOfPrimes =  filter isP [2..]
primes n = take n $ listOfPrimes

findPrimeSum ps
    | isP sumps = Just sumps
    | otherwise = findPrimeSum (tail ps) `mplus` findPrimeSum (init ps)
        where sumps = sum ps

main = print $ findPrimeSum $ primes 546
