import Control.Applicative

triplePair :: Int -> Bool
triplePair n = and [z | x <- (b n), let y = (n `mod` x), let z = if  y /= 0  &&  y /= (-6) `mod` x && y /=(-12) `mod` x then True else False]  --equal to 6 equal to 12, make fmap stop computing when there is a false
--   where a = (flip mod) <$> (b n) <*> [n]

b :: Int -> [Int]
b n = (fmap ((+1).(*2)) [1.. (truncate . (flip (/) 2). (subtract 1) . sqrt . (+12) $ fromIntegral n)])


-- around 9x slower
c :: Int -> [Int]
c n = [x | x <- [3.. (truncate . sqrt . (+9) $ fromIntegral n)], odd x]


engineerChecker n   = undefined
