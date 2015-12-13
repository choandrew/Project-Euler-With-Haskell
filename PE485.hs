import Control.Applicative

--sexyNumberChecker :: Int -> Bool
sexyNumberChecker n = and (fmap (/= 0 ) a) --equal to 6 equal to 12, make fmap stop computing when there is a false
   where a = (flip mod) <$> (fmap ((+1).(*2)) [1.. (truncate . (flip (/) 2). (subtract 1) . sqrt . (+9) $ fromIntegral n)]) <*> [n]

b :: Int -> [Int]
b n = (fmap ((+1).(*2)) [1.. (truncate . (flip (/) 2). (subtract 1) . sqrt . (+9) $ fromIntegral n)])

c :: Int -> [Int]
c n = [x | x <- [3.. (truncate . sqrt . (+9) $ fromIntegral n)], odd x]


main = do 
  x <- b 99999999
-- x <- c 99999999
  putStrLn "hi"


engineerChecker n   = undefined
