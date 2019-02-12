module Streams where

ones = 1:ones

nats = [0..]

nats1 = 0 : zipWith (+) ones nats1

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

sieve (x:xs) = x : sieve tail
  where tail = filter (\n -> n `mod` x > 0) xs
 
primes = sieve [2..]

facts = 1 : zipWith (*) facts [1..]

generateGlm :: Int -> Int -> [Int] -> [Int]
generateGlm current 0 (occurancesOfNext : tail) = generateGlm next occurancesOfNext tail
   where next = current + 1
generateGlm current occurancesLeft glm = current : generateGlm current (occurancesLeft - 1) glm  

golomb :: [Int]
golomb = 1 : 2 : generateGlm 2 1 (drop 2 golomb)
