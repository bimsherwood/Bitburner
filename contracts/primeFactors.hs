
hasFactor :: Int -> Int -> Bool
hasFactor n f = n `mod` f == 0

anyFactors :: Int -> [Int] -> Bool
anyFactors n xs = or $ map (hasFactor n) xs

primes :: [Int]
primes =
  let lessThanRoot n x = x <= (floor . sqrt . fromIntegral $ n)
      primeCandidates n = takeWhile (lessThanRoot n) primes
      anyPrimeFactors n = anyFactors n (primeCandidates n)
  in 2 : 3 : ( filter (not . anyPrimeFactors) [5..])
  
factorise :: Int -> [Int]
factorise 1 = []
factorise n =
  let lesserPrimes = takeWhile (<= n) primes
      nextPrimeFactor = head . filter (hasFactor n) $ lesserPrimes
      otherFactor = div n nextPrimeFactor
  in nextPrimeFactor : factorise otherFactor
  
number :: Int
number = 191605402

main :: IO ()
main = print . maximum . factorise $ number