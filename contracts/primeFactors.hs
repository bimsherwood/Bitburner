
hasFactor :: Int -> Int -> Bool
hasFactor n f = n `mod` f == 0

anyFactors :: Int -> [Int] -> Bool
anyFactors n xs = or $ map (hasFactor n) xs

lessThanRoot :: Int -> Int -> Bool
lessThanRoot n x = x <= (floor . sqrt . fromIntegral $ n)

primeFactorCandidates :: Int -> [Int]
primeFactorCandidates n = takeWhile (lessThanRoot n) primes
      
primes :: [Int]
primes =
  let anyPrimeFactors n = anyFactors n (primeFactorCandidates n)
  in 2 : 3 : ( filter (not . anyPrimeFactors) [5..])
  
factorise :: Int -> [Int]
factorise 1 = []
factorise n =
  let primeFactors = filter (hasFactor n) . primeFactorCandidates $ n
      firstPrimeFactor = head primeFactors
      otherFactor = div n firstPrimeFactor
  in if null primeFactors
    then [n]
    else firstPrimeFactor : factorise otherFactor
  
number :: Int
number = 696474140

main :: IO ()
main = print . maximum . factorise $ number