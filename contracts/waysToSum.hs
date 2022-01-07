
import Data.List(nub, sort)

addOneSomewhere :: [Int] -> [[Int]]
addOneSomewhere [n] = [[succ n]]
addOneSomewhere (x:xs) =
  let addOneToHead = succ x:xs
      addOneLater = map (x:) . addOneSomewhere $ xs
  in addOneToHead : addOneLater

waysToSum :: Int -> [[Int]]
waysToSum 1 = [[1]]
waysToSum n = nub $ do
  wayToSumPred <- waysToSum (pred n)
  let extendedByOne = 1 : wayToSumPred
  oneAddedSomewhere <- addOneSomewhere $ wayToSumPred
  map sort $ [extendedByOne, oneAddedSomewhere]

multiSums :: Int -> [[Int]]
multiSums n =
  filter ((>1) . length) .
  waysToSum $
  n

number :: Int
number = 77

main :: IO ()
main =
  print .
  length .
  multiSums $
  number