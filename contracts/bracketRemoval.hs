import Data.List (sort, nub)

balancedBrackets' :: Int -> String -> Bool
balancedBrackets' level _
  | level < 0              = False
balancedBrackets' level "" = level == 0
balancedBrackets' level ('(':xs) = balancedBrackets' (succ level) xs
balancedBrackets' level (')':xs) = balancedBrackets' (pred level) xs
balancedBrackets' level (_:xs) = balancedBrackets' level xs

balancedBrackets :: String -> Bool
balancedBrackets = balancedBrackets' 0

bracketRemovals :: String -> [String]
bracketRemovals "" = [""]
bracketRemovals (x:xs)
  | elem x "()" = do
    subRemoval <- bracketRemovals xs
    [x:subRemoval, subRemoval]
  | otherwise   = map (x:) $ bracketRemovals xs

addLengths :: [String] -> [(Int, String)]
addLengths = map (\a -> (length a, a))

solve :: String -> String
solve str =
  let byLengthDesc =
        reverse .
        sort .
        addLengths .
        filter balancedBrackets .
        nub .
        bracketRemovals $
        str
      maxLength = fst $ head byLengthDesc
  in filter (/= '"') . show . map snd . filter ((==)maxLength . fst) $ byLengthDesc

string :: String
string = "(((())a())))"

main :: IO ()
main = print $ solve string