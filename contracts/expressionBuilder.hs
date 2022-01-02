
import Data.List(elemIndex, intersperse)
import Data.Maybe(fromJust)

data Product = Product [Int]
data Sum = Sum [Product]
data Operator = Plus | Minus | Times
  deriving (Eq)
data Symbol = Digit Int | Operator Operator
  
instance Show (Product) where
  show (Product xs) = concat . intersperse "*" . map show $ xs
  
instance Show (Sum) where
  show (Sum xs) = concat . intersperse "+" . map show $ xs

instance Show (Symbol) where
  show (Digit n) = show n
  show (Operator Plus) = "+"
  show (Operator Minus) = "-"
  show (Operator Times) = "*"

digitsToExpression :: String -> [Int]
digitsToExpression [] = []
digitsToExpression (x:xs) =
  let leadingDigit = fromJust . (`elemIndex` "0123456789")
  in leadingDigit x : digitsToExpression xs

possibleExpressions :: [Int] -> [[Symbol]]
possibleExpressions [] = []
possibleExpressions [x] = [[Digit x]]
possibleExpressions (x:xs) = do
  operator <- [[], [Operator Plus], [Operator Minus], [Operator Times]]
  possibleRight <- possibleExpressions xs
  return $ [Digit x] ++ operator ++ possibleRight

printExpressions =
  putStrLn .
  unlines .
  map ("  "++) .
  map (++",") .
  map (concat . map show) .
  possibleExpressions .
  map read .
  map return $
  string

countValues =
  length .
  filter (\x -> x==target || x==(-target)) $
  values

target = -20
string = "596195"

values = ([
----------------------------------
  596195,
  59619+5,
  59619-5,
  59619*5,
  5961+95,
  5961+9+5,
  5961+9-5,
  5961+9*5,
  5961-95,
  5961-9+5,
  5961-9-5,
  5961-9*5,
  5961*95,
  5961*9+5,
  5961*9-5,
  5961*9*5,
  596+195,
  596+19+5,
  596+19-5,
  596+19*5,
  596+1+95,
  596+1+9+5,
  596+1+9-5,
  596+1+9*5,
  596+1-95,
  596+1-9+5,
  596+1-9-5,
  596+1-9*5,
  596+1*95,
  596+1*9+5,
  596+1*9-5,
  596+1*9*5,
  596-195,
  596-19+5,
  596-19-5,
  596-19*5,
  596-1+95,
  596-1+9+5,
  596-1+9-5,
  596-1+9*5,
  596-1-95,
  596-1-9+5,
  596-1-9-5,
  596-1-9*5,
  596-1*95,
  596-1*9+5,
  596-1*9-5,
  596-1*9*5,
  596*195,
  596*19+5,
  596*19-5,
  596*19*5,
  596*1+95,
  596*1+9+5,
  596*1+9-5,
  596*1+9*5,
  596*1-95,
  596*1-9+5,
  596*1-9-5,
  596*1-9*5,
  596*1*95,
  596*1*9+5,
  596*1*9-5,
  596*1*9*5,
  59+6195,
  59+619+5,
  59+619-5,
  59+619*5,
  59+61+95,
  59+61+9+5,
  59+61+9-5,
  59+61+9*5,
  59+61-95,
  59+61-9+5,
  59+61-9-5,
  59+61-9*5,
  59+61*95,
  59+61*9+5,
  59+61*9-5,
  59+61*9*5,
  59+6+195,
  59+6+19+5,
  59+6+19-5,
  59+6+19*5,
  59+6+1+95,
  59+6+1+9+5,
  59+6+1+9-5,
  59+6+1+9*5,
  59+6+1-95,
  59+6+1-9+5,
  59+6+1-9-5,
  59+6+1-9*5,
  59+6+1*95,
  59+6+1*9+5,
  59+6+1*9-5,
  59+6+1*9*5,
  59+6-195,
  59+6-19+5,
  59+6-19-5,
  59+6-19*5,
  59+6-1+95,
  59+6-1+9+5,
  59+6-1+9-5,
  59+6-1+9*5,
  59+6-1-95,
  59+6-1-9+5,
  59+6-1-9-5,
  59+6-1-9*5,
  59+6-1*95,
  59+6-1*9+5,
  59+6-1*9-5,
  59+6-1*9*5,
  59+6*195,
  59+6*19+5,
  59+6*19-5,
  59+6*19*5,
  59+6*1+95,
  59+6*1+9+5,
  59+6*1+9-5,
  59+6*1+9*5,
  59+6*1-95,
  59+6*1-9+5,
  59+6*1-9-5,
  59+6*1-9*5,
  59+6*1*95,
  59+6*1*9+5,
  59+6*1*9-5,
  59+6*1*9*5,
  59-6195,
  59-619+5,
  59-619-5,
  59-619*5,
  59-61+95,
  59-61+9+5,
  59-61+9-5,
  59-61+9*5,
  59-61-95,
  59-61-9+5,
  59-61-9-5,
  59-61-9*5,
  59-61*95,
  59-61*9+5,
  59-61*9-5,
  59-61*9*5,
  59-6+195,
  59-6+19+5,
  59-6+19-5,
  59-6+19*5,
  59-6+1+95,
  59-6+1+9+5,
  59-6+1+9-5,
  59-6+1+9*5,
  59-6+1-95,
  59-6+1-9+5,
  59-6+1-9-5,
  59-6+1-9*5,
  59-6+1*95,
  59-6+1*9+5,
  59-6+1*9-5,
  59-6+1*9*5,
  59-6-195,
  59-6-19+5,
  59-6-19-5,
  59-6-19*5,
  59-6-1+95,
  59-6-1+9+5,
  59-6-1+9-5,
  59-6-1+9*5,
  59-6-1-95,
  59-6-1-9+5,
  59-6-1-9-5,
  59-6-1-9*5,
  59-6-1*95,
  59-6-1*9+5,
  59-6-1*9-5,
  59-6-1*9*5,
  59-6*195,
  59-6*19+5,
  59-6*19-5,
  59-6*19*5,
  59-6*1+95,
  59-6*1+9+5,
  59-6*1+9-5,
  59-6*1+9*5,
  59-6*1-95,
  59-6*1-9+5,
  59-6*1-9-5,
  59-6*1-9*5,
  59-6*1*95,
  59-6*1*9+5,
  59-6*1*9-5,
  59-6*1*9*5,
  59*6195,
  59*619+5,
  59*619-5,
  59*619*5,
  59*61+95,
  59*61+9+5,
  59*61+9-5,
  59*61+9*5,
  59*61-95,
  59*61-9+5,
  59*61-9-5,
  59*61-9*5,
  59*61*95,
  59*61*9+5,
  59*61*9-5,
  59*61*9*5,
  59*6+195,
  59*6+19+5,
  59*6+19-5,
  59*6+19*5,
  59*6+1+95,
  59*6+1+9+5,
  59*6+1+9-5,
  59*6+1+9*5,
  59*6+1-95,
  59*6+1-9+5,
  59*6+1-9-5,
  59*6+1-9*5,
  59*6+1*95,
  59*6+1*9+5,
  59*6+1*9-5,
  59*6+1*9*5,
  59*6-195,
  59*6-19+5,
  59*6-19-5,
  59*6-19*5,
  59*6-1+95,
  59*6-1+9+5,
  59*6-1+9-5,
  59*6-1+9*5,
  59*6-1-95,
  59*6-1-9+5,
  59*6-1-9-5,
  59*6-1-9*5,
  59*6-1*95,
  59*6-1*9+5,
  59*6-1*9-5,
  59*6-1*9*5,
  59*6*195,
  59*6*19+5,
  59*6*19-5,
  59*6*19*5,
  59*6*1+95,
  59*6*1+9+5,
  59*6*1+9-5,
  59*6*1+9*5,
  59*6*1-95,
  59*6*1-9+5,
  59*6*1-9-5,
  59*6*1-9*5,
  59*6*1*95,
  59*6*1*9+5,
  59*6*1*9-5,
  59*6*1*9*5,
  5+96195,
  5+9619+5,
  5+9619-5,
  5+9619*5,
  5+961+95,
  5+961+9+5,
  5+961+9-5,
  5+961+9*5,
  5+961-95,
  5+961-9+5,
  5+961-9-5,
  5+961-9*5,
  5+961*95,
  5+961*9+5,
  5+961*9-5,
  5+961*9*5,
  5+96+195,
  5+96+19+5,
  5+96+19-5,
  5+96+19*5,
  5+96+1+95,
  5+96+1+9+5,
  5+96+1+9-5,
  5+96+1+9*5,
  5+96+1-95,
  5+96+1-9+5,
  5+96+1-9-5,
  5+96+1-9*5,
  5+96+1*95,
  5+96+1*9+5,
  5+96+1*9-5,
  5+96+1*9*5,
  5+96-195,
  5+96-19+5,
  5+96-19-5,
  5+96-19*5,
  5+96-1+95,
  5+96-1+9+5,
  5+96-1+9-5,
  5+96-1+9*5,
  5+96-1-95,
  5+96-1-9+5,
  5+96-1-9-5,
  5+96-1-9*5,
  5+96-1*95,
  5+96-1*9+5,
  5+96-1*9-5,
  5+96-1*9*5,
  5+96*195,
  5+96*19+5,
  5+96*19-5,
  5+96*19*5,
  5+96*1+95,
  5+96*1+9+5,
  5+96*1+9-5,
  5+96*1+9*5,
  5+96*1-95,
  5+96*1-9+5,
  5+96*1-9-5,
  5+96*1-9*5,
  5+96*1*95,
  5+96*1*9+5,
  5+96*1*9-5,
  5+96*1*9*5,
  5+9+6195,
  5+9+619+5,
  5+9+619-5,
  5+9+619*5,
  5+9+61+95,
  5+9+61+9+5,
  5+9+61+9-5,
  5+9+61+9*5,
  5+9+61-95,
  5+9+61-9+5,
  5+9+61-9-5,
  5+9+61-9*5,
  5+9+61*95,
  5+9+61*9+5,
  5+9+61*9-5,
  5+9+61*9*5,
  5+9+6+195,
  5+9+6+19+5,
  5+9+6+19-5,
  5+9+6+19*5,
  5+9+6+1+95,
  5+9+6+1+9+5,
  5+9+6+1+9-5,
  5+9+6+1+9*5,
  5+9+6+1-95,
  5+9+6+1-9+5,
  5+9+6+1-9-5,
  5+9+6+1-9*5,
  5+9+6+1*95,
  5+9+6+1*9+5,
  5+9+6+1*9-5,
  5+9+6+1*9*5,
  5+9+6-195,
  5+9+6-19+5,
  5+9+6-19-5,
  5+9+6-19*5,
  5+9+6-1+95,
  5+9+6-1+9+5,
  5+9+6-1+9-5,
  5+9+6-1+9*5,
  5+9+6-1-95,
  5+9+6-1-9+5,
  5+9+6-1-9-5,
  5+9+6-1-9*5,
  5+9+6-1*95,
  5+9+6-1*9+5,
  5+9+6-1*9-5,
  5+9+6-1*9*5,
  5+9+6*195,
  5+9+6*19+5,
  5+9+6*19-5,
  5+9+6*19*5,
  5+9+6*1+95,
  5+9+6*1+9+5,
  5+9+6*1+9-5,
  5+9+6*1+9*5,
  5+9+6*1-95,
  5+9+6*1-9+5,
  5+9+6*1-9-5,
  5+9+6*1-9*5,
  5+9+6*1*95,
  5+9+6*1*9+5,
  5+9+6*1*9-5,
  5+9+6*1*9*5,
  5+9-6195,
  5+9-619+5,
  5+9-619-5,
  5+9-619*5,
  5+9-61+95,
  5+9-61+9+5,
  5+9-61+9-5,
  5+9-61+9*5,
  5+9-61-95,
  5+9-61-9+5,
  5+9-61-9-5,
  5+9-61-9*5,
  5+9-61*95,
  5+9-61*9+5,
  5+9-61*9-5,
  5+9-61*9*5,
  5+9-6+195,
  5+9-6+19+5,
  5+9-6+19-5,
  5+9-6+19*5,
  5+9-6+1+95,
  5+9-6+1+9+5,
  5+9-6+1+9-5,
  5+9-6+1+9*5,
  5+9-6+1-95,
  5+9-6+1-9+5,
  5+9-6+1-9-5,
  5+9-6+1-9*5,
  5+9-6+1*95,
  5+9-6+1*9+5,
  5+9-6+1*9-5,
  5+9-6+1*9*5,
  5+9-6-195,
  5+9-6-19+5,
  5+9-6-19-5,
  5+9-6-19*5,
  5+9-6-1+95,
  5+9-6-1+9+5,
  5+9-6-1+9-5,
  5+9-6-1+9*5,
  5+9-6-1-95,
  5+9-6-1-9+5,
  5+9-6-1-9-5,
  5+9-6-1-9*5,
  5+9-6-1*95,
  5+9-6-1*9+5,
  5+9-6-1*9-5,
  5+9-6-1*9*5,
  5+9-6*195,
  5+9-6*19+5,
  5+9-6*19-5,
  5+9-6*19*5,
  5+9-6*1+95,
  5+9-6*1+9+5,
  5+9-6*1+9-5,
  5+9-6*1+9*5,
  5+9-6*1-95,
  5+9-6*1-9+5,
  5+9-6*1-9-5,
  5+9-6*1-9*5,
  5+9-6*1*95,
  5+9-6*1*9+5,
  5+9-6*1*9-5,
  5+9-6*1*9*5,
  5+9*6195,
  5+9*619+5,
  5+9*619-5,
  5+9*619*5,
  5+9*61+95,
  5+9*61+9+5,
  5+9*61+9-5,
  5+9*61+9*5,
  5+9*61-95,
  5+9*61-9+5,
  5+9*61-9-5,
  5+9*61-9*5,
  5+9*61*95,
  5+9*61*9+5,
  5+9*61*9-5,
  5+9*61*9*5,
  5+9*6+195,
  5+9*6+19+5,
  5+9*6+19-5,
  5+9*6+19*5,
  5+9*6+1+95,
  5+9*6+1+9+5,
  5+9*6+1+9-5,
  5+9*6+1+9*5,
  5+9*6+1-95,
  5+9*6+1-9+5,
  5+9*6+1-9-5,
  5+9*6+1-9*5,
  5+9*6+1*95,
  5+9*6+1*9+5,
  5+9*6+1*9-5,
  5+9*6+1*9*5,
  5+9*6-195,
  5+9*6-19+5,
  5+9*6-19-5,
  5+9*6-19*5,
  5+9*6-1+95,
  5+9*6-1+9+5,
  5+9*6-1+9-5,
  5+9*6-1+9*5,
  5+9*6-1-95,
  5+9*6-1-9+5,
  5+9*6-1-9-5,
  5+9*6-1-9*5,
  5+9*6-1*95,
  5+9*6-1*9+5,
  5+9*6-1*9-5,
  5+9*6-1*9*5,
  5+9*6*195,
  5+9*6*19+5,
  5+9*6*19-5,
  5+9*6*19*5,
  5+9*6*1+95,
  5+9*6*1+9+5,
  5+9*6*1+9-5,
  5+9*6*1+9*5,
  5+9*6*1-95,
  5+9*6*1-9+5,
  5+9*6*1-9-5,
  5+9*6*1-9*5,
  5+9*6*1*95,
  5+9*6*1*9+5,
  5+9*6*1*9-5,
  5+9*6*1*9*5,
  5-96195,
  5-9619+5,
  5-9619-5,
  5-9619*5,
  5-961+95,
  5-961+9+5,
  5-961+9-5,
  5-961+9*5,
  5-961-95,
  5-961-9+5,
  5-961-9-5,
  5-961-9*5,
  5-961*95,
  5-961*9+5,
  5-961*9-5,
  5-961*9*5,
  5-96+195,
  5-96+19+5,
  5-96+19-5,
  5-96+19*5,
  5-96+1+95,
  5-96+1+9+5,
  5-96+1+9-5,
  5-96+1+9*5,
  5-96+1-95,
  5-96+1-9+5,
  5-96+1-9-5,
  5-96+1-9*5,
  5-96+1*95,
  5-96+1*9+5,
  5-96+1*9-5,
  5-96+1*9*5,
  5-96-195,
  5-96-19+5,
  5-96-19-5,
  5-96-19*5,
  5-96-1+95,
  5-96-1+9+5,
  5-96-1+9-5,
  5-96-1+9*5,
  5-96-1-95,
  5-96-1-9+5,
  5-96-1-9-5,
  5-96-1-9*5,
  5-96-1*95,
  5-96-1*9+5,
  5-96-1*9-5,
  5-96-1*9*5,
  5-96*195,
  5-96*19+5,
  5-96*19-5,
  5-96*19*5,
  5-96*1+95,
  5-96*1+9+5,
  5-96*1+9-5,
  5-96*1+9*5,
  5-96*1-95,
  5-96*1-9+5,
  5-96*1-9-5,
  5-96*1-9*5,
  5-96*1*95,
  5-96*1*9+5,
  5-96*1*9-5,
  5-96*1*9*5,
  5-9+6195,
  5-9+619+5,
  5-9+619-5,
  5-9+619*5,
  5-9+61+95,
  5-9+61+9+5,
  5-9+61+9-5,
  5-9+61+9*5,
  5-9+61-95,
  5-9+61-9+5,
  5-9+61-9-5,
  5-9+61-9*5,
  5-9+61*95,
  5-9+61*9+5,
  5-9+61*9-5,
  5-9+61*9*5,
  5-9+6+195,
  5-9+6+19+5,
  5-9+6+19-5,
  5-9+6+19*5,
  5-9+6+1+95,
  5-9+6+1+9+5,
  5-9+6+1+9-5,
  5-9+6+1+9*5,
  5-9+6+1-95,
  5-9+6+1-9+5,
  5-9+6+1-9-5,
  5-9+6+1-9*5,
  5-9+6+1*95,
  5-9+6+1*9+5,
  5-9+6+1*9-5,
  5-9+6+1*9*5,
  5-9+6-195,
  5-9+6-19+5,
  5-9+6-19-5,
  5-9+6-19*5,
  5-9+6-1+95,
  5-9+6-1+9+5,
  5-9+6-1+9-5,
  5-9+6-1+9*5,
  5-9+6-1-95,
  5-9+6-1-9+5,
  5-9+6-1-9-5,
  5-9+6-1-9*5,
  5-9+6-1*95,
  5-9+6-1*9+5,
  5-9+6-1*9-5,
  5-9+6-1*9*5,
  5-9+6*195,
  5-9+6*19+5,
  5-9+6*19-5,
  5-9+6*19*5,
  5-9+6*1+95,
  5-9+6*1+9+5,
  5-9+6*1+9-5,
  5-9+6*1+9*5,
  5-9+6*1-95,
  5-9+6*1-9+5,
  5-9+6*1-9-5,
  5-9+6*1-9*5,
  5-9+6*1*95,
  5-9+6*1*9+5,
  5-9+6*1*9-5,
  5-9+6*1*9*5,
  5-9-6195,
  5-9-619+5,
  5-9-619-5,
  5-9-619*5,
  5-9-61+95,
  5-9-61+9+5,
  5-9-61+9-5,
  5-9-61+9*5,
  5-9-61-95,
  5-9-61-9+5,
  5-9-61-9-5,
  5-9-61-9*5,
  5-9-61*95,
  5-9-61*9+5,
  5-9-61*9-5,
  5-9-61*9*5,
  5-9-6+195,
  5-9-6+19+5,
  5-9-6+19-5,
  5-9-6+19*5,
  5-9-6+1+95,
  5-9-6+1+9+5,
  5-9-6+1+9-5,
  5-9-6+1+9*5,
  5-9-6+1-95,
  5-9-6+1-9+5,
  5-9-6+1-9-5,
  5-9-6+1-9*5,
  5-9-6+1*95,
  5-9-6+1*9+5,
  5-9-6+1*9-5,
  5-9-6+1*9*5,
  5-9-6-195,
  5-9-6-19+5,
  5-9-6-19-5,
  5-9-6-19*5,
  5-9-6-1+95,
  5-9-6-1+9+5,
  5-9-6-1+9-5,
  5-9-6-1+9*5,
  5-9-6-1-95,
  5-9-6-1-9+5,
  5-9-6-1-9-5,
  5-9-6-1-9*5,
  5-9-6-1*95,
  5-9-6-1*9+5,
  5-9-6-1*9-5,
  5-9-6-1*9*5,
  5-9-6*195,
  5-9-6*19+5,
  5-9-6*19-5,
  5-9-6*19*5,
  5-9-6*1+95,
  5-9-6*1+9+5,
  5-9-6*1+9-5,
  5-9-6*1+9*5,
  5-9-6*1-95,
  5-9-6*1-9+5,
  5-9-6*1-9-5,
  5-9-6*1-9*5,
  5-9-6*1*95,
  5-9-6*1*9+5,
  5-9-6*1*9-5,
  5-9-6*1*9*5,
  5-9*6195,
  5-9*619+5,
  5-9*619-5,
  5-9*619*5,
  5-9*61+95,
  5-9*61+9+5,
  5-9*61+9-5,
  5-9*61+9*5,
  5-9*61-95,
  5-9*61-9+5,
  5-9*61-9-5,
  5-9*61-9*5,
  5-9*61*95,
  5-9*61*9+5,
  5-9*61*9-5,
  5-9*61*9*5,
  5-9*6+195,
  5-9*6+19+5,
  5-9*6+19-5,
  5-9*6+19*5,
  5-9*6+1+95,
  5-9*6+1+9+5,
  5-9*6+1+9-5,
  5-9*6+1+9*5,
  5-9*6+1-95,
  5-9*6+1-9+5,
  5-9*6+1-9-5,
  5-9*6+1-9*5,
  5-9*6+1*95,
  5-9*6+1*9+5,
  5-9*6+1*9-5,
  5-9*6+1*9*5,
  5-9*6-195,
  5-9*6-19+5,
  5-9*6-19-5,
  5-9*6-19*5,
  5-9*6-1+95,
  5-9*6-1+9+5,
  5-9*6-1+9-5,
  5-9*6-1+9*5,
  5-9*6-1-95,
  5-9*6-1-9+5,
  5-9*6-1-9-5,
  5-9*6-1-9*5,
  5-9*6-1*95,
  5-9*6-1*9+5,
  5-9*6-1*9-5,
  5-9*6-1*9*5,
  5-9*6*195,
  5-9*6*19+5,
  5-9*6*19-5,
  5-9*6*19*5,
  5-9*6*1+95,
  5-9*6*1+9+5,
  5-9*6*1+9-5,
  5-9*6*1+9*5,
  5-9*6*1-95,
  5-9*6*1-9+5,
  5-9*6*1-9-5,
  5-9*6*1-9*5,
  5-9*6*1*95,
  5-9*6*1*9+5,
  5-9*6*1*9-5,
  5-9*6*1*9*5,
  5*96195,
  5*9619+5,
  5*9619-5,
  5*9619*5,
  5*961+95,
  5*961+9+5,
  5*961+9-5,
  5*961+9*5,
  5*961-95,
  5*961-9+5,
  5*961-9-5,
  5*961-9*5,
  5*961*95,
  5*961*9+5,
  5*961*9-5,
  5*961*9*5,
  5*96+195,
  5*96+19+5,
  5*96+19-5,
  5*96+19*5,
  5*96+1+95,
  5*96+1+9+5,
  5*96+1+9-5,
  5*96+1+9*5,
  5*96+1-95,
  5*96+1-9+5,
  5*96+1-9-5,
  5*96+1-9*5,
  5*96+1*95,
  5*96+1*9+5,
  5*96+1*9-5,
  5*96+1*9*5,
  5*96-195,
  5*96-19+5,
  5*96-19-5,
  5*96-19*5,
  5*96-1+95,
  5*96-1+9+5,
  5*96-1+9-5,
  5*96-1+9*5,
  5*96-1-95,
  5*96-1-9+5,
  5*96-1-9-5,
  5*96-1-9*5,
  5*96-1*95,
  5*96-1*9+5,
  5*96-1*9-5,
  5*96-1*9*5,
  5*96*195,
  5*96*19+5,
  5*96*19-5,
  5*96*19*5,
  5*96*1+95,
  5*96*1+9+5,
  5*96*1+9-5,
  5*96*1+9*5,
  5*96*1-95,
  5*96*1-9+5,
  5*96*1-9-5,
  5*96*1-9*5,
  5*96*1*95,
  5*96*1*9+5,
  5*96*1*9-5,
  5*96*1*9*5,
  5*9+6195,
  5*9+619+5,
  5*9+619-5,
  5*9+619*5,
  5*9+61+95,
  5*9+61+9+5,
  5*9+61+9-5,
  5*9+61+9*5,
  5*9+61-95,
  5*9+61-9+5,
  5*9+61-9-5,
  5*9+61-9*5,
  5*9+61*95,
  5*9+61*9+5,
  5*9+61*9-5,
  5*9+61*9*5,
  5*9+6+195,
  5*9+6+19+5,
  5*9+6+19-5,
  5*9+6+19*5,
  5*9+6+1+95,
  5*9+6+1+9+5,
  5*9+6+1+9-5,
  5*9+6+1+9*5,
  5*9+6+1-95,
  5*9+6+1-9+5,
  5*9+6+1-9-5,
  5*9+6+1-9*5,
  5*9+6+1*95,
  5*9+6+1*9+5,
  5*9+6+1*9-5,
  5*9+6+1*9*5,
  5*9+6-195,
  5*9+6-19+5,
  5*9+6-19-5,
  5*9+6-19*5,
  5*9+6-1+95,
  5*9+6-1+9+5,
  5*9+6-1+9-5,
  5*9+6-1+9*5,
  5*9+6-1-95,
  5*9+6-1-9+5,
  5*9+6-1-9-5,
  5*9+6-1-9*5,
  5*9+6-1*95,
  5*9+6-1*9+5,
  5*9+6-1*9-5,
  5*9+6-1*9*5,
  5*9+6*195,
  5*9+6*19+5,
  5*9+6*19-5,
  5*9+6*19*5,
  5*9+6*1+95,
  5*9+6*1+9+5,
  5*9+6*1+9-5,
  5*9+6*1+9*5,
  5*9+6*1-95,
  5*9+6*1-9+5,
  5*9+6*1-9-5,
  5*9+6*1-9*5,
  5*9+6*1*95,
  5*9+6*1*9+5,
  5*9+6*1*9-5,
  5*9+6*1*9*5,
  5*9-6195,
  5*9-619+5,
  5*9-619-5,
  5*9-619*5,
  5*9-61+95,
  5*9-61+9+5,
  5*9-61+9-5,
  5*9-61+9*5,
  5*9-61-95,
  5*9-61-9+5,
  5*9-61-9-5,
  5*9-61-9*5,
  5*9-61*95,
  5*9-61*9+5,
  5*9-61*9-5,
  5*9-61*9*5,
  5*9-6+195,
  5*9-6+19+5,
  5*9-6+19-5,
  5*9-6+19*5,
  5*9-6+1+95,
  5*9-6+1+9+5,
  5*9-6+1+9-5,
  5*9-6+1+9*5,
  5*9-6+1-95,
  5*9-6+1-9+5,
  5*9-6+1-9-5,
  5*9-6+1-9*5,
  5*9-6+1*95,
  5*9-6+1*9+5,
  5*9-6+1*9-5,
  5*9-6+1*9*5,
  5*9-6-195,
  5*9-6-19+5,
  5*9-6-19-5,
  5*9-6-19*5,
  5*9-6-1+95,
  5*9-6-1+9+5,
  5*9-6-1+9-5,
  5*9-6-1+9*5,
  5*9-6-1-95,
  5*9-6-1-9+5,
  5*9-6-1-9-5,
  5*9-6-1-9*5,
  5*9-6-1*95,
  5*9-6-1*9+5,
  5*9-6-1*9-5,
  5*9-6-1*9*5,
  5*9-6*195,
  5*9-6*19+5,
  5*9-6*19-5,
  5*9-6*19*5,
  5*9-6*1+95,
  5*9-6*1+9+5,
  5*9-6*1+9-5,
  5*9-6*1+9*5,
  5*9-6*1-95,
  5*9-6*1-9+5,
  5*9-6*1-9-5,
  5*9-6*1-9*5,
  5*9-6*1*95,
  5*9-6*1*9+5,
  5*9-6*1*9-5,
  5*9-6*1*9*5,
  5*9*6195,
  5*9*619+5,
  5*9*619-5,
  5*9*619*5,
  5*9*61+95,
  5*9*61+9+5,
  5*9*61+9-5,
  5*9*61+9*5,
  5*9*61-95,
  5*9*61-9+5,
  5*9*61-9-5,
  5*9*61-9*5,
  5*9*61*95,
  5*9*61*9+5,
  5*9*61*9-5,
  5*9*61*9*5,
  5*9*6+195,
  5*9*6+19+5,
  5*9*6+19-5,
  5*9*6+19*5,
  5*9*6+1+95,
  5*9*6+1+9+5,
  5*9*6+1+9-5,
  5*9*6+1+9*5,
  5*9*6+1-95,
  5*9*6+1-9+5,
  5*9*6+1-9-5,
  5*9*6+1-9*5,
  5*9*6+1*95,
  5*9*6+1*9+5,
  5*9*6+1*9-5,
  5*9*6+1*9*5,
  5*9*6-195,
  5*9*6-19+5,
  5*9*6-19-5,
  5*9*6-19*5,
  5*9*6-1+95,
  5*9*6-1+9+5,
  5*9*6-1+9-5,
  5*9*6-1+9*5,
  5*9*6-1-95,
  5*9*6-1-9+5,
  5*9*6-1-9-5,
  5*9*6-1-9*5,
  5*9*6-1*95,
  5*9*6-1*9+5,
  5*9*6-1*9-5,
  5*9*6-1*9*5,
  5*9*6*195,
  5*9*6*19+5,
  5*9*6*19-5,
  5*9*6*19*5,
  5*9*6*1+95,
  5*9*6*1+9+5,
  5*9*6*1+9-5,
  5*9*6*1+9*5,
  5*9*6*1-95,
  5*9*6*1-9+5,
  5*9*6*1-9-5,
  5*9*6*1-9*5,
  5*9*6*1*95,
  5*9*6*1*9+5,
  5*9*6*1*9-5,
  5*9*6*1*9*5
----------------------------------
  ])