import Data.List (sort)

type Price = Int
type Profit = Int
type History = [Price]
type Trade = (History, History)
type OwnershipPeriod = (History, History, History) -- Unowned, owned, unowned
type OwnershipProfit = (Profit, OwnershipPeriod)

profit :: OwnershipPeriod -> Profit
profit (_, owned, unowned) = head unowned - head owned

totalProfit :: [OwnershipPeriod] -> Profit
totalProfit = sum . map profit

possibleTrades :: History -> [OwnershipPeriod]
possibleTrades history = do
  buyDay <- [0 .. length history - 2]
  sellDay <- [buyDay + 1..length history - 1]
  let daysOwned = sellDay - buyDay
  let preBuy = take buyDay history
  let owned = take daysOwned . drop buyDay $ history
  let postSell = drop sellDay history
  return (preBuy, owned, postSell)

reasonableSells' :: Price -> History -> [(History, History)]
reasonableSells' _ [x] = [([], [x])]
reasonableSells' previousPrice (x:y:xs)
  | x > previousPrice && x > y =
    let thisSell = ([], x:y:xs)
        addXToHistory = (\(a,b) -> (x:a, b))
        otherReasonableSells = map addXToHistory $ reasonableSells' x (y:xs)
    in thisSell : otherReasonableSells
reasonableSells' _ (x:xs) =
  let addXToHistory = (\(a,b) -> (x:a, b))
  in map addXToHistory $ reasonableSells' x xs

-- Produce a list of sales for local maximum prices
reasonableSells :: History -> [(History, History)]
reasonableSells (x:xs) = reasonableSells' (pred x) (x:xs)

reasonableBuys' :: Price -> History -> [(History, History)]
reasonableBuys' _ [] = []
reasonableBuys' previousPrice (x:y:xs)
  | x < previousPrice && x < y =
    let thisBuy = ([], x:y:xs)
        addXToHistory = (\(a,b) -> (x:a, b))
        otherReasonableBuys = map addXToHistory $ reasonableBuys' x (y:xs)
    in thisBuy : otherReasonableBuys
reasonableBuys' _ (x:xs) =
  let addXToHistory = (\(a,b) -> (x:a, b))
  in map addXToHistory $ reasonableBuys' x xs
  
-- Produce a list of sales for local minimum prices
reasonableBuys :: History -> [(History, History)]
reasonableBuys (x:xs) = reasonableBuys' (succ x) (x:xs)

reasonableTransactions :: History -> [OwnershipPeriod]
reasonableTransactions history = do
  buy@(preBuy, (buyPrice:afterBuy)) <- reasonableBuys history
  sell@(preSell, (sellPrice:afterSell)) <- reasonableSells afterBuy
  return (preBuy, buyPrice:preSell, sellPrice:afterSell)

reasonableTransactionSequences :: History -> [[OwnershipPeriod]]
reasonableTransactionSequences history = do
  reasonableTrans@(before, during, after) <- reasonableTransactions history
  reasonableContinuation <- [] : reasonableTransactionSequences after
  return (reasonableTrans : reasonableContinuation)

optimalTradingSequence :: Int -> History -> [(Profit, [OwnershipPeriod])]
optimalTradingSequence transactionLimit history =
  reverse .
  sort .
  map (\s -> (totalProfit s, s)) .
  filter ((<= transactionLimit) . length) . 
  reasonableTransactionSequences $
  history

history :: History
history = [28,133,96,129,126,15,31,115,133,34,96,125,18,159,118,147,48,131,139,141,61,38,160,154,74,16,6,2,53,186,115,31,177,95,110,183,114,17,9,171,191,45,199,111,184,140,172,63]

tradeLimit :: Int
tradeLimit = 50

optimalTrade :: Profit
optimalTrade =
  fst .
  head .
  optimalTradingSequence tradeLimit $
  history

main :: IO ()
main = print optimalTrade