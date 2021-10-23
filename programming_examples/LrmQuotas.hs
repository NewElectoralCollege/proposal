{-
On some occasions, the Hagenbach-Bischoff and Imperiali quotas can result in more
seats being allocated than necessary.
This is solved by increasing the quota by one until the number of seats is correct.
This is mathematically impossible under Hare or Droop.
-}

import Lrm hiding (main)

import Data.List
import Data.Ord

infixr 0 `for`

-- Quotas

data Quota = Traditional (Int -> Int -> Int) | Generous Int (Int -> Int -> Int)

hare :: Quota
hare =
  Traditional div

droop :: Quota
droop =
  Traditional (\votes seats -> votes `div` (seats + 1) + 1)

hagenbachBischoff :: Quota
hagenbachBischoff =
  Generous 0 (\votes seats -> votes `div` (seats + 1))

imperiali :: Quota
imperiali =
  Generous 0 (\votes seats -> votes `div` (seats + 2))

-- Helpers

proper :: Quota -> Int -> [Party] -> Quota
proper (Generous a b) seats parties
  | awarded == seats = Generous a b
  | otherwise        = Generous (a + 1) b 
  where 
    q = b (sum $ map votes parties) seats
    awarded = sum $ map (\p -> votes p `div` q) parties
proper q _ _ = q

for :: Quota -> Int -> Int -> Int
for quota votes seats =
  case quota of
    Traditional f -> f votes seats
    Generous a f ->  f votes seats + a

hareMethodWithQuotas :: Quota -> Int -> [Party] -> [Party]
hareMethodWithQuotas _ _ [] = error "No Parties provided"
hareMethodWithQuotas _ 0 _ = error "At least one seat must be awarded"
hareMethodWithQuotas q s list =
  extraSeats s $
  tp $ sortOn (Down . extraVotes quota) $ quotaSeats quota list
  where
    tv = totalVotes list
    quota = (q `for` tv) s

-- Testing

main :: IO ()
main =
  print $
  hareMethodWithQuotas imperiali 5 $ makeParties [31251, 4090, 70709, 30275, 73888]