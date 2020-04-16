type Point = (Double, Double)

midpoint :: Point -> Point -> Point
midpoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

newtype CustomerId = CustomerId Int

customerToInt :: CustomerId -> Int
customerToInt (CustomerId i) = i

-- data Customer = Customer
--     { customerId  :: CustomerId
--     , name        :: String
--     , luckyNumber :: Int
--     }

-- alice :: Customer
-- alice = Customer
--     { customerId  = CustomerId 1
--     , name        = "Alice"
--     , luckyNumber = 42
--     }

data Customer = Customer CustomerId String Int

alice :: Customer
alice = Customer (CustomerId 1) "Alice" 42

getCustomerId :: Customer -> CustomerId
getCustomerId (Customer id _ _) = id
