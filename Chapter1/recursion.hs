pow2 n =
  if n == 0
    then 1
    else 2 * (pow2 (n - 1))

pow2' n
  | n == 0    = 1
  | otherwise = 2 * (pow2' (n - 1))

repeatString str n =
  if n == 0
    then ""
    else str ++ (repeatString str (n - 1))

double numbers =
  if null numbers
    then []
    else (2 * (head numbers)) : (double (tail numbers))

double' []      = []
double' (h : t) = (2 * h) : (double t)

double'' numbers = case numbers of
  []      -> []
  (h : t) -> (2 * h) : (double t)

double''' = map (2*)

removeOdd numbers =
  if null numbers
    then []
    else
      if (mod (head numbers) 2) == 2 -- even?
        then (head numbers) : (removeOdd (tail numbers))
        else removeOdd (tail numbers)

removeOdd' [] = []
removeOdd' (h : t)
  | mod h 2 == 0 = h : (removeOdd' t)
  | otherwise    = removeOdd' t

isEven x = x `mod` 2 == 0
removeOdd'' = filter isEven

anyEven numbers = case (removeOdd' numbers) of
  []      -> False
  (h : t) -> True

numEven numbers =
  let evenNums = removeOdd' numbers
  in length evenNums
