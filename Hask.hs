
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Blue Blue = True
colorEq Green Green = True
colorEq _ _ = False

-- Factorial implementation
fact :: Integer -> Integer -> Integer
fact 1 prod = prod
fact n prod = fact (pred n) (n * prod)

factSlow :: Integer -> Integer
factSlow 1 = 1
factSlow n = n * factSlow (pred n)

factBetter :: Integer -> Integer
factBetter n = product [1..n]

sumPattern :: (Integral a) => [a] -> a
sumPattern [] = 0
sumPattern (x:xs) = x + sumPattern xs

sumOddMultEven :: [Integer] -> Integer
sumOddMultEven lst
  | null lst = 1
  | odd x  = x + sumOddMultEven xs
  | even x = x * sumOddMultEven xs
  where (x:xs) = lst

sumOddMultEven2 :: [Integer] -> Integer
sumOddMultEven2 [] = 1
sumOddMultEven2 (x:xs) =
  if odd x
  then
    x + sumOddMultEven2 xs
  else
    x * sumOddMultEven2 xs

sumTuple :: (Int, Int) -> Int
sumTuple t = fst t + snd t

applyUnaryIntFn :: (Int -> Int) -> Int -> Int
applyUnaryIntFn fn n = fn n

applyUnaryFn fn n = fn n

-- Perceptrons can be volatile; implement sigmoid neuron so that small
-- changes in the weight cause small or otherwise proportional changes
-- in the output.
percept :: (Ord a, Num a) => [a] -> a -> ([a] -> Int)
percept wghts thresh =
  (\vals -> if sum (zipWith (*) wghts vals) + thresh > 0
            then 1
            else 0)

letMeIn :: (Num a) => Double -> Double -> Double
letMeIn timeOfKnock responseTime =
  let expectedDoorOpen = timeOfKnock + responseTime
      epsilon = 0.01
  in expectedDoorOpen + epsilon

sumThreeTuple :: (Integer, Integer, Integer) -> Integer
sumThreeTuple threeTuple =
  let (a, b, c) = threeTuple
  in a + b + c

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

matchMaker stuff =
  case stuff of [] -> 0
                [x] -> 1
                xs -> length xs

fib :: Integer -> Integer 
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

max' :: (Ord a) => [a] -> a
max' [] = error "wtf man??"
max' [x] = x
max' (x:xs)
  | x > maxOfTail = x
  | otherwise = maxOfTail
  where maxOfTail = max' xs

reverseList :: [a] -> [a]
reverseList []  = []
reverseList [x] = [x]
reverseList xs  = (last xs):(reverseList (init xs))



