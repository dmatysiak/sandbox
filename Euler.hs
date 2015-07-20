--
-- Project Euler Solutions
--
import Data.Char

-- Problem 1
divides :: Integer -> Integer -> Bool
divides m n = snd (divMod n m) == 0

problemOne = problemOne' 0 1

problemOne' :: Integer -> Integer -> Integer
problemOne' sum n
  | n < 1000 =
    if divides 3 n || divides 5 n
    then
      problemOne' (n + sum) (succ n)
    else
      problemOne' sum (succ n)
  | otherwise = sum
    
-- Problem 2
problemTwo = problemTwo' 1 1 0

problemTwo' :: Integer -> Integer -> Integer-> Integer
problemTwo' n m sum
  | fib < 4000000 = 
      if even fib
      then problemTwo' m fib (sum + fib)
      else problemTwo' m fib sum
  | otherwise = sum
  where fib = n + m

-- Problem 3
problemThree =
  problemThree' 60085147514 60085147514

problemThree' :: Integer -> Integer -> Integer
problemThree' n curr
  | curr <= sqrtLimit =
      error "Wtf!"
  | isPrime curr =
      if divides curr n
      then curr
      else problemThree' n (pred curr)
  | otherwise = problemThree' n (pred curr)
  where sqrtLimit = floorSqrt n

floorSqrt :: Integer -> Integer
floorSqrt n =
  toInteger $ fromIntegral $ floor $ sqrt $ fromIntegral n
          
isPrime :: Integer -> Bool
isPrime n = isPrime' n 2

isPrime' :: Integer -> Integer -> Bool
isPrime' 2 _ = True
isPrime' n m
  | m <=  (ceiling $ sqrt $ fromIntegral n) =
      if divides m n
      then False
      else isPrime' n (succ m)
  | otherwise = True

-- Problem 4
problemFour = problemFour' 999 999 1

problemFour' :: Int -> Int -> Int -> Int
problemFour' 100 100 prod = prod 
problemFour' n1 100 prod = problemFour' (pred n1) (pred n1) prod
problemFour' n1 n2 prod =
  let prod' = n1 * n2 in
   if isPalindrome prod' && prod' > prod
   then problemFour' n1 (pred n2) prod'
   else problemFour' n1 (pred n2) prod
        
isPalindrome :: Int -> Bool
isPalindrome n =
  n == (read $ reverse $ show n :: Int)

-- Problem 5 (works, but inefficent as fuck)
problemFive = problemFive' (fromIntegral $ foldl (*) 1 (filter isPrime (map toInteger [1..20])))
problemFive' :: Int -> Int
problemFive' curr =
  let end = 20 in
   if (length $ filter (\m -> divides (toInteger m) (toInteger curr)) [1..end]) == end
   then curr
   else problemFive' (succ curr)
  
-- Problem 6
problemSix :: Integer -> Integer
problemSix nMax =
  (foldl (+) 0 [1..nMax])^2 - foldl (+) 0 (map (\n -> n^2) [1..nMax])

-- Problem 7
problemSeven = problemSeven' 3 2 1

problemSeven' :: Integer -> Integer -> Integer -> Integer
problemSeven' currNum currPrime 10001 = currPrime
problemSeven' currNum currPrime cnt = 
  if isPrime currNum
  then problemSeven' (succ currNum) currNum (succ cnt)
  else problemSeven' (succ currNum) currPrime cnt
       
-- Problem 8

theGridRaw =
  [73167176531330624919225119674426574742355349194934,
   96983520312774506326239578318016984801869478851843,
   85861560789112949495459501737958331952853208805511,
   12540698747158523863050715693290963295227443043557,
   66896648950445244523161731856403098711121722383113,
   62229893423380308135336276614282806444486645238749,
   30358907296290491560440772390713810515859307960866,
   70172427121883998797908792274921901699720888093776,
   65727333001053367881220235421809751254540594752243,
   52584907711670556013604839586446706324415722155397,
   53697817977846174064955149290862569321978468622482,
   83972241375657056057490261407972968652414535100474,
   82166370484403199890008895243450658541227588666881,
   16427171479924442928230863465674813919123162824586,
   17866458359124566529476545682848912883142607690042,
   24219022671055626321111109370544217506941658960408,
   07198403850962455444362981230987879927244284909188,
   84580156166097919133875499200524063689912560717606,
   05886116467109405077541002256983155200055935729725,
   71636269561882670428252483600823257530420752963450]

theGrid = map (\n -> map digitToInt (show n)) theGridRaw

problemEight' :: [[Integer]] -> (Integer, Integer) -> [(Integer, Integer)] -> Integer
problemEight' grid cellCoords adjacentCoords =
  
  
adjacentCells :: (Integer, Integer) -> (Integer, Integer) -> [(Integer, Integer)]
adjacentCells (xCoord, yCoord) (xSize, ySize) =
  let diffs = [(0, -1), (0, 1), (-1, 0), (1, 0)] in
   filter (\coord -> fst coord >= 0 && fst coord <= xSize - 1
                     && snd coord >= 0 && snd coord <= ySize - 1)
   [(x, y) | (x, y) <- map (\diff -> (fst diff + xCoord, snd diff + yCoord)) diffs]
        


