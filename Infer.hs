{-
  Author: Moses Huang, based on code originally written by Karl Cronberg


-- `F a` is a probability distribution, with Finite support, over values of type a
------------ constructors ------------

fchoose' :: Probability -> F a -> F a -> F a
fchoose :: Probability -> a -> a -> F a
 -- `choose p left right` mixes two values (or two distributions),
 -- taking the left with probability p and right with probability 1-p.
------------ transformers -----------------------
pmap :: (a -> b) -> F a -> F b
pfilter :: (a -> Bool) -> F a -> F a -- ^ conditional probability
pfilterMap :: (a -> Maybe b) -> F a -> F b
pfilterMap f = pmap fromJust . pfilter isJust . pmap f
------------ combining forms ----------------------
bindx :: F a -> (a -> F b) -> F (a, b)
 -- `bindx d k` produces a joint distribution of a's and b's where
 -- the distribution of b's for any given a is given by `k a`.
------------- observers ------------------------- 


byLikelihood :: Ord a => F a -> [(Probability, a)]
  -- ^ list of all possibilities ordered by decreasing probabilities.
  -- No value of type a appears more than once in the list.
 
-- variance is left as an exercise for the reader
----------- Algebraic laws ------------------------
choose_weight_law :: Eq a => Probability -> a -> a -> Bool
choose_weight_law p left right =
 fchoose p left right == weightedly [(p, left), (1-p, right)]
--------------------------------------------------------------------------
-}

module Infer where
import Data.List

-- Modifying this from Jayme's code
type Probability = Double

-- Formal representation of Probability distribution
-- Modified from Karl/Jayme's code
data P a = Dist [(a, Probability)] deriving( Show, Eq )

----------------- Constructors ---------------------
certainly :: a -> P a
certainly xs = Dist [(xs, 1.0)]

equally :: [a] -> P a
equally xs = 
  Dist (zip xs (replicate 
                (length xs) 
                ((/) 1.0 (realToFrac (length xs)))))

normWeighted :: (Real w) => [(w, a)] -> P a
-- ^ Takes a weighted combination of 'a' and produces a
-- normalized probability distribution based on the weights
normWeighted xs = 
    let denom = realToFrac $ sum (map fst xs)
        probs = map (/ denom) (map realToFrac (map fst xs))
        as = map snd xs
    in 
      Dist (zip as probs)

--------------- Transformers -----------------------
pmap :: (a -> b) -> P a -> P b
pmap f (Dist dist1) = 
    Dist (zip (map f (map fst dist1)) (map snd dist1))

--------------- Observers --------------------------
outcomeProb :: (Eq a) => P a -> a -> Probability 
-- ^ probability of an outcome from the distribution on 'a'
-- Currently O(n) access time
-- can probably use a hash table to get O(1) access time
outcomeProb (Dist dist) out = checkNil (Dist dist)
    where checkNil (Dist []) = 0
          checkNil (Dist (y:_)) = 
              let a = [x | x <- dist, (fst x) == out]
              in 
                -- In case outcome is not in the set
                case a of [] -> 0.0
                          (y:_) -> snd $ head a
    
eventProb :: (Eq a) => (a -> Bool) -> P a -> Probability 
-- ^ probability of an Event, which matches the given predicate
eventProb pred (Dist dist) = checkNil (Dist dist)
    where checkNil (Dist []) = 0
          checkNil (Dist (y:_)) = sum [snd x | x <- dist, pred (fst x)]

support :: P a -> [a]
-- ^ list including every value of type a that has nonzero probability          
support (Dist dist) = map fst dist

expected :: (a -> Double) -> P a -> Double
-- ^ the expected value of the given function
expected f (Dist dist) = 
    sum (zipWith (*) (map f (map fst dist))
                     (map snd dist))

{-
-- Part (B)
throw :: D -> P Int
throw d = (SingleDie d)

-- Count # of instances of x in list of xs
count :: (Eq a) => a -> [a] -> Int
count x xs = length (filter (\x' -> x' == x) xs)

-- P(Sum = sum | D1 = d1, D2 = d2)
throw2 :: D -> D -> Int -> Float
throw2 (D d1) (D d2) sum =
  let allsums = (concat (map (\y -> (map (+y) [1..d1])) [1..d2])) in
  ((/) (realToFrac (count sum allsums)) (realToFrac (length allsums)))

-- Part (C)
draw :: D -> Bag -> Float
draw d (Bag xs)
  | xs == [] = 0.0
  | otherwise = 
    ((/) c s)
    where
      s = realToFrac (sum (map fst xs))  -- number of dice in the bag
      dice = (map snd xs)                -- list of dice
      Just i = elemIndex d dice          -- index of the die in the bag
      c = realToFrac (fst (xs !! i))     -- number dice with d-sides in the bag
  

-- Probability of drawing a d1-sided die and a d2-sided die from the given bag
-- P(D1 = d1 and D2 = d2) = P(D1 = d1 | D2 = d2) * P(D2 = d2)
-- Drawing with replacement, so draws from bag are independent
draw2 :: D -> D -> Bag -> Float
draw2 d1 d2 bag =
  ((*) (draw d1 bag) (draw d2 bag))

-- Part (D)
joint :: D -> D -> Int -> Bag -> Float
joint d1 d2 s bag = (draw2 d1 d2 bag) * (throw2 d1 d2 s)

-- Computes the number of ways an m-sided and an n-sided die can sum to s
countSums :: Int -> Int -> Int -> Int
countSums m n s
  | m > n = countSums n m s  -- re-order terms such that n >= m
  | (m >= s) && (n >= s) = s
  | (m < s) && (n >= s) = m
  | n + m < s = 0
  | (m < s) && (n < s) = n+m-s+1
  | otherwise = error "invalid countSums input?"

-- Part (E)
cndtnl :: D -> D -> Int -> Bag -> Float
cndtnl (D d1) (D d2) sum (Bag bag) =
  let dice = map snd bag
      pairs = [Pair x y | x <- dice, y <- dice]
      f1 = (filter (\ (Pair (D a) (D b)) -> ((a+b) >= sum)) pairs) -- filter out pairs which can't add to sum
      f2 = [(count x f1,x) | x <- (nub f1)] -- compress duplicate pairs
      f3 = [ (n,countSums a b sum) | (n,Pair (D a) (D b)) <- f2]
      denom = foldr (+) 0 (map (\ (a,b) -> a*b) f3) -- # of ways possible to roll sum given bag
      numer = countSums d1 d2 sum -- # of ways to roll sum given d1 and d2
  in (rTF numer) / (rTF denom)

--countTally :: Int -> Int -> Int -> Int

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

-- Computes the number of ways an m-sided and an n-sided die can sum to greater than s
countGTSums :: Int -> Int -> Int -> Int
countGTSums m n s
  | n + m < s = 0
  | m > n = countGTSums n m s -- re-order terms such that n >= m
  | (m >= s) && (n >= s) = (n-2*s+m)*m - (countSums m n s)
  | (m < s) && (n >= s) = (n-s-1)*m + (div (m*(m-1)) 2)
  | (m < s) && (n < s) = div (((^) (n+m-s+1) 2) - (countSums m n s)) 2

{-|
-- Part (F)
RTally :: D -> D -> Int -> Int -> Bag -> Float
RTally (D d1) (D d2) tallies trials bag =
  let
    bin_coeff = (choose trials tallies)
    p1 = (^^) (countSums ) tallies
  in
    returnval

--RTally :: D -> D -> Int -> Int -> Bag -> Float
--RTally d1 d2 tallies trials =
--  let pdice = (draw2 d1 d2 bag) -}

-}
