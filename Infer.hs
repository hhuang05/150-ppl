{-
  Author: Moses Huang, based on code originally written by Karl Cronberg


-- `F a` is a probability distribution, with Finite support, over values of type a
------------ constructors ------------

fchoose' :: Probability -> F a -> F a -> F a
fchoose :: Probability -> a -> a -> F a
 -- `choose p left right` mixes two values (or two distributions),
 -- taking the left with probability p and right with probability 1-p.
------------ transformers -----------------------


pfilterMap :: (a -> Maybe b) -> F a -> F b
pfilterMap f = pmap fromJust . pfilter isJust . pmap f
------------ combining forms ----------------------

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
type LogProb = Double

--------------- Helpers  -------------------
codeProb :: Probability -> LogProb
-- ^ to prevent underflow, we can encode prob using natural log function
codeProb p = log p

decodeProb :: LogProb -> Probability
-- ^ Once we need an actual probability, we apply the inverse log 
decodeProb lp = exp lp

-- Formal representation of Probability distribution
-- Modified from Karl/Jayme's code
data P a = Dist [(a, LogProb)] deriving( Show, Eq )

-- Must have operational procedure that Distributions can only be 
-- combined using summation, not multiplication since it is the Log
-- probabilities that are stored

----------------- Constructors ---------------------
certainly :: a -> P a
certainly xs = Dist [(xs, codeProb 1.0)]

equally :: [a] -> P a
equally xs = 
  Dist (zip xs (map codeProb
                        (replicate 
                         (length xs) 
                         ((/) 1.0 (realToFrac (length xs))))))

normWeighted :: (Real w) => [(a, w)] -> P a
-- ^ Takes a weighted combination of 'a' and produces a
-- normalized probability distribution based on the weights
normWeighted xs = 
    let denom = realToFrac $ sum (map snd xs)
        probs = map codeProb (map (/ denom) (map realToFrac (map snd xs)))
        as = map fst xs
    in 
      Dist (zip as probs)


--------------- Transformers -----------------------
pmap :: (a -> b) -> P a -> P b
pmap f (Dist dist1) = 
    Dist (zip (map f (map fst dist1)) (map snd dist1))


pfilter :: (a -> Bool) -> P a -> P a 
-- ^ conditional probability, renormalized to sum to 1
pfilter pred (Dist dist) = normWeighted [ xs | xs <- dist, pred (fst xs) ]

--------------- Combining Forms  -------------------
bindx :: P a -> (a -> P b) -> [Probability] -- P (a,b)
-- ^ `bindx d k` produces a joint distribution of a's and b's where
-- the distribution of b's for any given a is given by `k a`
bindx (Dist dist1) f = 
    let --combo = pmap f (Dist dist1)     -- Joint
        -- combo :: P (P b)
        -- alist = map fst dist1
        probs = map snd dist1
    in
      probs
-- collapse



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
                          (y:_) -> decodeProb (snd $ head a)
    
eventProb :: (Eq a) => (a -> Bool) -> P a -> Probability 
-- ^ probability of an Event, which matches the given predicate
eventProb pred (Dist dist) = checkNil (Dist dist)
    where checkNil (Dist []) = 0
          checkNil (Dist (y:_)) = 
              sum [decodeProb (snd x) | x <- dist, pred (fst x)]

support :: P a -> [a]
-- ^ list including every value of type a that has nonzero probability          
support (Dist dist) = map fst dist

expected :: (a -> Double) -> P a -> Double
-- ^ the expected value of the given function
expected f (Dist dist) = 
    sum (zipWith (*) (map f (map fst dist))
                     (map decodeProb (map snd dist)))
