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
data P a = Dist [(a, LogProb)] deriving( Eq, Show )

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

pmap' :: (LogProb -> LogProb) -> P a -> P a
pmap' f (Dist dist1) = 
    Dist (zip (map fst dist1) (map f (map snd dist1)))

pfilter :: (a -> Bool) -> P a -> P a 
-- ^ conditional probability
pfilter pred (Dist dist) = Dist [ (x,px) | (x,px) <- dist, pred x]    

pfoldl :: (LogProb -> LogProb -> LogProb) -> Double -> P a -> LogProb
pfoldl f i (Dist dist) = codeProb (foldl f i (map decodeProb (map snd dist)))

{- TODO
regroup :: Eq a => P a -> [[(a, LogProb)]]
regroup (Dist dist) = 
    let uniq = [ x | x <- nub (map fst dist) ]
        a = [ filter (\(x,y) -> ((==) x u)) dist | u <- uniq]
    in
      a
-}
--------------- Combining Forms  -------------------

allpairs :: (a, LogProb) -> (LogProb -> LogProb)-> P b -> [((a,b),LogProb)]
-- ^ Instead of doing bindx in one step, splitting this into a helper function
-- which can generate all pairs
allpairs pair f (Dist y) = 
    let probs = snd (unzip y)
    in
      zip [ (fst pair, ys) | ys <- support (Dist y) ] 
              (map f probs)

bindx :: P a -> (a -> P b) -> P (a,b)
-- ^ `bindx d k` produces a joint distribution of a's and b's where
-- the distribution of b's for any given a is given by `k a`
bindx (Dist dist1) f = 
    let list = concat [ allpairs x (+ (snd x)) (f (fst x)) | x <- dist1]
    in
      Dist list

join :: P a -> P a -> P (a,a)
-- ^ Combining distributions over the same type into a joint distribution
join (Dist xs) (Dist ys) = Dist [ ((x,y),px+py)  | (x,px) <- xs, (y,py) <- ys]

{- TODO
joinCond :: P (a,b) -> P b -> P a
-- ^ Given a distribution P(M|d) and P(d), combining them to create P(M) 
joinCond (Dist condJoint) (Dist dist) 
-}

collapseLeft :: Eq b => P (a,b) -> P b 
-- ^ generates the marginal distribution of b from a joint distribution
-- of (a,b)
collapseLeft (Dist dist) = 
    let uniqb = nub (map snd (map fst dist))
        probs = map 
                (\y -> (sum 
                        (map decodeProb 
                         (map snd 
                          [ x | x <- dist , snd (fst x) == y ])))) 
                uniqb
    in
      Dist (zip uniqb (map codeProb probs))


collapseRight :: Eq a => P (a,b) -> P a
-- ^ generates the marginal distribution of a from a joint distribution
-- of (a,b)
collapseRight (Dist dist) = 
    let uniqb = nub (map fst (map fst dist))
        probs = map 
                (\y -> (sum 
                        (map decodeProb 
                         (map snd 
                          [ x | x <- dist , fst (fst x) == y ])))) 
                uniqb
    in
      Dist (zip uniqb (map codeProb probs))

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
              sum [snd x | x <- dist, pred (fst x)]

support :: P a -> [a]
-- ^ list including every value of type a that has nonzero probability          
support (Dist dist) = map fst dist

expected :: (a -> Double) -> P a -> Double
-- ^ the expected value of the given function
expected f (Dist dist) = 
    sum (zipWith (*) (map f (map fst dist))
                     (map decodeProb (map snd dist)))
