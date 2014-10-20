module Dice where
import Infer
import Data.List
import Data.Tuple

-- Data types specific to solving 'Dice' problems
data Die = D4 | D6 | D8 | D10 | D12 | D20 deriving (Show,Eq,Ord)
data DieTriple = DieTriple Die Die Die deriving(Show)
data ThrowSet = ThrowSet Integer Integer Integer deriving(Show) 
data DiePair = DiePair Die Die deriving (Show)

instance Eq DiePair where 
    DiePair d1 d2 == DiePair d3 d4 = 
        (d1 == d3 && d2 == d4) || (d1 == d4 && d2 == d3)
 
instance Eq DieTriple where 
    DieTriple d1 d2 d3 == DieTriple d4 d5 d6 
        = (d1 == d4 && d2 == d5 && d3 == d6) || 
          (d1 == d4 && d2 == d6 && d3 == d5) || 
          (d1 == d5 && d2 == d6 && d3 == d4) ||
          (d1 == d5 && d2 == d4 && d3 == d6) ||
          (d1 == d6 && d2 == d4 && d3 == d5) ||
          (d1 == d6 && d2 == d5 && d3 == d4)

instance Eq ThrowSet where 
    ThrowSet d1 d2 d3 == ThrowSet d4 d5 d6 
        = (d1 == d4 && d2 == d5 && d3 == d6) || 
          (d1 == d4 && d2 == d6 && d3 == d5) || 
          (d1 == d5 && d2 == d6 && d3 == d4) ||
          (d1 == d5 && d2 == d4 && d3 == d6) ||
          (d1 == d6 && d2 == d4 && d3 == d5) ||
          (d1 == d6 && d2 == d5 && d3 == d4)


dieDist :: Die -> P Integer
dieDist D4 = equally [1..4]
dieDist D6 = equally [1..6]
dieDist D8 = equally [1..8]
dieDist D10 = equally [0..9]
dieDist D12 = equally [1..12]
dieDist D20 = equally [1..20]

-- ************** Shortcuts for commonly used functions *****
psum :: P a -> Probability
psum dist = pfoldl (+) 0 dist

plogsum :: P a -> Probability
plogsum dist = decodeProb (plogfoldl (+) 0 dist)

decodeDist :: P a -> P a
decodeDist dist = pmap' decodeProb dist

codeDist :: P a -> P a
codeDist dist = pmap' codeProb dist

renorm :: P a -> LogProb -> P a
-- ^ Renormalize the space such that the probabilities sum to 1
-- Assumes probability distributions are in log space
renorm dist d = pmap' (\x -> (x - d)) dist
-- **************************************************

twoDieProb :: P Die -> Die -> Die -> ((Integer,Integer) -> Bool)
             -> Probability
-- ^ Draw two dice, throw them, total the numbers and give
-- a probability of meeting the filter
twoDieProb dist d1 d2 f =
    let p1 = outcomeProb dist d1
        p2 = outcomeProb dist d2
        pjoin = join (dieDist d1) (dieDist d2)
        sumProb = plogsum (pfilter f pjoin)
    in
      p1*p2*sumProb

-- ***************************************************

dicePairDist :: P Die -> P DiePair
dicePairDist dist = 
    regroup (pmap (\(x,y) -> (DiePair x y)) (join dist dist))

diceTripleDist :: P Die -> P DieTriple
diceTripleDist (Dist dist) = 
    regroup (Dist [ ((DieTriple d1 d2 d3),p1+p2+p3) | 
                    (d1,p1) <- dist, (d2,p2) <- dist, (d3,p3) <- dist] )

dieTripleToThrowHelper :: DieTriple -> P ThrowSet
dieTripleToThrowHelper (DieTriple d1 d2 d3) = 
    let Dist dist1 = dieDist d1
        Dist dist2 = dieDist d2
        Dist dist3 = dieDist d3
    in
      regroup (Dist [ ((ThrowSet t1 t2 t3), p1+p2+p3) | 
                                     (t1,p1) <- dist1, (t2,p2) <- dist2, 
                                     (t3,p3) <- dist3])

condThrowSet' :: P (DieTriple, ThrowSet) -> P (DieTriple, ThrowSet)
condThrowSet' dist = 
    pfilter (\(x,y) -> (isMul4 y)) 
                (pfilter (\(x,y) -> (isEleven y)) 
                 (pfilter (\(x,y) -> (isSeven y)) dist))

condThrowSet :: P ThrowSet -> P ThrowSet
condThrowSet dist = 
    pfilter isMul4 (pfilter isEleven (pfilter isSeven  dist))

isSeven :: ThrowSet -> Bool
isSeven (ThrowSet 7 y z) = True
isSeven (ThrowSet x 7 z) = True
isSeven (ThrowSet x y 7) = True
isSeven (ThrowSet x y z) = False

isEleven :: ThrowSet -> Bool
isEleven (ThrowSet 11 y z) = True
isEleven (ThrowSet x 11 z) = True
isEleven (ThrowSet x y 11) = True
isEleven (ThrowSet x y z) = False

isMul4 :: ThrowSet -> Bool
isMul4 (ThrowSet x y z) 
    | x `mod` 4 == 0 = True
    | y `mod` 4 == 0 = True
    | z `mod` 4 == 0 = True
    | otherwise = False
     
isD8 :: DieTriple -> Bool
isD8 (DieTriple x y z) 
    | x == D8 = True
    | y == D8 = True
    | z == D8 = True
    | otherwise = False

-- ********** Solutions to the problems ******************

partD :: P Die -> Probability
partD diceDist = 
    ((+) (twoDieProb diceDist D6 D12 (\(x,y) -> (x+y == 11)))
         (twoDieProb diceDist D12 D6 (\(x,y) -> (x+y == 11))))

{- 
   You draw three dice from the standard bag, throw them, 
   and you see a 7, an 11, and a multiple of 4.  
   What is the probability that at least one of the dice is a d8? 
 -}  
q1New :: P Die -> Probability
q1New diceDist = 
    let diceJoint = diceTripleDist diceDist      
        fullJoint = bindx diceJoint 
                    (\x -> (condThrowSet (dieTripleToThrowHelper x)))
        marginal = collapseRight fullJoint 
        conditional = pfilter isD8 (renorm marginal (codeProb (plogsum marginal)))
    in
      plogsum conditional
