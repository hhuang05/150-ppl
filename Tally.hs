module Tally where
import Infer
import Dice
import Data.List

data Column = LEFT | RIGHT deriving(Show,Eq,Ord)
type Trials = [Column]
data Tally = Tally { left :: Int, right :: Int } deriving (Show)

instance Eq Tally where 
    Tally t1 t2 == Tally t3 t4 = 
        (t1 == t3 && t2 == t4) 


sumSpace :: DiePair -> P Integer
-- ^ Computes the probability distribution of a pair of dice
-- as the prob dist of its sum
sumSpace (DiePair d1 d2) =
    regroup 
    (pmap (\(x,y) -> (x+y)) (join (dieDist d1) (dieDist d2)))

sumToCol :: Integer -> Column  
sumToCol s 
    | s <= 7 = LEFT
    | otherwise = RIGHT

sumToColDist :: P Integer -> P Column
sumToColDist sumDist = regroup (pmap sumToCol sumDist)

colToTrials :: P Column -> P Trials
colToTrials cols = pmap (\x -> (x:[])) cols

compressTrials :: P (Trials,Trials) -> P Trials
compressTrials trials = regroup (pmap (\(x,y) -> sort (x ++ y)) trials)

trialsToTally :: Trials -> Tally
trialsToTally t = Tally
                  (length (filter (\x -> ((==) LEFT x)) t))
                  (length (filter (\x -> ((==) RIGHT x)) t))
                 
colToNTrials :: P Column -> Int -> P Trials
colToNTrials cols n
    | n == 1 = colToTrials cols
    | otherwise =
        compressTrials (join (colToTrials cols) 
                        (colToNTrials cols (n - 1)))

diePairToTally :: DiePair -> P Tally
diePairToTally pair = 
    let colDist = sumToColDist (sumSpace pair)
        trialsDist = colToNTrials colDist 30
    in 
      pmap trialsToTally trialsDist 

partF :: P Die -> Probability
partF diceDist = 
    let pairDist = dicePairDist diceDist        
        fullJoint = bindx pairDist diePairToTally
    in
        outcomeProb fullJoint (DiePair D4 D4, Tally 27 3)

partG :: P Die -> Double
partG diceDist = 
    let pairDist = dicePairDist diceDist        
        fullJoint = bindx pairDist diePairToTally
        marginalTally = collapseLeft fullJoint
    in
      expected (\(Tally x y) -> realToFrac y) marginalTally
