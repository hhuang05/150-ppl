module Tally where
import Infer
import Dice

data Tally = Tally { left :: Integer, right :: Integer } deriving (Show,Eq)
data DiePair = DiePair (Die,Die) deriving (Show)

instance Eq DiePair where 
    DiePair (d1, d2) == DiePair (d3, d4) = (d1 == d3 && d2 == d4) || (d1 == d4 && d2 == d3)

tallyCount :: Tally -> Integer
-- ^ We need the total number of possible tallies for a given
-- spread of left and right
tallyCount (Tally left right) = combo (left+right) right

tallyDist :: Integer -> [(Tally, Integer)]
-- ^ Now using the counts we generate a distribution
-- We'll always start with right = 0 and increase right
tallyDist total = (tallyDistHelper total 0)
    where
      tallyDistHelper :: Integer -> Integer -> [(Tally, Integer)]
      tallyDistHelper 0 right = [((Tally 0 right), 1)]
      tallyDistHelper left right = 
          let t = Tally left right
          in 
            [(t, (tallyCount t))] ++ (tallyDistHelper (left-1) (right+1))

leftMarkDist :: P Die -> P DiePair
-- ^ Generates a distribution of marks, either the left or 
-- the right given two dice
leftMarkDist distDie = 
    regroup (weighted [(DiePair (a,b),
                       twoDieProb distDie a b (\(x,y) -> (x+y<=7)) ) | a <- dice, b <- dice ])
    where 
      dice = support (regroup distDie)

rightMarkDist :: P Die -> P DiePair
-- ^ Generates a distribution of marks, either the left or 
-- the right given two dice
rightMarkDist distDie = 
    regroup (weighted [(DiePair (a,b),
                       twoDieProb distDie a b (\(x,y) -> (x+y>=8)) ) | a <- dice, b <- dice ])
    where 
      dice = support (regroup distDie)
