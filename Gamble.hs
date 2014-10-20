module Gamble where
import Infer
import Dice
import Tally

probGoodGuess :: Tally -> P (DiePair,Tally) -> Probability
probGoodGuess t joint =
    let tallyProb = codeProb (outcomeProb (collapseLeft joint) t)
        eventDist = pmap' (\x -> (x - tallyProb)) 
                    (pfilter (\(x,y) -> ((==) t y)) joint)
    in 
      sum (map snd (take 3 (byLikelihood (collapseRight eventDist))))
    

partH :: P Die -> Probability
partH diceDist = 
    let pairDist = dicePairDist diceDist        
        fullJoint = bindx pairDist diePairToTally 
    in
      probGoodGuess (Tally 27 3) fullJoint
