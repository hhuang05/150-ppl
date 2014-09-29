import Infer
import Text.Printf

-- Data types specific to solving 'Dice' problems
data Die = D4 | D6 | D8 | D10 | D12 | D20 deriving (Show,Eq)
data Mark a = Mark Int deriving (Show)
 
dieDist :: Die -> P Integer
dieDist D4 = equally [1..4]
dieDist D6 = equally [1..6]
dieDist D8 = equally [1..8]
dieDist D10 = equally [0..9]
dieDist D12 = equally [1..12]
dieDist D20 = equally [1..20]

main = do
  -- Bag of dice as a literal object
  let bagLiteral = [(D4, 12), (D6, 12), (D8, 12), (D10, 16),
                    (D12, 17), (D20, 17)]                   
  print "The Bag:"
  print bagLiteral

  
  -- Now we turn this representation into a probability dist
  let diceDist = normWeighted bagLiteral
  print "Distribution of Dice:"
  print diceDist
        
  
  -- Part D
  -- Draw two dice, throw them, total the numbers
  -- what's the joint probability of drawing a d6 and d12 and throwing 11

  -- Probability of drawing a d6 and d12 comes from the dist over dice
  -- However, we need the probability distribution of each to calculate
  -- the joint
  let p6 = outcomeProb diceDist D6
      p12 = outcomeProb diceDist D12
      pjoin = join (dieDist D6) (dieDist D12)
      filtered = pfilter (\(x,y) -> (x+y==11)) pjoin
      sumProb = pfoldl (+) 0 filtered
  -- We restrict the sum to be no more than 11
  print "D: Probability of drawing d6 & d12 and rolling sum = 11:" 
  print (2*p6*p12*sumProb)
  
  -- Part F
  -- What's the probability that you have draw 2d4 and put 3 marks on the right
  -- of the tally sheet
  -- P ( (d4,d4) | Mark = 3 ) = 
  -- [ P( Mark = 3 | (d4,d4) ) P( (d4,d4) ) ] / P ( Mark = 3)
  -- For (d4,d4): Sum ( p(Mark=3|(d4,d4))*p(d4,d4) )
  -- Where p(Mark=3|(d4,d4)) = 30C3 * p(Mark=3|(d4,d4))**3 * 
  --                                  p(Mark!=3|(d4,d4))**27
  -- Similar to how we solve part F
      
  -- Let's see how to solve this problem for P(Mark=1)
  let 
      -- Generate all pairings of dice and the joint distribution
      allpairs = [ (die1,die2) | (die1,x) <- bagLiteral, (die2,x) <- bagLiteral]
      outcomesJoint =  map (\(x,y) -> (join (dieDist x) (dieDist y))) allpairs
      markCondPairs = (map (pfoldl (+) 0) 
                          (map (pfilter (\(x,y) -> (x+y>=8))) outcomesJoint))
      markCondDist = Dist (zip 
                           (zip (replicate (length allpairs) (Mark 1)) allpairs)
                           markCondPairs)

      -- Distribution over pairs of dice
      dicepairJoint = join diceDist diceDist
      
      --pairJoin = pmap' (\x -> (x + (log 2))) (join diceDist diceDist) 
      --outProbs = map codeProb (map (outcomeProb pairJoin) (support pairJoin))

      -- Probability of getting 1 mark given all the different drawing of pairs
      -- of dice
      --mark1Joint = Dist (zip allpairs (zipWith (+) probMark outProbs))
  print "F: Probability of drawing 2 D4's and putting 3 marks on the right:" 
--  print allpairs
  print dicepairJoint
  print markCondDist
--  print pairJoin
--  print zipped
  

  -- Part G

  -- On average, how many marks can you expect to put in the right column of
  -- the tally sheet

  -- Asking about the expected value of the right column given that we do 
  -- 30 throws with two dice
  -- It's asking Summation over x*p(x), where x = number of marks
  -- Let's calculate the probability of making just one mark
  -- P(1 Mark on right) = Summation over all dice combinations where sum >= 8
  -- E.g. for (d4,d4): Sum ( p(Mark=1|(d4,d4))*p(d4,d4) )
  -- Where p(Mark=1|(d4,d4)) = 30C1*p(Mark=1|(d4,d4))*p(Mark!=1|(d4,d4))**29
  -- Similar to how we solve part F


  --let 
      --diceCombo = join diceDist diceDist
                  
      --filter = pfilter (\(x,y) -> (x+y>=8)) diceCombo

  --print diceCombo

