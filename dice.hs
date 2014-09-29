import Infer
import Text.Printf

-- Data types specific to solving 'Dice' problems
data Die = D4 | D6 | D8 | D10 | D12 | D20 deriving (Show,Eq)

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
  -- What's the probability that you have drawn two d4's and put 3 marks 
  -- in the right column of the tally sheet

  {-
    We can express this in Bayes rules form: 

P(3 marks| Drawn 2D4's) = 
    [P(Drawn 2D4's | 3 Marks) P (3 Marks)] / P (Drawn 2D4)
    
   -}

  let p4 = outcomeProb diceDist D4
  -- What's the probability of drawing 2 D4's
      prob2d4 = p4**2

  -- Prob of making 3 marks on the right out of 30 possible marks

      {-
      pjoin = join (dieDist D4) (dieDist D4)
      filtered = pfilter (\(x,y) -> (x+y>=8)) pjoin
      poutcome = outcomeProb filtered (4,4)
      30C3 * (poutcome**3) * (1-poutcome)**27
      -}
  print "F: Probability of drawing 2 D4's and making three marks on the right"
  print prob2d4
      
  -- Part G
  -- On average, how many marks can you expect to put in the right column of
  -- the tally sheet

  -- Asking about the expected value of the right column given that we do 
  -- 30 throws with two dice
  -- It's asking Summation over x*p(x), where x = number of marks
  -- Let's calculate the probability of making just one mark
  -- P(1 Mark on right) = Summation over all dice combinations where sum >= 8

  let 
      diceCombo = join diceDist diceDist
                  
      --filter = pfilter (\(x,y) -> (x+y>=8)) diceCombo

  print diceCombo
