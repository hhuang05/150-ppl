import Infer
import Dice
import Text.Printf

main = do
  
  -- Bag of dice as a literal object
  let bagLiteral = [(D4, 12), (D6, 12), (D8, 12), (D10, 16),
                 (D12, 17), (D20, 17)]                   
      diceDist = normWeighted bagLiteral 
  
  printf "Part D: Probability of rolling D6 and D12 and getting a sum of 11: %f\n" (partD diceDist)


  {- 
     Using pfilter, solve this problem: you draw three dice from the standard bag, throw them, and you see a 7, an 11, and a multiple of 4.  What is the probability that at least one of the dice is a d8? 
   -}
  let diceJoint = diceTripleDist diceDist
      fullJoint = bindx diceJoint 
                  (\x ->(condThrowSet (dieTripleToThrowHelper x)))

      -- P(Seeing 7 and 11 and x4)
      pCondition = decodeProb (plogfoldl (+) 0 fullJoint)

      -- P(At least 1 Die = 8)      
      distD8 = pfilter isD8 diceJoint    
      pAtLeast8 = decodeProb (plogfoldl (+) 0 distD8)
      a = (pmap' decodeProb diceJoint)
      -- P(Seeing 7 and 11 and x4|At least 1 Die = 8)
      --distAtLeast8 = pfilter isD8 (regroup (collapseRight fullJoint))
      --pCondGiven8 = decodeProb (plogfoldl (+) 0 distAtLeast8)

  print a
  print (pfoldl (+) 0 a)
  print (pfoldl (+) 0 (pmap' decodeProb distD8))
  --print distAtLeast8
  --printf "P(A|B): %f\n" pCondGiven8
  printf "P(B): %f\n" pAtLeast8
  --printf "P(A): %f\n" pCondition
  --printf "Final Answer: %f\n" ((pCondGiven8 * pAtLeast8) / pCondition)
