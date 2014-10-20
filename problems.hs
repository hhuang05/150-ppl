import Infer
import Dice
import Tally
import Text.Printf

main = do
  
  -- Bag of dice as a literal object
  let bagLiteral = [(D4, 12), (D6, 12), (D8, 12), (D10, 16),
                    (D12, 17), (D20, 17)]                   
      diceDist = normWeighted bagLiteral 

  --printf "Part D: Probability of rolling D6 and D12 and getting a sum of 11: %f\n" (partD diceDist)
  
  printf "Part F: Given drawing 2D4's, what is probability of putting 3 marks on the right column of tally sheet: %f\n" (partF diceDist)

  printf "Part G: Expected value of the right column: %f\n" (partG diceDist)

  --printf "New Question 1: Given we take three dice from the bag, rolling them, we observe a 7, 11 an a multiple of 4. Probability of at least one of the dice is a D8: %f\n" (q1New diceDist)

  
  

