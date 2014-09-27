import Infer
import Text.Printf

-- Data types specific to solving 'Dice' problems
data Die = D4 | D6 | D8 | D10 | D12 | D20 deriving (Show)

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
  print bagLiteral
  
  -- Now we turn this representation into a probability dist
  let bagDist = normWeighted bagLiteral
  print bagDist
  {-
  let bag = [(equally [1..4], 12), (equally [1..6], 12),
             (equally [1..8], 12), (equally [0..9], 16),
             (equally [1..12], 17), (equally [1..20], 17)]
             -}

  let d0 = equally [1..6]
  print d0
  let d1 = equally [1..8]
  print d1
  let o1 = outcomeProb d1 7
  print o1
  let e1 = eventProb (<= 4) d0
  print e1

  let exp1 = expected (\x -> x) d0
  print exp1
  --let sup = support bagDist
  --print sup
  let d2 = pmap (\x -> (x * 3.14)) d0
  print d2
  let d3 = pfilter (\x -> (x <= 7)) d1
  print d3
  let b1 = bindx bagDist dieDist
  print b1


