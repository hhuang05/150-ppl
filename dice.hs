import Infer
import Text.Printf

main = do
  -- Bag of dice
  let bag = [(12, equally [1..4]), (12, equally [1..6]),
             (12, equally [1..8]), (16, equally [0..9]),
             (17, equally [1..12]), (17, equally [1..20])]

  -- Now we turn this representation into a probability dist
  let bagDist = normWeighted bag
  --print bagDist
  let d0 = equally [1..6]
  print d0
  let d1 = equally [1..8]
  print d1
  let o1 = outcomeProb d1 9
  print o1
  let e1 = eventProb (> 11) d0
  print e1
  let exp1 = expected (\x -> x) d0
  print exp1
  let sup = support bagDist
  print sup
  let d2 = pmap (\x -> (x * 3.14)) d0
  print d2
