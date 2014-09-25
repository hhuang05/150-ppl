import Infer
import Text.Printf


main = do
  let d1 = equally [1..6]
  print d1
  let d2 = certainly 2
  print d2
  let d3 = weightedly (zip [1..6] [0.5, 0.2, 0.1, 0.1, 0.1])
  print d3
