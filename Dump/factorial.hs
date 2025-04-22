import Debug.Trace

fact1 0 = 1
fact1 n = n * fact1(n-1)

fact2 n = if n == 0 then 1 else n * fact2 (n-1)

fact3 n = case n of
  0 -> 1
  _ -> n * fact3 (n-1)

fact4 n
    | n == 0 = 1
    | otherwise = n * fact4 (n-1)

fact5 :: Int -> Int
fact5 n = product [1..n]

fact6 = \n -> product [1..n]

fact7 n = foldl (*) 1 [1..n]

fact8 n = foldr (*) 1 [1..n]

fact9 n = fact9' n 1
  where
    fact9' 0 acc = acc
    fact9' n acc = fact9' (n-1) (n*acc)

fact10 n = 
  let fact10' 0 = 1
      fact10' m = m * fact10' (m-1)
  in fact10' n

fact11 n =
  let fact11' 0 acc = acc
      fact11' m acc = fact11' (m-1) (m*acc)
  in fact11' n 1

test = trace "Evaluated" 1

main = do
  print ("Hello World")
  putStrLn "Hello World 2"

  n <- getLine
  print ("Original: " ++ n)
  let u = read n :: Int
  let test = test
  print u