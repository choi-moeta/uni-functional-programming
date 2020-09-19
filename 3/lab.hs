factorial :: Integral a => a -> a
factorial x
  | x == 0    = 1
  | x == 1    = 1
  | otherwise = x * (factorial (x-1))
    
-- recursive subfactorial
-- !n = (n-1)*(!(n-1) + !(n-2))
subfactorial :: Integral a => a -> a
subfactorial x
  | x < 1     = error "Should be natural number"
  | x == 1    = 0
  | x == 2    = 1
  | otherwise = (x-1) * (subfactorial (x-1) + subfactorial (x-2))

-- list comprehension factorial
-- !n = n! * sum(k=0->n) ((-1)^k / k!)
subfactorial' :: Integral a => a -> [a]
subfactorial' x
  | x < 1     = error "Should be natural number"
  | x == 1    = [0]
  | otherwise = [ floor (fromIntegral (product [1..(x-1)] * ((-1)^k)) *
    (1 / fromIntegral (product [1..k]))) | k <- [0..(x-1)] ]
    