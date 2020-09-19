data Person = Person {
  name    :: String,
  surname :: String,
  height  :: Int
} deriving Show

data PersonAvg = PersonAvg {
  avgName    :: String,
  avgHeight  :: Double
} deriving Show

p1 = Person {
  name = "artem",
  surname = "sotnichenko",
  height = 175
}

p2 = Person "nikita" "ivanov" 180
p3 = Person "dima" "horin" 170
p4 = Person "dima" "giga" 160

list :: [Person]
list = [p1, p2, p3, p4]

add :: [Person] -> Int -> Person -> [Person]
add list i p
  | i <  -1 = error "Incorrect index, too small"
  | i >= (length list) = error "Incorrect index, too big"
  | i == -1 = list ++ [p]
  | i == 0  = p : list
  | otherwise = head list : add (tail list) (i - 1) p

remove :: [Person] -> Int -> [Person]
remove list i
  | i <  -1 = error "Incorrect index"
  | i >= (length list) = error "Incorrect index, too big"
  | i == -1 = init list
  | i == 0  = tail list
  | otherwise = head list : remove (tail list) (i - 1)

modify :: [Person] -> Int -> Person -> [Person]
modify list i p
  | i <  -1 = error "Incorrect index, too small"
  | i >= (length list) = error "Incorrect index, too big"
  | otherwise = add (remove list i) i p

maximumHeight :: [Person] -> Int -> Int
maximumHeight [p] i = height p
maximumHeight (p1:p2:ps) i
  | i <  -1 = error "Incorrect index, too small"
  | i >= length (p1:p2:ps) = error "Incorrect index, too big"
  | i == -1 = maximumHeight (p1:p2:ps) ((length (p1:p2:ps)) - 1)
  | i /= 0 = maximumHeight (p2:ps) (i - 1)
  | otherwise = if (height p1) >= (height p2) then (maximumHeight (p1:ps) 0) else (maximumHeight (p2:ps) 0)

getAvgHeight :: [Person] -> [PersonAvg]
getAvgHeight (p:ps)
  | length (remain (p:ps)) /= 0 =
    PersonAvg { avgName = name p,avgHeight = (fromIntegral (sum $ personToHeight $ heights (p:ps)) / fromIntegral (length $ heights (p:ps))) } :(getAvgHeight (remain (p:ps)))
  | otherwise =
    PersonAvg { avgName = name p, avgHeight = (fromIntegral (sum $ personToHeight $ heights (p:ps)) / fromIntegral (length $ heights (p:ps)))} : []
  where
    -- filter and map are higher-order functions because they take functions
    -- (in this case lambda-functions) as first argument
    heights (p:ps) = filter (\x -> (name x) == (name p)) (p:ps)
    personToHeight ps = map (\x -> height x) ps
    remain (p:ps) = filter (\x -> (name x) /= (name p)) (p:ps)

-- showcase
add' = add list 1 (Person "Artem" "Petrov" 160)
remove' = remove list 3
modify' = modify list (-1) (Person "Alex" "Petrov" 180)
maxHeight' = maximumHeight list 0
avgHeight' = getAvgHeight list
