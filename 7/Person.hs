module Person
(
    Person (..),
    PersonAvg (..),
    add,
    remove,
    modify,
    maximumHeight,
    avgHeightList
) where

import Data.Typeable
import Prelude hiding (Functor, fmap, (<$>), Applicative, pure, (<*>))

data Person = Person {
    name    :: String,
    surname :: String,
    height  :: Int
} deriving Show

p1 = Person {
    name = "artem",
    surname = "sotnichenko",
    height = 175
}
p2 = Person "nikita" "ivanov" 160
p3 = Person "dima" "horin" 170
p4 = Person "dima" "giga" 190

personList :: [Person]
personList = [p1, p2, p3, p4]

-- Adds element into list, i - position,p - element
add :: [Person] -> Int -> Person -> [Person]
add list i elem
    | i <  0           = error "Incorrect index, too small"
    | i > (length list) = error "Incorrect index, too big"
    | i == 0            = elem : list
    | otherwise         = head list : add (tail list) (i - 1) elem

-- Removes element from list, i - position
remove :: [Person] -> Int -> [Person]
remove list i
    | i <  0           = error "Incorrect index"
    | i > (length list) = error "Incorrect index, too big"
    | i == 0            = tail list
    | otherwise         = head list : remove (tail list) (i - 1)

-- Modify element in list, i - position, p - modified person
modify :: [Person] -> Int -> Person -> [Person]
modify list i p
    | i <  0            = error "Incorrect index"
    | i > (length list) = error "Incorrect index, too big"
    | i == 0            = p : (tail list)
    | otherwise         = head list : modify (tail list) (i - 1) p

-- Gets maximum height of people in list from index
maximumHeight :: [Person] -> Int -> Int
maximumHeight [p] i = height p
maximumHeight (p1:p2:ps) i
    | i <  0                = error "Incorrect index, too small"
    | i > length (p1:p2:ps) = error "Incorrect index, too big"
    | i /= 0                = maximumHeight (p2:ps) (i - 1)
    | otherwise             = if (height p1) >= (height p2) then (maximumHeight (p1:ps) 0) else (maximumHeight (p2:ps) 0)

data PersonAvg = PersonAvg {
    avgName    :: String,
    avgHeight  :: Double
} deriving Show

-- Gets list [name: average height]
avgHeightList :: [Person] -> [PersonAvg]
avgHeightList (p:ps)
    | length (remain (p:ps)) /= 0 =
        PersonAvg { avgName = name p,avgHeight = (fromIntegral (sum $ personToHeight $ heights (p:ps)) / fromIntegral (length $ heights (p:ps))) } :(avgHeightList (remain (p:ps)))
    | otherwise =
        PersonAvg { avgName = name p, avgHeight = (fromIntegral (sum $ personToHeight $ heights (p:ps)) / fromIntegral (length $ heights (p:ps)))} : []
    where
        -- filter and map are higher-order functions because they take functions
        -- (in this case lambda-functions) as first argument
        heights (p:ps) = filter (\x -> (name x) == (name p)) (p:ps)
        personToHeight ps = map (\x -> height x) ps
        remain (p:ps) = filter (\x -> (name x) /= (name p)) (p:ps)

-- Additional task: Gets list of peple with min height
minHeight :: [Person] -> [Person]
minHeight list = filter (\x -> (height x) <= minimumHeight list) list
    where
        minimumHeight [p] = height p
        minimumHeight (p1:p2:ps) = if (height p1) >= (height p2) then (minimumHeight (p2:ps)) else (minimumHeight (p1:ps))