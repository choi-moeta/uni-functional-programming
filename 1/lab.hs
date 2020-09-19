-- ! return NaN if equation has no roots i.e. 0x + 3 = 0
-- ! return [] if equation has no real roots i.e. x^2 = -1

discriminant :: Double -> Double -> Double -> Double
discriminant a b c = b * b - 4 * a * c

-- ax + b = 0 -> x = -b/a
linearEquation :: Double -> Double -> IO ()
linearEquation a b = if (a == 0)
    then if (b == 0)
        then print $ 1/0
        else print $ "wrong input" -- string wrong a b
    else if (b == 0)
        then print $ 0.0 -- make sure ouptput is 0 instead of -0
        else print $ ((-b) / a) -- regular solution


-- ax^2 + bx + c = 0 -> x = (-b +- sqrt(discriminant)) / (2*a)
quadraticEquation :: Double -> Double -> Double -> IO ()
quadraticEquation a b c = if (a == 0)
    then (linearEquation b c) -- if a = 0 -> equation become linear
    else if ((discriminant a b c) < 0)
        then print $ "no real roots" -- no real roots
        else if (discriminant a b c == 0)
            then if (b == 0)
                then print $ 0 -- make sure result 0 (not -0)
                else print $ ((-b) / (2 * a)) -- 1 root
            else print $ (((-b) + sqrt(discriminant a b c)) / (2 * a)):(((-b) - sqrt(discriminant a b c)) / (2 * a)):[] -- 2 roots
