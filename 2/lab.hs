-- return last element of list
last' :: [a] -> a
last' [x]    = x
last' (x:xs) = last' xs
last' []     = error "Empty list"

-- return list without first element
init' :: [a] -> [a]
init' [x]    = []
init' (x:xs) = x : init' xs
init' []     = error "Empty list"

-- swap the first and the last elements of list
swap :: [a] -> [a]
swap []  = error "Empty list"
swap [x] = [x]
swap x   = (last' x) : (tail (init' x)) ++ (head x) : []

-- swap the last element with the third from the end
swap_last_thirdlast :: [a] -> [a]
swap_last_thirdlast []    = error "Empty list"
swap_last_thirdlast [x]   = [x]
swap_last_thirdlast [x,y] = error "Empty list"
swap_last_thirdlast x     = init' (init' (init' x)) ++ (last' x) : (last' (init' x)) : (last' (init' (init' x))) : []
