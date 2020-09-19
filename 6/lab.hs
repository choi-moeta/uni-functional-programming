import Data.IORef
import Data.Typeable
import System.IO

data Person = Person {
	name    :: String,
	surname :: String,
	height  :: Int
} deriving Show

p1 = Person {
	name = "choi",
	surname = "moeta",
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
	| i <  -1 = error "Incorrect index, too small"
	| i > (length list) = error "Incorrect index, too big"
	| i == -1 = list ++ [elem]
	| i == 0  = elem : list
	| otherwise = head list : add (tail list) (i - 1) elem

-- Removes element from list, i - position
remove :: [Person] -> Int -> [Person]
remove list i
	| i <  -1 = error "Incorrect index"
	| i > (length list) = error "Incorrect index, too big"
	| i == -1 = init list
	| i == 0  = tail list
	| otherwise = head list : remove (tail list) (i - 1)

-- Modify element in list, i - position, p - modified person
modify :: [Person] -> Int -> Person -> [Person]
modify list i p
	| i <  -1 = error "Incorrect index"
	| i > (length list) = error "Incorrect index, too big"
	| i == -1 = (init list) ++ [p]
	| i == 0  = p : (tail list)
	| otherwise = head list : modify (tail list) (i - 1) p

-- Gets maximum height of people in list from index
maximumHeight :: [Person] -> Int -> Int
maximumHeight [p] i = height p
maximumHeight (p1:p2:ps) i
	| i <  -1 = error "Incorrect index, too small"
	| i > length (p1:p2:ps) = error "Incorrect index, too big"
	| i == -1 = maximumHeight (p1:p2:ps) ((length (p1:p2:ps)) - 1)
	| i /= 0 = maximumHeight (p2:ps) (i - 1)
	| otherwise = if (height p1) >= (height p2) then (maximumHeight (p1:ps) 0) else (maximumHeight (p2:ps) 0)

data PersonAvg = PersonAvg {
	avgName    :: String,
	avgHeight  :: Double
} deriving Show

-- Gets list [name: average height]
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

-- Additional task: Gets list of peple with min height
minHeight :: [Person] -> [Person]
minHeight list = filter (\x -> (height x) <= minimumHeight list) list
	where
		minimumHeight [p] = height p
		minimumHeight (p1:p2:ps) = if (height p1) >= (height p2) then (minimumHeight (p2:ps)) else (minimumHeight (p1:ps))

-- Main function
main = do
	menu personList

-- Main menu
menu :: [Person] -> IO ()
menu persons = do
	putStrLn "  +-------- M E N U --------+"
	putStrLn "  |                         |"
	putStrLn "  | 1.   Add Person         |"
	putStrLn "  | 2.   Remove Person      |"
	putStrLn "  | 3.   Modify Person      |"
	putStrLn "  | 4.   Show Person        |"
	putStrLn "  | 5.   Get avg list       |"
	putStrLn "  | 6.   Get min height     |"
	putStrLn "  | 7.   Save into file     |"
	putStrLn "  | 8.   Load from file     |"
	putStrLn "  | 9.   Exit               |"
	putStrLn "  |                         |"
	putStrLn "  +-------------------------+"
	putStr " >>> "
	choice <- getLine
	case choice of
		"1" -> do
			persons <- menuAdd persons
			menu persons
		"2" -> do
			persons <- menuRemove persons
			menu persons
		"3" -> do
			persons <- menuModify persons
			menu persons
		"4" -> do
			menuShow persons
			menu persons
		"5" -> do
			menuAvgList $ getAvgHeight persons
			menu persons
		"6" -> do
			let maxHeight = (maximumHeight persons 0)
			putStrLn "  +-------------------------+"
			putStrLn $ "  | Maximum height is " ++ (show maxHeight)
			putStrLn "  +-------------------------+"
			menu persons
		"7" -> do
			writeData persons
			putStrLn "  +-------------------------+"
			putStrLn "  |    Succesfully Saved    |"
			putStrLn "  +-------------------------+"
			menu persons
		"8" -> do
			file <- openFile "save.txt" ReadMode
			content <- hGetContents file
			print $ length content
			if length content == 0
				then do
					putStrLn "  +-------------------------+"
					putStrLn "  |      Error on load      |"
					putStrLn "  +-------------------------+"
				else do
					let persons = readData content
					putStrLn "  +-------------------------+"
					putStrLn "  |   Succesfully  loaded   |"
					putStrLn "  +-------------------------+"
					menu persons
			hClose file
		"9" -> do
			putStrLn "  +-------------------------+"
			putStrLn "  |    Succesfully  exit    |"
			putStrLn "  +-------------------------+"
		_ -> do
			putStrLn " <!> Error. Input correct number (1-8)"
			menu persons

-- menu: Get input, ddds person into the list
menuAdd :: [Person] -> IO [Person]
menuAdd persons = do
	putStrLn "  +--------- A D D ---------+"
	putStr "  |   Name: "
	name <- getLine
	putStr "  |   Surname: "
	surname <- getLine
	putStr "  |   Height: "
	line <- getLine
	height <- getHeight line
	putStr "  |   Position (start, end, num): "
	pos <- getPos (length persons + 1)

	return (add persons pos (Person name surname height))

-- menu: Gets Input, modify person in the list
menuModify :: [Person] -> IO [Person]
menuModify persons = do
	putStrLn "  +------ M O D I F Y ------+"
	putStr "  |   Position (start, end, num): "
	pos <- getPos (length persons)
	let person = persons !! pos

	putStr "  |   Name: "
	line <- getLine
	let pName = if line == "-"
		then (name person)
		else line

	putStr "  |   Surname: "
	line <- getLine
	let pSurname = if line == "-"
		then (surname person)
		else line

	putStr "  |   Height: "
	line <- getLine
	pHeight <- if line == "-"
		then return (height person)
		else getHeight line

	return (modify persons pos (Person pName pSurname pHeight))

-- menu: Gets Input, remove person from the list
menuRemove :: [Person] -> IO [Person]
menuRemove persons = do
	putStrLn "  +------- R E M O V E -------+"
	putStr "  |   Position (start, end, num): "
	pos <- getPos (length persons)
	putStrLn "  +---------------------------+"

	return (remove persons pos)

-- menu: Shows list of persons
menuShow :: [Person] -> IO ()
menuShow [] = do
	putStrLn "  +-------------------------+"
menuShow (p:ps) = do
	let i = length personList - length ps
	if i == 1
		then do
			putStrLn ("  +------- S H O W ---------+")
			putStrLn "  |"
		else putStr ""
	putStrLn $ "  |   Name: " ++ (name p)
	putStrLn $ "  |   Surname: " ++ (surname p)
	putStrLn $ "  |   Height: " ++ (show (height p))
	putStrLn "  |"
	menuShow (ps)

-- menu: Shows list (name: average height) of persons
menuAvgList :: [PersonAvg] -> IO ()
menuAvgList [] = do
	putStrLn "  +-------------------------+"
menuAvgList (p:ps) = do
	let i = length personList - length (p:ps)
	if i == 1
		then putStrLn ("  +------- S H O W ---------+")
		else putStr ""
	putStrLn ("  +-" ++ show (i) ++  "" )
	putStrLn $ "  |   Name: " ++ (avgName p)
	putStrLn $ "  |   Avg height: " ++ (show (avgHeight p))
	menuAvgList (ps)

-- menu: Shows minimum height
getHeight :: [Char] -> IO Int
getHeight line = do
	let maybeHeight = readMaybe line :: Maybe Int
	case maybeHeight of
		Nothing -> do
			putStr " <!> Error. Input correct number: "
			line <- getLine
			getHeight line
		Just n  -> return n

-- menu: Read data from the file
readData :: [Char] -> [Person]
readData content = do
	let string = content
	let listOfStrings = getListOfStrings string
	let listOfFields = getListOfFields listOfStrings
	let listOfPersons = getListOfPersons listOfFields
	result <- listOfPersons
	return result
	where
		getListOfStrings :: [Char] -> [[Char]]
		getListOfStrings string = init $ split string '\n'

		getListOfFields :: [[Char]] -> [[[Char]]]
		getListOfFields [string] = [split string ',']
		getListOfFields listOfStrings = [split (head listOfStrings) ','] ++ getListOfFields (tail listOfStrings)

		getListOfPersons :: [[[Char]]] -> [Person]
		getListOfPersons listOfFields
			| length listOfFields == 1 = [(Person n s h)]
			| otherwise = (Person n s h) : (getListOfPersons $ tail listOfFields)
			where
				person = head listOfFields
				n = person !! 0
				s = person !! 1
				h = read $ person !! 2
		split :: [Char] -> Char -> [[Char]]
		split [] delim = [""]
		split (c:cs) delim
			| c == delim = "" : rest
			| otherwise = (c : head rest) : tail rest
			where
				rest = split cs delim

-- menu: Write data into the file
writeData persons = do
	str <- getString persons ""
	writeFile "save.txt" str
	where
		getString :: [Person] -> [Char] -> IO [Char]
		getString list string = do
			let fieldDivider = ","
			let structDivider = ['\n']
			let person = head list
			let newString = string ++ (name person) ++ fieldDivider
							++ (surname person) ++ fieldDivider
							++ (show $ height person) ++ structDivider
			if length list == 1
				then return newString
				else getString (tail list) newString

-- menu-utility: Gets input of position in list
getPos :: Int -> IO Int
getPos size = do
	line <- getLine
	case line of
		"start" -> return 0 :: IO Int
		"end"   -> return (size - 1) :: IO Int
		_       -> do
			let getPosNum = do
				let maybePos = readMaybe line :: Maybe Int
				case maybePos of
					Nothing -> putStr " <!!> Error. Input correct number: " >> getPos size
					Just n  -> if n > size
						then putStr " <!> Error. Input correct number: " >> getPos size
						else return n
			getPosNum

-- menu-utility: Gets number of nothing
readMaybe :: Read a => [Char] -> Maybe a
readMaybe s = case reads s of
					[(val, "")] -> Just val
					_           -> Nothing