module Menu where

import Person
import System.IO
import System.Directory
import Control.Exception
import Data.Typeable

data MenuException
    = FileNotFound
    deriving (Show,Typeable)

instance Exception MenuException

-- Main menu
menu :: [Person] -> [[Char]] -> IO ()
menu persons (fileNameIn:fileNameOut:_) = do
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
    -- putStr " >>> "
    choice <- getLine
    case choice of
        "1" -> do
            persons <- menuAdd persons
            menu persons [fileNameIn,fileNameOut]
        "2" -> do
            persons <- if length persons /= 0
                then menuRemove persons
                else do
                    putStrLn "  +-------------------------+"
                    putStrLn "  |        Empty list       |"
                    putStrLn "  +-------------------------+"
                    return []
            menu persons [fileNameIn,fileNameOut]
        "3" -> do
            persons <- if length persons /= 0
                then menuModify persons
                else do
                    putStrLn "  +-------------------------+"
                    putStrLn "  |        Empty list       |"
                    putStrLn "  +-------------------------+"
                    return []
            menu persons [fileNameIn,fileNameOut]
        "4" -> do
            if length persons /= 0
                then do
                    menuShow persons (length persons)
                else do
                    putStrLn "  +-------------------------+"
                    putStrLn "  |        Empty list       |"
                    putStrLn "  +-------------------------+"
            menu persons [fileNameIn,fileNameOut]
        "5" -> do
            if length persons /= 0
                then do
                    menuAvgList (avgHeightList persons) (length persons)
                else do
                    putStrLn "  +-------------------------+"
                    putStrLn "  |        Empty list       |"
                    putStrLn "  +-------------------------+"
            menu persons [fileNameIn,fileNameOut]
        "6" -> do
            if length persons /= 0
                then do
                    menuMinimumHeight persons
                else do
                    putStrLn "  +-------------------------+"
                    putStrLn "  |        Empty list       |"
                    putStrLn "  +-------------------------+"
            menu persons [fileNameIn,fileNameOut]
        "7" -> do
            if length persons == 0
                then do
                    putStrLn "  +-------------------------+"
                    putStrLn "  |    Nothing to write     |"
                    putStrLn "  +-------------------------+"
                else do
                    fileExist <- doesFileExist fileNameOut
                    if not fileExist
                        then do
                            putStrLn "  +-------------------------+"
                            putStrLn "  | File not found,"
                            putStrLn $ "  | creating file " ++ fileNameOut
                        else undefined
                    writeData persons fileNameOut
                    putStrLn "  +-------------------------+"
                    putStrLn "  |    Succesfully Saved    |"
                    putStrLn "  +-------------------------+"
            menu persons [fileNameIn,fileNameOut]
        "8" -> do
            fileExist <- doesFileExist fileNameIn
            if not fileExist
                then do
                    putStrLn "  +-------------------------+"
                    putStrLn "  |      File not found     |"
                    putStrLn "  +-------------------------+"
                    menu persons [fileNameIn,fileNameOut]
                else do
                    file <- openFile fileNameIn ReadMode
                    content <- hGetContents file
                    if length content == 0
                        then do
                            putStrLn "  +--------------------------"
                            putStrLn "  | File is blank, nothing loaded"
                            putStrLn "  +--------------------------"
                            menu [] [fileNameIn,fileNameOut]
                        else do
                            putStrLn "  +-------------------------+"
                            putStrLn "  |   Succesfully  loaded   |"
                            putStrLn "  +-------------------------+"
                            let persons = readData content
                            menu persons [fileNameIn,fileNameOut]
                    hClose file
        "9" -> do
            putStrLn "  +-------------------------+"
            putStrLn "  |    Succesfully  exit    |"
            putStrLn "  +-------------------------+"
        _ -> do
            putStrLn " <!> Error. Input correct number (1-9)"
            menu persons [fileNameIn,fileNameOut]

-- menu: Get input, ddds person into the list
menuAdd :: [Person] -> IO [Person]
menuAdd persons = do
    putStrLn "  +--------- A D D ---------+"
    putStrLn "  |   Name: "
    name <- getLine
    putStrLn "  |   Surname: "
    surname <- getLine
    putStrLn "  |   Height: "
    line <- getLine
    height <- getHeight line
    putStrLn "  |   Position (start, end, num): "
    pos <- getPos (length persons)

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
menuShow :: [Person] -> Int -> IO ()
menuShow [] _ = do
    putStrLn "  +-------------------------+"
    putStrLn "  |   Error : Empty List    |"
    putStrLn "  +-------------------------+"
menuShow (p:ps) size
    -- | length (p:ps) == size = do
    --     putStrLn ("  +------- S H O W ---------+")
    --     showPerson
    --     putStrLn "  |"
    --     menuShow ps (size + 1)
    | length (p:ps) == 1 = do 
        showPerson
        putStrLn "  |"
        putStrLn "  +-------------------------+"
    | otherwise = do
        showPerson
        putStrLn "  |"
        menuShow ps (size + 1)
    where
        showPerson = do
            if length (p:ps) == size 
                then do
                    putStrLn ("  +------- S H O W ---------+")
                    putStrLn "  |"
                    showFields
                else showFields
        showFields = do 
            putStrLn $ "  |   Name:    " ++ (name p)
            putStrLn $ "  |   Surname: " ++ (surname p)
            putStrLn $ "  |   Height:  " ++ (show (height p))

-- menu: Shows smallest height
menuMinimumHeight :: [Person] -> IO ()
menuMinimumHeight ps = do
    let maxHeight = (maximumHeight ps 0)
    putStrLn "  +-------------------------+"
    putStrLn $ "  | Maximum height is " ++ (show maxHeight)
    putStrLn "  +-------------------------+"

-- menu: Shows list (name: average height) of persons
menuAvgList :: [PersonAvg] -> Int -> IO ()
menuAvgList [] _ = do
    putStrLn "  +-------------------------+"
    putStrLn "  |   Error : Empty List    |"
    putStrLn "  +-------------------------+"

menuAvgList [p] _ = do
    putStrLn $ "  |   Name: " ++ (avgName p)
    putStrLn $ "  |   Avg height: " ++ (show (avgHeight p))
    putStrLn "  +-------------------------+"
menuAvgList (p:ps) size = do
    if (length (p:ps)) == size
        then putStrLn ("  +------- S H O W ---------+")
        else putStr ""
    putStrLn $ "  |   Name: " ++ (avgName p)
    putStrLn $ "  |   Avg height: " ++ (show (avgHeight p))
    putStrLn "  |"
    menuAvgList (ps) size

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
-- writeData :: [Person] -> [Char] -> IO ()
writeData persons fileNameIn = do
    str <- getString persons ""
    writeFile fileNameIn str
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
        _       -> getPosNum line
    where
        getPosNum line = do
            let maybePos = readMaybe line
            case maybePos of
                Nothing -> putStr " <!> Input correct number: " >> getPos size
                Just n  -> if n > size
                    then putStr " <!> Input correct number: " >> getPos size
                    else return n

-- menu-utility: Gets number of nothing
readMaybe :: Read a => [Char] -> Maybe a
readMaybe s = case reads s of
                    [(val, "")] -> Just val
                    _           -> Nothing

                    -- menu-utility: Gets number of nothing
