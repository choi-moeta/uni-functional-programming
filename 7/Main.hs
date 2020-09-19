import Menu
import Person
import System.Environment
import Control.Exception
import System.IO.Error 

persons :: [Person]
persons = []

-- main :: [Char] -> [Char] -> IO ()
main = do
    args <- getArgs
    if length args < 2
        then do
            putStrLn " Too few arguments."
            putStrLn " Specify files for input and output."
            putStrLn " Press Enter to exit."
        else do
            let fileNameIn  = args !! 0
            let fileNameOut = args !! 1
            catch (menu persons (fileNameIn:fileNameOut:[])) handler

handler :: IOException -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "Exception: File doesn't exist"
    | otherwise = putStrLn "Undefined exception"
