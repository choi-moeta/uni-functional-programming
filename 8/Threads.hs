module Threads where

import Control.Concurrent
import System.Environment
import Convert

getStrings :: IO [String]
getStrings = do
    params <- getArgs
    if length params == 0
        then do 
            putStrLn "Too few arguments"
            return []
        else return params

thread :: String -> IO (MVar String)
thread str = do
    mvar <- newEmptyMVar
    forkIO (do putMVar mvar (convert str))
    return mvar

showResult :: [String] -> [String] -> IO ()
showResult strings results = do
    putStrLn " <> --- Result --- <>"
    putStrLn " |"
    show' strings results
        where
            show' :: [String] -> [String] -> IO ()
            show' strings results
                | length strings /= length results = error "Unexpected error"
                | length strings /= 0 = do
                    putStrLn $ " | from: \"" ++ head strings ++ "\""
                    putStrLn $ " |   to: \"" ++ head results ++ "\""
                    putStrLn $ " |"
                    show' (tail strings) (tail results)
                | otherwise = putStr " <> -------------- <>"