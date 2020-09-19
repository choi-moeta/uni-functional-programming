module Main where

import Control.Concurrent
import Threads

main :: IO ()
main = do
    strings <- getStrings
    if length strings /=0
        then do
            childs  <- mapM thread strings -- store MVars of threads to show only after all threads are done.
            results <- mapM takeMVar childs
            showResult strings results
        else return ()