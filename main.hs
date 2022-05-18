module Main where
   
import Trace
import PWR 

main :: IO ()
main = do print (pwr (addLoc exampleTrace))