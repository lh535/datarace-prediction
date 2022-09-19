
module Profiling where

import ReorderNaive
import TraceReorder
import Trace
import Examples


main :: IO ()
main = naiveProf <> reorderProf

naiveProf :: IO ()
naiveProf = do
    print $ {-# SCC "naive" #-}reorderNaivePWR $ addLoc benchmark1
    print $ {-# SCC "naive" #-}reorderNaivePWR $ addLoc benchmark2
    print $ {-# SCC "naive" #-}reorderNaivePWR $ addLoc benchmark4
    print $ {-# SCC "naive" #-}reorderNaivePWR $ addLoc benchmark10

reorderProf :: IO ()
reorderProf = do 
    print $ {-# SCC "recursive" #-}reorder $ addLoc benchmark1
    print $ {-# SCC "recursive" #-}reorder $ addLoc benchmark2
    print $ {-# SCC "recursive" #-}reorder $ addLoc benchmark4
    print $ {-# SCC "recursive" #-}reorder $ addLoc benchmark10