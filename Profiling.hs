
module Profiling where

import ReorderNaive
import TraceReorder
import Trace
import Examples


main :: IO ()
main = naiveProf <> reorderProf

naiveProf :: IO ()
naiveProf = do
    print $ {-# SCC "naive" #-}reorderNaivePWR  benchmark1
    print $ {-# SCC "naive" #-}reorderNaivePWR benchmark2
    print $ {-# SCC "naive" #-}reorderNaivePWR benchmark4
    print $ {-# SCC "naive" #-}reorderNaivePWR benchmark10

reorderProf :: IO ()
reorderProf = do 
    print $ {-# SCC "recursive" #-}reorder benchmark1
    print $ {-# SCC "recursive" #-}reorder benchmark2
    print $ {-# SCC "recursive" #-}reorder  benchmark4
    print $ {-# SCC "recursive" #-}reorder benchmark10