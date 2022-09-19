module ReorderBenchmark where

import Criterion.Main

import ReorderNaive
import TraceReorder
import Trace
import Examples


naiveIO :: [Event] -> [[Event]]
naiveIO = reorderNaivePWR . addLoc

reorderIO :: [Event] -> [[Event]]
reorderIO = reorder . addLoc

main :: IO ()
main = defaultMain [
    bgroup "naive" [bench "Test 1" $ whnf naiveIO benchmark1,
                    bench "Test 2" $ whnf naiveIO benchmark2,
                    bench "Test 3" $ whnf naiveIO benchmark3,
                    bench "Test 4" $ whnf naiveIO benchmark4,
                    bench "Test 5" $ whnf naiveIO benchmark5,
                    bench "Test 6" $ whnf naiveIO benchmark6,
                    bench "Test 7" $ whnf naiveIO benchmark7,
                    bench "Test 8" $ whnf naiveIO benchmark8
                    ],
    bgroup "backtrack" [bench "Test 1" $ whnf reorderIO benchmark1,
                        bench "Test 2" $ whnf reorderIO benchmark2,
                        bench "Test 3" $ whnf reorderIO benchmark3,
                        bench "Test 4" $ whnf reorderIO benchmark4,
                        bench "Test 5" $ whnf reorderIO benchmark5,
                        bench "Test 6" $ whnf reorderIO benchmark6,
                        bench "Test 7" $ whnf reorderIO benchmark7,
                        bench "Test 8" $ whnf reorderIO benchmark8,
                        bench "Test 9" $ whnf reorderIO benchmark9
                    ]]