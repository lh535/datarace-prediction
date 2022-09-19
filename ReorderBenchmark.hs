module ReorderBenchmark where

import Criterion.Main

import ReorderNaive
import TraceReorder
import Trace
import Examples



naiveIO :: Trace -> [Int]
naiveIO t = map (length . unTrace) (reorderNaivePWR t)

reorderIO :: Trace -> [Int]
reorderIO t = map (length . unTrace) ( reorder t)

main :: IO ()
main = defaultMain [
    bgroup "naive" [bench "Test Short (length: 5)" $ nf naiveIO benchmark1,
                    bench "Test Write-Read (length: 10)" $ nf naiveIO benchmark2,
                    -- bench "Test 3" $ nfIO $ naiveIO benchmark3,
                    bench "Test Regular (length: 10)" $ nf naiveIO benchmark4,
                    -- bench "Test 5" $ whnfIO $ naiveIO benchmark5,
                    -- bench "Test 6" $ whnfIO $ naiveIO benchmark6,  -- time out 10 mins
                    -- bench "Test 7" $ whnf naiveIO benchmark7,  -- time out
                    -- bench "Test 8" $ whnf naiveIO benchmark8
                    bench "Test NaiveStress (length: 12)" $ nf naiveIO benchmark10
                    ],
    bgroup "backtrack" [bench "Test Short (length: 5)" $ nf reorderIO benchmark1,
                        bench "Test Write-Read (length: 10)" $ nf reorderIO benchmark2,
                        -- bench "Test 3" $ nfIO $ reorderIO benchmark3,
                        bench "Test Regular (length: 10)" $ nf reorderIO benchmark4,
                        -- bench "Test 5" $ nfIO $ reorderIO benchmark5,
                        -- bench "Test 5" $ nf reorderIO benchmark6,
                        bench "Test Lock-Unlock (length: 12)" $ nf reorderIO benchmark7,
                        bench "Test More-Threads (length: 12)" $ nf reorderIO benchmark8,
                        bench "Test NaiveStress (length: 12)" $ nf naiveIO benchmark10,
                        bench "Test BacktrackStress (length: 27)" $ nf reorderIO benchmark9
                    ]]