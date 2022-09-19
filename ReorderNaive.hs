
module ReorderNaive where -- formerly just "Reorder"

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Trace
import Examples

import qualified PWR as P


-- Build all WRDs, write-read dependencies.
-- Each WRD represented as a pair (write,read).
wrds :: [Event] -> [(Event, Event)]
wrds trace =
  let var :: Event -> Var
      var e = case op e of
                (Read x) -> x
                (Write x) -> x

      -- Maintain history of writes to check for last write.
      go writeHist acc [] = acc
      go writeHist acc (e:es)
         | isWrite (op e) = go (e:writeHist) acc es
         | isRead (op e)  = case filter (\f -> var e == var f) writeHist of
                               (w:_) -> go writeHist ((w,e):acc) es
                               [] -> go writeHist acc es
                                     -- Read has no (last) write.
                                     -- Such traces shall be discarded.
         | otherwise = go writeHist acc es

  in go [] [] trace


isWrite (Write _) = True
isWrite _ = False

isRead (Read _) = True
isRead _ = False

isRelease (Release _) = True
isRelease _ = False

-- Check that trace respects lock semantics.
-- For each rel(x) there's a preceding acq(x), and
-- there are no two consecutive acq(x)'s without rel(x) in between.
lockSemantics :: [Event] -> Bool
lockSemantics trace =
  let lock :: Event -> Lock
      lock e = case op e of
                  (Acquire x) -> x
                  (Release x) -> x

      go activeLocks [] = S.null activeLocks
      go activeLocks (e:es)
         | isAcquire (op e) = not (S.member (lock e) activeLocks)
                              && go (S.insert (lock e) activeLocks) es
         | isRelease (op e) = S.member (lock e) activeLocks
                              && go (S.delete (lock e) activeLocks) es
         | otherwise = go activeLocks es

  in go S.empty trace

-- Lookup dots = set of events.
-- If k entry not present yet, yields empty set.
dots k m = fromMaybe S.empty (M.lookup k m)

 -- PWR is complete.
 -- Hence, any valid trace reordering will respect the PWR relation.
 -- This suggest the following naive strategy to generate reorderings.
-- 1. Compute all permutations.
-- 2. Consider only permutations under which WRDs remain stable.
-- 3. Keep all permuted traces for which the PWR relation remains stable (i.e. the same).
reorderNaivePWR :: Trace ->  [Trace]
reorderNaivePWR t =
     let dotsPWR t = P.eventMap $ P.pwrSet t
         trace = unTrace t
     in [ Trace trace' | trace' <- permutations trace,
                   lockSemantics trace',
                   sort (wrds trace) == sort (wrds trace'), -- must sort, order of WRD pairs may change
                   all (\e -> dots e (dotsPWR (Trace trace)) == dots e (dotsPWR (Trace trace'))) trace ]
