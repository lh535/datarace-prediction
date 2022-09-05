module TraceReorder where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State

import Trace
import PWR

-- splits trace into a dict with list of events for each thread. Adds to a given map m 
-- TODO: rewrite to go ...
traceToLists' :: [Event] -> Map Thread [Event] -> Map Thread [Event]
traceToLists' [] m = m
traceToLists' (e:es) m = M.insert thr (e:prevList) prevM where
    thr = thread e
    prevM = traceToLists' es m
    prevList = M.findWithDefault [] thr prevM

traceToLists :: [Event] -> Map Thread [Event]
traceToLists t = traceToLists' t M.empty

-- check if pwr relation is not violated for new addition to a trace. PWR computation of original trace is already given
-- If there is a violation, there needs to be a rewind, because the new event had to have been earlier in the trace.
pwrCheck :: R VClock -> [Event] -> Event -> Bool
pwrCheck (EventMap v) t e = any (\x -> (v M.! e) `before` (v M.! x)) t -- if e < x for any x in t, violation found. rewind.

-- builds map of wrd relations from a trace: every key is a read, and the value is the corresponding last write.
wrdBuild :: [Event] -> Map Event Event
wrdBuild t = let
    go r lastWrites [] = r
    go r lastWrites (e@Event{op=Write x}:es) = go r (M.insert x e lastWrites) es
    go r lastWrites (e@Event{op=Read x}:es) = go (M.insert e (lastWrites M.! x) r) lastWrites es
    go r lastWrites (_:es) = go r lastWrites es
    in go M.empty M.empty t

-- given map that specifies wrd relation, current trace fragment and new event to be added, checks if wrd is not violated
wrdCheck :: Map Event Event -> [Event] -> Event -> Bool
wrdCheck wrds t e@Event{op=Read x} = case e `M.lookup` wrds of
                                        Nothing -> False
                                        Just w  -> w == findLastWrite (reverse t)
    where findLastWrite (w@Event{op=Write y}:es) | y == x = w
          findLastWrite (_:es)                            = findLastWrite es
          findLastWrite []                                = e  -- stands for anything that is guranteed != write
wrdCheck wrds t _ = True

-- check if lock semantics are not violated for new addition to trace. Current locks which are acquired are already given.
-- returns Just (new list of locks) if no violation, Nothing if there is a violation
lockCheck :: [Lock] -> Event -> Maybe [Lock]
lockCheck ls Event{op=(Acquire y)} = if y `elem` ls then Nothing else Just $ y:ls
lockCheck ls Event{op=(Release y)} = if y `elem` ls then Just $ filter (/= y) ls else Nothing
lockCheck ls _ = Just ls


data ReorderSt = ReorderSt {threadPos :: [Integer],
                            threadNum :: Integer,
                            lockSet :: [Lock]}

reorder :: [Event] -> [[Event]]
reorder t = let
    pwrT = pwr t :: R VClock
    threadEv = traceToLists t
    wrdT = wrdBuild t
    maxThreadLen = M.foldr (\x r -> length x : r) [] threadEv  -- save lengths of event lists
    threadCount = M.foldr (\x r -> r + 1) 0 threadEv -- count number of threads

    tryAdd e curT = do
        s <- get
        case lockCheck (lockSet s) e of
            Just x -> if wrdCheck wrdT curT e && pwrCheck pwrT curT e then do
                            put s {lockSet = x}
                            return Just e : curT
                      else Nothing
            Nothing -> Nothing
    
    iterOne e curT = undefined

    in evalState
    (do

    )
    (ReorderSt [0 | i <- [1..threadCount]] 0 [])

