module TraceReorder where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Debug.Trace

import Trace
import PWR

-- splits trace into a dict with list of events for each thread. Adds to a given map m 
traceToLists' :: [Event] -> Map Thread [Event] -> Map Thread [Event]
traceToLists' [] m = m
traceToLists' (e:es) m = M.insert thr (e:prevList) prevM where
    thr = thread e
    prevM = traceToLists' es m
    prevList = M.findWithDefault [] thr prevM

-- the same as traceToLists', but starting with empty map
traceToLists :: [Event] -> Map Thread [Event]
traceToLists t = traceToLists' t M.empty

-- check if pwr relation is not violated for new addition to a trace. PWR computation of original trace is already given
-- If there is a violation, there needs to be a rewind, because the new event had to have been earlier in the trace.
pwrCheck :: R VClock -> [Event] -> Event -> Bool
pwrCheck (EventMap v) t e = not $ any (\x -> (v M.! e) `before` (v M.! x)) t -- if e < x for any x in t, violation found. rewind.

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


-- for saving state during reordering computations
data ReorderSt = ReorderSt {curTrace :: [Event],          -- current attempt for a trace reordering
                            threadPos :: Map Thread Int,  -- for each thread, points to the index of the event that hasn't been added yet
                            lockSet :: [Lock]}            -- saves which locks are currently in use

-- given a trace, returns all valid trace reorderings as a list of traces (unordered)
reorder :: [Event] -> [[Event]]
reorder t = let
    pwrT = pwr t :: R VClock
    threadEv = traceToLists t
    wrdT = wrdBuild t
    maxThreadLen = M.map length threadEv  -- save lengths of event lists
    threadCount = M.foldr (\x r -> r + 1) 0 threadEv -- count number of threads

    -- see if new event passes all checks, could be valid addition to trace
    tryAdd e curT lockS = case lockCheck lockS e of
            Just newLockS -> if wrdCheck wrdT curT e && pwrCheck pwrT curT e
                                then do Just (curT ++ [e], newLockS)  -- it might be more efficient to start building traces from the end...
                                else Nothing
            Nothing -> Nothing

    removeLock es Event{op=Release y} = y : es
    removeLock es Event{op=Acquire y} = filter (/= y) es
    removeLock es _ = es

    -- removes last event from trace, recreates what the state would have been like if the event wasn't added. Continues normal iteration afterwards
    backtrack :: [[Event]] -> State ReorderSt [[Event]]
    backtrack r = do
        s <- get
        case curTrace s of
          [] -> return r
          evs -> do 
            let evThread = thread $ last evs
            let oldPos = M.adjust (\x -> x - 1) evThread (threadPos s)
            let oldLockS = removeLock (lockSet s) (last evs)
            put s {curTrace = init evs, threadPos = oldPos, lockSet = oldLockS}; iter r (unThread evThread + 1)

    -- adds completed valid reorderings to result
    returnTrace :: [[Event]] -> State ReorderSt [[Event]]
    returnTrace r = do
        s <- get
        backtrack (curTrace s : r)

    -- tries to iteratively add events from lists of events, starting from the beginning. At failure, tries next events, or if there are no options, backtracks
    iter :: [[Event]] -> Int -> State ReorderSt [[Event]]
    iter r i | i == threadCount = backtrack r -- tried to iterate, but out of options -> backtrack
             | otherwise = do
                        s <- get
                        let j = threadPos s M.! Thread i
                        if j < maxThreadLen M.! Thread i
                            then case tryAdd ((threadEv M.! Thread i) !! j) (curTrace s) (lockSet s) of
                                Just (x, lockS) -> do  -- event can be added successfully
                                    let newPos = M.adjust (+1) (Thread i) (threadPos s)
                                    put s {curTrace = x, lockSet = lockS, threadPos = newPos}
                                    s <- get
                                    if threadPos s == maxThreadLen
                                        then returnTrace r
                                        else iter r 0
                                Nothing -> iter r (i+1)  -- adding event failed, try next one
                            else iter r (i+1)  -- for the current thread there are no events left to be added, try next one

    in evalState (iter [] 0) (ReorderSt [] (M.fromList [(Thread i, 0) | i <- [0..threadCount-1]]) [])