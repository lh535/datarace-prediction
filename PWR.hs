{-# LANGUAGE GADTs, GeneralisedNewtypeDeriving, FlexibleInstances #-}

-- Current naming: v/w for vector clock, c for clock dictionary, i/j for threads, y for locks, x for vars

module PWR where

import Control.Monad.State
import Data.Set (Set)
import Data.Map (Map)
import Examples
import qualified Data.Map as M
import qualified Data.Set as S

import Trace
import PrintTrace

---------- data definitions ----------

newtype TStamp = TStamp Int  deriving (Ord, Eq, Num)
newtype VClock = VClock {clock :: Map Thread TStamp}
data HistEl a  = PWRType a => HistEl {acquire :: a, release :: a}

-- state for storing global variables of PWR algorithm (oriented from PWR paper)
data PWRGlobal a = PWRType a => PWRGlobal { lockS   :: Map Thread (Set Lock),    -- current lockset per thread
                               tStates :: Map Thread a,                          -- current state per thread
                               lastW   :: Map Var a,                             -- vector clock of last write on var
                               acq     :: Map Lock a,                            -- state at last aquire
                               hist    :: Map Lock [HistEl a]}                   -- lock history


-- return type: maps vector clock to every event
data R a = PWRType a => EventMap {eventMap :: Map Event a}

---------- PWR ----------

-- run pwr, return map from events to calculated vector clocks
pwr :: PWRType a => Trace -> R a
pwr (Trace trace) = evalState (foldM (\r e -> case op e of
                                        Acquire y -> do
                                            acquirePWR e y
                                            s <- get
                                            return $ rAdd r e (tStates s M.! thread e)  -- save post-clock

                                        Release y -> do
                                            releasePWR e y
                                            s <- get
                                            return $ rAdd r e (tStates s M.! thread e)

                                        Read x -> do
                                            readPWR e x
                                            s <- get
                                            return $ rAdd r e (tStates s M.! thread e)

                                        Write x -> do
                                            writePWR e x
                                            s <- get
                                            return $ rAdd r e (tStates s M.! thread e)

                                        Fork j -> do
                                            forkPWR e j
                                            s <- get
                                            return $ rAdd r e (tStates s M.! thread e)

                                        Join j -> do
                                            joinPWR e j
                                            s <- get
                                            return $ rAdd r e (tStates s M.! thread e)

                             ) (EventMap M.empty) trace) emptyState
            where rAdd r e v = EventMap (M.insert e v (eventMap r))

pwrClock :: Trace -> R VClock
pwrClock = pwr

pwrSet :: Trace -> R (Set Event)
pwrSet = pwr

---------- functions for every type of event ----------

-- modify state for acquire: sync clocks -> update lockset of thread, update last aquire for lock -> increment
acquirePWR :: PWRType a => Event -> Lock -> State (PWRGlobal a) ()
acquirePWR e y = do
    let i = thread e
    s <- get
    let newStates = if M.member i (tStates s) then tStates s else M.insert i startState (tStates s)
    put s {tStates = newStates}
    incState e
    applyW3 i
    s <- get
    let newLockS = S.union (lockSFind i (lockS s)) (S.singleton y)
    let newAcq = tStates s M.! i  -- tStates is initialized, so access is safe
    put s {lockS = M.insert i newLockS (lockS s),
           acq = M.insert y newAcq (acq s)}

-- modify state for release: sync clocks -> update lockset of thread and history of lock -> increment
releasePWR :: PWRType a => Event -> Lock -> State (PWRGlobal a) ()
releasePWR e y = do
    let i = thread e
    s <- get
    let newStates = if M.member i (tStates s) then tStates s else M.insert i startState (tStates s)
    put s {tStates = newStates}
    incState e
    applyW3 i
    s <- get
    let newLockS = S.delete y (lockSFind i (lockS s))
    let newHist = HistEl (acq s M.! y) (tStates s M.! i) : histFind y (hist s)  -- access to acq safe, because acquire happens before release
    put s {lockS = M.insert i newLockS (lockS s),
           hist = M.insert y newHist (hist s)}

-- modify state for write: sync clocks -> change last write -> increment
writePWR :: PWRType a => Event -> Var -> State (PWRGlobal a) ()
writePWR e x = do
    let i = thread e
    s <- get
    let newStates = if M.member i (tStates s) then tStates s else M.insert i startState (tStates s)
    put s {tStates = newStates}
    s <- get
    incState e
    applyW3 i
    s <- get
    put s {lastW = M.insert x (tStates s M.! i) (lastW s)}

-- modify state for read (unoptimized): update clock to last write -> sync clocks -> increment
readPWR :: PWRType a => Event -> Var -> State (PWRGlobal a) ()
readPWR e x = do
    let i = thread e
    s <- get
    let newStates = if M.member i (tStates s) then tStates s else M.insert i startState (tStates s)
    put s {tStates = newStates}
    incState e
    s <- get
    let newClock = (tStates s M.! i) `union` (lastW s M.! x) -- access to lastW is safe because read can't be before first write
    put s {tStates = M.insert i newClock (tStates s)}
    s <- get
    applyW3 i

-- modify state for fork: extend existing clocks by one for new thread -> initalise new clock to clock of caller -> increment
forkPWR :: PWRType a => Event -> Thread -> State (PWRGlobal a) ()
forkPWR e j = do
    let i = thread e
    s <- get
    let newStates = if M.member i (tStates s) then tStates s else M.insert i startState (tStates s)
    put s {tStates = newStates}
    incState e
    s <- get
    let extendedClocks = extend j (tStates s)
    let withAddedClocks = forkAdd j i extendedClocks
    put s {tStates = withAddedClocks}

--- modify state for join: sync clock of calling thread to joined thread -> increment
joinPWR :: PWRType a => Event -> Thread -> State (PWRGlobal a) ()
joinPWR e j = do
    let i = thread e
    s <- get
    let newStates' = if M.member j (tStates s) then tStates s else M.insert j startState (tStates s)
    let newStates = if M.member i newStates' then newStates' else M.insert i startState newStates'
    put s {tStates = newStates}
    incState e
    s <- get
    let newClock = (tStates s M.! j) `union` (tStates s M.! i)
    put s {tStates = M.insert i newClock (tStates s)}

---------- PWR helpers ----------

-- look through lock history. If an acquire in history is earlier, sync current status of thread with status in history
-- helper function for w3
syncOne :: PWRType a => a -> [HistEl a] -> a
syncOne = foldr (\(HistEl acq rel) x ->
                    if acq `before` x
                    -- if k < clock x M.! j
                    then x `union` rel
                    else x)

-- establish ROD (=release order dependency): If e, f in two crit. sections, e < f, then rel < f
w3 :: PWRType a => a -> Set Lock -> Map Lock [HistEl a] -> a
w3 v ls h = S.foldr (\l result -> syncOne result (histFind l h)) v ls


-- apply w3 to vector clock of thread
applyW3 :: PWRType a => Thread -> State (PWRGlobal a) ()
applyW3 i = do
    s <- get
    let newClock = w3 (tStates s M.! i) (lockSFind i (lockS s)) (hist s)
    put s {tStates = M.insert i newClock (tStates s)}

-- increment state for thread i, in thread i
incState :: PWRType a => Event -> State (PWRGlobal a) ()
incState e = do
    let i = thread e
    s <- get
    let currClock = tStates s M.! i
    put s {tStates = M.insert i (inc e currClock) (tStates s)}

-- when calling fork: add new entry in map for new thread, which is copy of state of calling thread
forkAdd :: PWRType a => Thread -> Thread -> Map Thread a -> Map Thread a
forkAdd t call_t c = M.insert t (c M.! call_t) c

---------- Printing ----------

-- markdown printing for PWR - Vector Clocks. Always includes fork/join for now
annotatedWithPWR :: Trace -> IO ()
annotatedWithPWR = annotTrace (eventMap . pwrClock) show "Vector Clocks"

-- markdown printing for PWR - Sets.
annotatedWithPWRSet :: Trace -> IO ()
annotatedWithPWRSet = annotTraceSet (eventMap . pwr) "PWR Set"

interactivePWR :: Trace -> IO ()
interactivePWR = interactiveSet (eventMap . pwr) "PWR Sets"

interactiveLatexPWR :: Trace -> IO ()
interactiveLatexPWR = interactiveLatex (eventMap . pwr)

instance Show TStamp where
    show (TStamp i) = show i

instance Show VClock where
    show (VClock c) = init $ init $ M.foldrWithKey (\k v r -> show k ++ ": " ++ show v ++ ", " ++ r) "" c

---------- general helper functions ----------

-- safe lookup for lookS. Returns empty set if key is not in map
lockSFind :: Thread -> Map Thread (Set Lock) -> Set Lock
lockSFind = M.findWithDefault S.empty

-- safe lookup for hist. Returns empty list if key is not in map
histFind :: Lock -> Map Lock [HistEl a] -> [HistEl a]
histFind = M.findWithDefault []

-- starting state
emptyState :: PWRType a => PWRGlobal a
emptyState = PWRGlobal {lockS = M.empty,
                        tStates = M.fromList [(Thread 0, startState)],
                        lastW = M.empty,
                        acq = M.empty,
                        hist = M.empty}


-- definitions for PWRType

class PWRType a where
    startState :: a
    inc :: Event -> a -> a
    union :: a -> a -> a
    before :: a -> a -> Bool
    extend :: Thread -> Map Thread a -> Map Thread a


instance PWRType VClock where
    startState = VClock $ M.fromList [(Thread 0, TStamp 1)]
    inc e (VClock v) = VClock $ M.adjust (+ TStamp 1) (thread e) v
    union (VClock v) (VClock w) = VClock $ M.unionWith max v w
    before (VClock v) (VClock w) = M.isSubmapOfBy (<=) v w && not (M.isSubmapOf w v)
    extend t = M.map (VClock . M.insert t (TStamp 0) . clock)



instance PWRType (Set Event) where
    startState = S.empty
    inc e s = S.insert e s
    union s1 s2 = S.union s1 s2
    before s1 s2 = S.isSubsetOf s1 s2
    extend t m = m

