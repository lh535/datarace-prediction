{-# LANGUAGE GADTs, GeneralisedNewtypeDeriving #-}

-- deprecated! Just using for reference currently

-- Current naming: v/w for vector clock, c for clock dictionary, i/j for threads, y for locks, x for vars

module PWR where

import Control.Monad.State
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Trace
import PrintTrace

---------- data definitions ----------

newtype TStamp = TStamp Int  deriving (Ord, Eq, Num)
newtype VClock = VClock {clock :: Map Thread TStamp}
data Epoch     = Epoch {t ::  Thread, tstamp :: TStamp}
data HistEl    = HistEl {epoch :: Epoch, vclock :: VClock}

-- state for storing global variables of PWR algorithm (oriented from PWR paper)
data PWRGlobal = PWRGlobal { lockS       :: Map Thread (Set Lock),         -- current lockset per thread
                             vClocks     :: Map Thread VClock,             -- current vector clock per thread
                             lastW       :: Map Var VClock,                -- vector clock of last write on var
                             acq         :: Map Lock Epoch,                -- epoch of last aquire for lock
                             hist        :: Map Lock [HistEl]}             -- lock history


-- return type: maps vector clock to every event
newtype EventVC = EventVC {clocks :: Map Event VClock}

---------- PWR ----------

-- run pwr, return map from events to calculated vector clocks
pwr :: [Event] -> EventVC
pwr trace = evalState (foldM (\r e -> case op e of
                                        Acquire y -> do
                                            acquirePWR (thread e) y
                                            s <- get
                                            return $ EventVC (M.insert e (vClocks s M.! thread e) (clocks r))  -- save post-clock

                                        Release y -> do
                                            releasePWR (thread e) y
                                            s <- get
                                            return $ EventVC (M.insert e (vClocks s M.! thread e) (clocks r))

                                        Read x -> do
                                            readPWR (thread e) x
                                            s <- get
                                            return $ EventVC (M.insert e (vClocks s M.! thread e) (clocks r))

                                        Write x -> do
                                            writePWR (thread e) x
                                            s <- get
                                            return $ EventVC (M.insert e (vClocks s M.! thread e) (clocks r))

                                        Fork j -> do
                                            forkPWR (thread e) j
                                            s <- get
                                            return $ EventVC (M.insert e (vClocks s M.! thread e) (clocks r))

                                        Join j -> do
                                            joinPWR (thread e) j
                                            s <- get
                                            return $ EventVC (M.insert e (vClocks s M.! thread e) (clocks r))


                             ) (EventVC M.empty) trace) emptyState

-- compute set of events that are in <-PWR relation from result of PWR, for one event vector clock
relatedEvents :: Event -> VClock -> EventVC -> Set Event
relatedEvents e v c = M.foldlWithKey (\r e' v' -> if v' `vBefore` v then S.insert e' r else r) S.empty (clocks c)

-- compute relatedEvents for every event in result for PWR.
allRelatedEvents :: EventVC -> Map Event (Set Event)
allRelatedEvents c = M.mapWithKey (\e v -> relatedEvents e v c) (clocks c)

-- directly computes set of events in relation, from trace
pwrSet :: [Event] -> Map Event (Set Event)
pwrSet = allRelatedEvents . pwr

---------- functions for every type of event ----------

-- modify state for acquire: sync clocks -> update lockset of thread, update last aquire for lock -> increment
acquirePWR :: Thread -> Lock -> State PWRGlobal ()
acquirePWR i y = do
    incClock i
    applyW3 i
    s <- get
    let newLockS = S.union (lockSFind i (lockS s)) (S.singleton y)
    let newAcq = Epoch i (clock (vClocks s M.! i) M.! i)  -- vClocks is initialized, so access is safe
    put s {lockS = M.insert i newLockS (lockS s),
           acq = M.insert y newAcq (acq s)}

-- modify state for release: sync clocks -> update lockset of thread and history of lock -> increment
releasePWR :: Thread -> Lock -> State PWRGlobal ()
releasePWR i y = do
    incClock i
    applyW3 i
    s <- get
    let newLockS = S.delete y (lockSFind i (lockS s))
    let newHist = HistEl (acq s M.! y) (vClocks s M.! i) : histFind y (hist s)  -- access to acq safe, because acquire happens before release
    put s {lockS = M.insert i newLockS (lockS s),
           hist = M.insert y newHist (hist s)}

-- modify state for write: sync clocks -> change last write -> increment
writePWR :: Thread -> Var -> State PWRGlobal ()
writePWR i x = do
    incClock i
    applyW3 i
    s <- get
    put s {lastW = M.insert x (vClocks s M.! i) (lastW s)}

-- modify state for read (unoptimized): update clock to last write -> sync clocks -> increment
readPWR :: Thread -> Var -> State PWRGlobal ()
readPWR i x = do
    incClock i
    s <- get
    let newClock = (vClocks s M.! i) `vUnion` (lastW s M.! x) -- access to lastW is safe because read can't be before first write
    put s {vClocks = M.insert i newClock (vClocks s)}
    s <- get
    applyW3 i

-- modify state for fork: extend existing clocks by one for new thread -> initalise new clock to clock of caller -> increment
forkPWR :: Thread -> Thread -> State PWRGlobal ()
forkPWR i j = do
    incClock i
    s <- get
    let extendedClocks = forkExtendClocks j (vClocks s)
    let withAddedClocks = forkAddClock j i extendedClocks
    put s {vClocks = withAddedClocks}

--- modify state for join: sync clock of calling thread to joined thread -> increment
joinPWR :: Thread -> Thread -> State PWRGlobal ()
joinPWR i j = do
    incClock i
    s <- get
    let newClock = (vClocks s M.! j) `vUnion` (vClocks s M.! i)
    put s {vClocks = M.insert i newClock (vClocks s)}

---------- PWR helpers ----------

-- look through lock history. If an acquire in history has earlier timestamp, sync current clock with clock in history
-- helper function for w3
syncClock :: VClock -> [HistEl] -> VClock
syncClock = foldr (\(HistEl (Epoch j k) w) v ->
                    if k < clock v M.! j
                    then v `vUnion` w
                    else v)

-- establish ROD (=release order dependency): If e, f in two crit. sections, e < f, then rel < f
--    VClock -> Lockset  -> hist              -> VClock
w3 :: VClock -> Set Lock -> Map Lock [HistEl] -> VClock
w3 v ls h = S.foldr (\l result -> syncClock result (histFind l h)) v ls


-- apply w3 to vector clock of thread
applyW3 :: Thread -> State PWRGlobal ()
applyW3 i = do
    s <- get
    let newClock = w3 (vClocks s M.! i) (lockSFind i (lockS s)) (hist s)
    put s {vClocks = M.insert i newClock (vClocks s)}

-- increment vector clock for thread i by one, in thread i
incClock :: Thread -> State PWRGlobal ()
incClock i = do
    s <- get
    let currClock = vClocks s M.! i
    put s {vClocks = M.insert i (vInc i currClock) (vClocks s)}

-- for calling fork: add thread with timestamp 0 to existing vector clocks
forkExtendClocks :: Thread -> Map Thread VClock -> Map Thread VClock
forkExtendClocks t = M.map (VClock . M.insert t (TStamp 0) . clock)

-- when calling fork: add new clock that is copy of the one from the calling thread, except new cloch has 1 as starting value for own thread
forkAddClock :: Thread -> Thread -> Map Thread VClock -> Map Thread VClock
forkAddClock t call_t c = M.insert t (vInc t (c M.! call_t)) c

---------- Printing ----------

-- markdown printing for PWR - Vector Clocks. Always includes fork/join for now
annotatedWithPWR :: [Event] -> IO ()
annotatedWithPWR = annotTrace (clocks . pwr) (show) "Vector Clocks"

-- markdown printing for PWR - Sets.
annotatedWithPWRSet :: [Event] -> IO ()
annotatedWithPWRSet = annotTraceSet pwrSet "PWR Set"

interactivePWR :: [Event] -> IO ()
interactivePWR = interactiveSet pwrSet "PWR Sets"

interactiveLatexPWR = interactiveLatex (allRelatedEvents . pwr)

instance Show TStamp where
    show (TStamp i) = show i

instance Show VClock where
    show (VClock c) = init $ init $ M.foldrWithKey (\k v r -> show k ++ ": " ++ show v ++ ", " ++ r) "" c

instance Show Epoch where
    show (Epoch j k) = show j ++ "#"  ++ show k

instance Show HistEl where
    show (HistEl e v) = show (e, v)

instance Show EventVC where
    show (EventVC c) = M.foldrWithKey (\k v r -> show k ++ ": " ++ show v ++ "\n" ++ r) "" c

---------- general helper functions ----------

-- initialize vector clock for n threads: time stamp 0 for all threads except current thread i
vInit :: Int -> Int -> VClock
vInit n i = VClock $ M.fromList $ map (\j -> (Thread j, if j == i then TStamp 1 else TStamp 0)) [0..(n-1)]

-- union for vector clocks (take max of each pair of elements)
vUnion :: VClock -> VClock -> VClock
vUnion (VClock v) (VClock w) = VClock $ M.unionWith max v w

-- "<" for vector clocks: <= for each time stamp, and not equal (one timestamp must be strictly smaller)
vBefore :: VClock -> VClock -> Bool
vBefore (VClock v) (VClock w) = M.isSubmapOfBy (<=) v w && not (M.isSubmapOf w v)

-- increment vector clock for one thread by one
vInc :: Thread -> VClock -> VClock
vInc i (VClock v) = VClock $ M.adjust (+ TStamp 1) i v

-- safe lookup for lookS. Returns empty set if key is not in map
lockSFind :: Thread -> Map Thread (Set Lock) -> Set Lock
lockSFind = M.findWithDefault S.empty

-- safe lookup for hist. Returns empty list if key is not in map
histFind :: Lock -> Map Lock [HistEl] -> [HistEl]
histFind = M.findWithDefault []

-- starting state
emptyState :: PWRGlobal
emptyState = PWRGlobal {lockS = M.empty,
                        vClocks = M.fromList [(Thread 0, vInit 1 0)],
                        lastW = M.empty,
                        acq = M.empty,
                        hist = M.empty}