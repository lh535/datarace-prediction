{-# LANGUAGE GADTs, GeneralisedNewtypeDeriving #-}

module PWR where

import Control.Monad.State
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M  -- use IntMap?
import qualified Data.Set as S

import Trace

---------- data definitions ----------

newtype TStamp = TStamp Int  deriving (Ord, Eq, Show, Num)  -- would just using type be good here too?
newtype VClock = VClock {clock :: Map Thread TStamp} deriving (Show) -- idea: use intmap, vector, mutable vector?
data Epoch     = Epoch {id ::  Thread, tstamp :: TStamp}
data HistEl    = HistEl {epoch :: Epoch, vclock :: VClock}

-- pretty direct copy of global variables for PWR algorithm from paper 
-- missing: "edges", "conc", "evt" are unneeded for simple PWR
data PWRGlobal = PWRGlobal { lockS   :: Map Thread (Set Lock),  -- current lockset per thread
                             vClocks :: Map Thread VClock,      -- current vector clock per thread
                             lastW_C :: Map Var VClock,         -- vector clock of last write on var
                             lastW_T :: Map Var Thread,         -- threads with last write on var
                             lastW_L :: Map Var (Set Lock),     -- lockset of last write on var
                             acq     :: Map Lock Epoch,         -- epoch of last aquire for lock
                             hist    :: Map Lock [HistEl],      -- lock history
                             rw      :: Map Var (Set Event) }


---------- helper functions ----------

-- initialize vector clock for n threads: time stamp 0 for all threads
-- NOTE: this means we need to pass the number of threads to the algorithm. Is it necessary to work with initialized clocks?
vInit :: Int -> VClock
vInit n = VClock $ M.fromList $ map (\i -> (Thread i, TStamp 0)) [0..(n-1)]

-- union for vector clocks (take max of each pair of elements)
vUnion :: VClock -> VClock -> VClock
vUnion (VClock v) (VClock w) = VClock $ M.unionWith max v w

-- "<" for vector clocks: <= for each time stamp, and not equal (one timestamp must be strictly smaller)
vBefore :: VClock -> VClock -> Bool
vBefore (VClock v) (VClock w) = M.isSubmapOfBy (<=) v w && not (M.isSubmapOf v w)

-- increment vector clock for one thread by one
vInc :: Thread -> VClock -> VClock
vInc i (VClock v) = VClock $ M.adjust (+ TStamp 1) i v

---------- PWR ----------

pwr :: [Event] -> Map Var (Set Event)
pwr = undefined

---------- functions for every type of event ----------

-- modify state for acquire event: 
-- sync clock -> add lock to lockset of thread -> update last aquire epoch -> increment thread clock
acquire :: Thread -> Lock -> State PWRGlobal ()
acquire i y = do
    s <- get
    applyW3 i
    let newLockS = S.union (lockS s M.! i) (S.singleton y)
    let newAcq = Epoch i (clock (vClocks s M.! i) M.! i)
    put s {lockS = M.insert i newLockS (lockS s),
           acq = M.insert y newAcq (acq s)}
    incClock i

-- modify state for release event:
-- sync clock -> remove lock from lockset of thread -> update lock history with prev acquire -> increment thread clock
release :: Thread -> Lock -> State PWRGlobal ()
release i y = do
    s <- get
    applyW3 i
    let newLockS = S.delete y (lockS s M.! i)
    let newHist = HistEl (acq s M.! y) (vClocks s M.! i) : (hist s M.! y)
    put s {lockS = M.insert i newLockS (lockS s),
           hist = undefined}
    incClock i

-- modify state for write event:
-- sync clock -> set concurrent read/writes -> save last write information -> increment thread clock (?)
write :: Thread -> Lock -> State PWRGlobal ()
write = undefined

-- modify state for read event:
-- set concurrent read/writes -> update clock to last write -> sync clock -> set concurrrent read/writes -> increment thread clock (?)
read :: Thread -> Lock -> State PWRGlobal ()
read = undefined

---------- PWR helpers ----------

-- look through lock history. If an acquire in history has earlier timestamp, sync current clock with clock in history
-- helper function for w3
syncClock :: VClock -> [HistEl] -> VClock
syncClock = foldr (\(HistEl (Epoch j k) v') v ->
                    if k < clock v M.! j
                    then vUnion v v'
                    else v)

-- establish ROD (=release order dependency): If e, f in two crit. sections, e < f, then rel < f
--    VClock -> Lockset  -> hist              -> VClock
w3 :: VClock -> Set Lock -> Map Lock [HistEl] -> VClock
w3 v ls h = S.foldr (\l result -> syncClock result (h M.! l)) v ls


-- can these be made pure...?
-- apply w3 to vector clock of thread
applyW3 :: Thread -> State PWRGlobal VClock
applyW3 i = do
    s <- get
    let newClock = w3 (vClocks s M.! i) (lockS s M.! i) (hist s)
    put s {vClocks = M.insert i newClock (vClocks s)}
    return newClock

-- increment vector clock for thread i by one, in thread i
incClock :: Thread -> State PWRGlobal ()
incClock i = do
    s <- get
    let currClock = vClocks s M.! i
    put s {vClocks = M.insert i (vInc i currClock) (vClocks s)}


---------- Tests ----------

-- Didn't think of good values, just put in something to see if it works at all
syncClockTest :: VClock
syncClockTest = let v1 = VClock $ M.fromList [(Thread 0, TStamp 0), (Thread 1, TStamp 0)]
                    v2 = VClock $ M.fromList [(Thread 0, TStamp 3), (Thread 1, TStamp 2)]
                    v3 = VClock $ M.fromList [(Thread 0, TStamp 2), (Thread 1, TStamp 10)]
                    e1 = Epoch (Thread 0) (TStamp 3)
                    e2 = Epoch (Thread 1) (TStamp 2)
                    h = [HistEl e1 v1, HistEl e2 v2]
                 in syncClock v3 h
