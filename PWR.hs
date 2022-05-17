{-# LANGUAGE GADTs #-}

module PWR where

import Control.Monad.State
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M  -- use IntMap?
import qualified Data.Set as S

import Trace

---------- data definitions ----------
newtype TStamp = TStamp Int  deriving (Ord, Eq, Show)  -- would just using type be good here too?
newtype VClock = VClock {clock :: Map Thread TStamp} deriving (Show) -- idea: use intmap, vector, mutable vector?
data Epoch     = Epoch {id ::  Thread, tstamp :: TStamp}
data HistEl    = HistEl {epoch :: Epoch, vclock :: VClock}

-- pretty direct copy of global variables for PWR algorithm from paper 
-- missing: "edges", "conc", "evt" are unneeded for simple PWR; "RW" is the result of PWR function
data PWRGlobal = PWRGlobal { lockS   :: Map Thread (Set Lock),  -- current lockset per thread
                             vclocks :: Map Thread VClock,      -- current vector clock per thread
                             lastW_C :: Map Var VClock,         -- vector clock of last write on var
                             lastW_T :: Map Var Thread,         -- threads with last write on var
                             lastW_L :: Map Var (Set Lock),     -- lockset of last write on var
                             acq     :: Map Lock Epoch,         -- epoch of last aquire for lock
                             hist    :: Map Lock [HistEl] }     -- lock history


newtype PWRState = State PWRGlobal
newtype RW = RW (Map Var (Set Event))  -- result type of computation

---------- helper functions ----------

-- initialize vector clock for n threads: time stamp 0 for all threads
vInit :: Int -> VClock
vInit n = VClock $ M.fromList $ map (\i -> (Thread i, TStamp 0)) [0..(n-1)]

-- union for vector clocks (take max of each pair of elements)
vUnion :: VClock -> VClock -> VClock
vUnion (VClock v) (VClock w) = VClock $ M.unionWith max v w

-- "<" for vector clocks: <= for each time stamp, and not equal (one timestamp must be strictly smaller)
tBefore :: VClock -> VClock -> Bool
tBefore (VClock v) (VClock w) = M.isSubmapOfBy (<=) v w && not (M.isSubmapOf v w)

---------- PWR ----------

pwr :: [Event] -> RW
pwr = undefined

---------- PWR helpers ----------

-- look through lock history. If an acquire in history has earlier timestamp, sync current clock with clock in history
syncClock :: VClock -> [HistEl] -> VClock
syncClock = foldr (\(HistEl (Epoch j k) v') v ->
                    if k < clock v M.! j
                    then vUnion v v'
                    else v)

-- establish ROD (=release order dependency): If e, f in two crit. sections, e < f, then rel < f
--    VClock -> Lockset  -> hist              -> VClock
w3 :: VClock -> Set Lock -> Map Lock [HistEl] -> VClock
w3 v ls h = S.foldr (\l result -> syncClock result (h M.! l)) v ls

-- modify state for acquire event
acquire :: PWRState -> PWRState
acquire = undefined

-- modify state for release event
release :: PWRState -> PWRState
release = undefined

-- modify state for write event
write :: PWRState -> PWRState
write = undefined

-- modify state for read event
read :: PWRState -> PWRState
read = undefined

---------- Tests ----------

syncClockTest :: VClock
syncClockTest = let v1 = VClock $ M.fromList [(Thread 0, TStamp 0), (Thread 1, TStamp 0)]
                    v2 = VClock $ M.fromList [(Thread 0, TStamp 3), (Thread 1, TStamp 2)]
                    v3 = VClock $ M.fromList [(Thread 0, TStamp 2), (Thread 1, TStamp 10)]
                    e1 = Epoch (Thread 0) (TStamp 3)
                    e2 = Epoch (Thread 1) (TStamp 2)
                    h = [HistEl e1 v1, HistEl e2 v2]
                 in syncClock v3 h
