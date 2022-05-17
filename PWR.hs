{-# LANGUAGE GADTs #-}

module PWR where

import Control.Monad.State
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M  -- use IntMap?
import qualified Data.Set as S

import Trace

---------- data definitions ----------
newtype TStamp = TStamp Int
newtype VClock = VClock {clock :: [TStamp]}  -- idea: use vector-clock package, intmap, vector? Probably unnecessary
data Epoch     = Epoch {id ::  Thread, tstamp :: TStamp}
data HistEl   = HistEl {e :: Event, v :: VClock}

-- pretty direct copy of global variables for PWR algorithm from paper 
-- missing: "edges", "conc", "evt" are unneeded for simple PWR; "RW" is the result of PWR function
data PWRGlobal = PWRGlobal { lockS   :: Map Thread (Set Lock),  -- current lockset per thread
                             vclocks :: Map Thread VClock,      -- current vector clock per thread
                             lastW_C :: Map Var VClock,         -- vector clock of last write on var
                             lastW_T :: Map Var Thread,         -- threads with last write on var
                             lastW_L :: Map Var (Set Lock),     -- lockset of last write on var
                             acq     :: Map Lock Epoch,         -- epoch of last aquire for lock
                             hist    :: Map Lock [HistEl] }     -- lock history


---------- PWR ----------

newtype PWRState = State PWRGlobal
newtype RW = RW (Map Var (Set Event))  -- result type of computation

-- establish ROD (=release order dependency): If e, f in two crit. sections, e < f, then rel < f
--    VClock -> lockS                 -> hist              -> VClock
w3 :: VClock -> Map Thread (Set Lock) -> Map Lock [HistEl] -> VClock
w3 = undefined