module HeuristicReorder where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State

import Trace
import TraceReorder

import PWR
