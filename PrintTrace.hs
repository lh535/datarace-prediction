module PrintTrace where

import Text.LaTeX  -- currently unused, since we're not generating full documents. Keep in consideration?
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import Trace
import PWR
import Examples

-- note that addLoc is applied to traces automatically as well

-- Markdown Printing of just Trace. If 2nd argument is true, fork/join are omitted
toMD :: [Event] -> Bool -> IO ()
toMD t b = (putStrLn . toMDExtra ("", const "") . addLoc) (if b then removeFork t else t)

-- markdown printing for PWR - Vector Clocks
annotatedWithPWR :: [Event] -> IO ()
annotatedWithPWR t = putStrLn $ toMDExtra ("Vector Clocks", \e -> show $ clocks (pwr trace) M.! e) trace
    where trace = addLoc t

-- markdown printing for PWR - Sets
annotatedWithPWRSet :: [Event] -> IO ()
annotatedWithPWRSet t =  putStrLn $ toMDExtra ("PWR Set", \e -> setShow $ allRelatedEvents (pwr trace) M.! e) trace
    where trace = addLoc t

-- TODO: markdown printing for PWR result of one event

-- latex printing for Trace:

-- latex printing for PWR:

-- remove fork/join from trace - used before addLoc. Can't be applied to traces used in pwr computation
removeFork :: [Event] -> [Event]
removeFork = filter f
    where f Event{op=(Fork t)} = False
          f Event{op=(Join t)} = False
          f _                  = True

---------- Markdown Printing ----------
-- (mostly inspired by existing work from souce/Trace.hs)

-- Markdown Printing of Trace, with extra last column
toMDExtra :: (String, Event -> String) -> [Event] -> String
toMDExtra (titleLast, genLast) es =
    let firstRow = 4
        rowS   = 10   -- longest event should be fork(tXX), which is 9 characters
        m        = maximum $ map (unThread . thread) es
        adj s xs | length xs >= s = error "fix row size"
                 | otherwise = xs ++ replicate (s - length xs) ' '
    in
        foldl (\s e -> let l = show (loc e) ++ "."
                           i = unThread $ thread e
                           r = adj firstRow l ++ replicate (rowS * i) ' '
                               ++ adj rowS (show (op e)) ++ replicate (rowS * (m-i)) ' ' ++ genLast e
                       in s ++ "\n" ++ r)
        (replicate (firstRow - 1) ' ' ++ concat [adj rowS ("T" ++ show i) | i <- [0..m]] ++ titleLast) -- arbitrary length for last row title
        es


-- helper for set printing
setShow :: Set Event -> String
setShow s = tail $ tail $ S.foldl (\r e -> r ++ ", " ++ show e) "" s

---------- Latex Printing ----------
-- resource : http://daniel-diaz.github.io/projects/hatex/hatex-guide.html

-- string representation of latex code for trace
toLatex :: Map Event (Event, String) -> [Event] -> String
toLatex m es =
    let firstRow = 4
        m        = maximum (map (unThread . thread) es)
    in
        foldl (\s e -> let l = show (loc e) ++ ". & "
                           i = unThread $ thread e
                           r = l ++ replicate i '&' ++ eventL e ++ replicate (m-i) '&'
                       in s ++ "\n" ++ r ++ lineL)
        (bdaL m ++ concat [" & " ++ threadL i | i <- [1..m+1]] ++ lineL ++ "\\hline")
        es
        ++ edaL -- ++ map for events in dict: tikzpicture

-- helpers for latex commands
threadL :: Int -> String
threadL i = "\\thread{" ++ show i ++ "}"

bdaL :: Int -> String
bdaL i = "\\bda{" ++ replicate (i+2) 'l' ++ "}\n"

edaL :: String
edaL = "\n \\eda{}"

lineL :: String  -- newline
lineL = "\\\\ "

eventL :: Event -> String
eventL Event{op=(Read x)} = "\\readE{" ++ show x ++ "}"
eventL Event{op=(Write x)} = "\\writeE{" ++ show x ++ "}"
eventL Event{op=(Acquire x)} = "\\lockE{" ++ show x ++ "}"
eventL Event{op=(Release x)} = "\\unlockE{" ++ show x ++ "}"
eventL _ = "&"  -- covers fork and join, which should have already been filtered out earlier

tikzL :: undefined -> String
tikzL e = "\\begin{tikzpicture}[overlay, remember picture, yshift=.25\\baselineskip, shorten >=.5pt, shorten <=.5pt]\n \\draw[->]" ++
          "({pic cs:" ++ "wrd-1-1" ++
          "}) [bend right] node [below, yshift=-0.1cm, xshift=0.5cm]{\\footnotesize{" ++ "WRD" ++
          "}} to ({pic cs:" ++ "wrd-1-2" ++
          "});\n \\end{tikzpicture}"