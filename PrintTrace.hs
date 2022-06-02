module PrintTrace where

import Text.LaTeX  -- currently unused, since we're not generating full documents. Keep in consideration?
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find)

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

ex1Set = S.singleton ((wrE (Thread 1) (Var "x")){loc = Loc 2}, (wrE (Thread 0) (Var "x")){loc = Loc 3})

-- latex printing for Trace:
latexTrace :: [Event] -> IO ()
latexTrace = putStrLn . toLatex S.empty "" . addLoc . removeFork

-- latex printing for PWR:
-- TODO: delete empty lines
-- TODO: convert PWR to set
-- efficient event set selection?
latexEx1 :: [Event] -> IO ()
latexEx1 = putStrLn . toLatex ex1Set "PWR" . addLoc

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

-- string representation of latex code for trace
-- adds arrows for events in relation (=set of pairs) and labels them with name
toLatex :: Set (Event, Event) -> String -> [Event] -> String
toLatex pairs name es =
    let firstRow = 4
        m        = maximum (map (unThread . thread) es)
        findRight tags = find (\t -> head t == 'r') tags
        findLeft tags = find (\t -> head t == 'l') tags
        tagListToStrings tags = case (findLeft tags, findRight tags) of
                                    (Just lt, Just rt) -> (markL lt, markL rt)
                                    (Just lt, _)       -> (markL lt, "")
                                    (_, Just rt)       -> ("", markL rt)
                                    _                  -> ("", "")
    in
        foldl (\s e -> let l = show (loc e) ++ ". & "
                           i = unThread $ thread e
                           tags = S.foldl (\r rel -> case (fst rel == e, snd rel == e) of
                                                            (True, _) -> fst (uncurry makeTag rel) : r
                                                            (_, True) -> snd (uncurry makeTag rel) : r
                                                            _         -> r) [] pairs
                           (tagLeft, tagRight) = tagListToStrings tags
                           r = l ++ replicate i '&' ++ tagLeft ++ eventL e ++ tagRight ++ replicate (m-i) '&'
                       in s ++ "\n" ++ r ++ lineL)
        (bdaL m ++ concat [" & " ++ threadL i | i <- [1..m+1]] ++ lineL ++ "\\hline")
        es
        ++ edaL ++ S.foldl (\r evts -> uncurry tikzL evts name ++ r) "" pairs

-- ideas: add mark left if threads just one apart, else right. generate mark always the same. xshift: 

-- TODO: more nuanced
data RowRelation = Far | Next | Same  deriving Eq

-- determines if two events are in threads that would be next to each other in table
rowRelation :: Event -> Event -> RowRelation
rowRelation Event{thread=t1} Event{thread=t2} = case abs (unThread t1 - unThread t2) of
                                                0 -> Same
                                                1 -> Next
                                                _ -> Far


-- helpers for latex commands
tikzL :: Event -> Event -> String -> String
tikzL e f name = "\n \\begin{tikzpicture}[overlay, remember picture, yshift=.25\\baselineskip, shorten >=.5pt, shorten <=.5pt]\n \\draw[->]" ++
                    "({pic cs:" ++ fst tags ++  -- insert mark name 1
                    "}) [bend " ++ if rowRelation e f == Far then "left=100" else "right" ++    -- calculate bend direction for arrow
                    "] node [below, yshift=" ++ "-0.1" ++ -- calculate yshift for caption
                    "cm, xshift=" ++ "0.5" ++ -- calculate xshift for caption
                    "cm]{\\footnotesize{" ++ name ++  -- get relation name
                    "}} to ({pic cs:" ++ snd tags ++  -- insert mark name 2
                    "});\n \\end{tikzpicture} \n"
    where tags = makeTag e f

makeTag :: Event -> Event -> (String, String)  -- generates two tags like this: "l13" (=tag at the left of event 13), "r16"
makeTag e f = case rowRelation e f of
                Far -> ("r" ++ show (loc e), "r" ++ show (loc f))
                Next -> ("r" ++ show (loc e), "l" ++ show (loc f))
                Same -> ("l" ++ show (loc e), "l" ++ show (loc f))

markL :: String -> String
markL s = " \\tikzmark{" ++ s ++ "}"

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
eventL _ = ""  -- covers fork and join, which should have already been filtered out earlier