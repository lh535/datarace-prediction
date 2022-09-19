module PrintTrace where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find)

import Trace
import Examples
import Control.Monad.RWS (MonadState(put))
import Debug.Trace


-- Markdown Printing of Traces. If 2nd argument is true, fork/join are omitted
toMD :: Bool -> Trace -> IO ()
toMD b t = (putStrLn . toMDExtra ("", const "")) (if b then removeFork t else t)

-- annotate Trace with results from an algorithm that maps events to a set of related events. Doesn't remove fork/join
-- f is function to compute relation, show_f is show function for result of f
annotTrace :: (Trace -> Map Event a) -> (a -> String) -> String -> Trace -> IO ()
annotTrace f fShow name trace@(Trace t) = putStrLn $ toMDExtra (name, \e -> fShow $ f trace M.! e) trace

-- specialized version of annotTrace which uses setShow
annotTraceSet :: (Trace -> Map Event (Set Event)) -> String -> Trace -> IO ()
annotTraceSet f = annotTrace f setShow

-- markdown printing where you can choose which events should be included in relation sets
-- note that only chosen events show their sets, and only chosen events are in those sets as well
interactiveSet :: (Trace -> Map Event (Set Event)) -> String -> Trace -> IO()
interactiveSet f name trace@(Trace t) = do
    let pwrSets = f trace
    toMD False trace
    putStrLn "\n Choose Events to show relation for (type event locations, separated by a space):"
    nums <- getLine
    let numList = map (Loc . read) (words nums)
    let filteredTrace = filter (\e -> loc e `elem` numList) t
    let filteredMap = M.filterWithKey (\e _ -> loc e `elem` numList) pwrSets
    let filteredSets = M.map (S.filter (\e -> loc e `elem` numList)) filteredMap
    putStrLn $ toMDExtra (name, \e -> setShow $ filteredSets M.! e) (Trace filteredTrace)


-- latex printing for Traces. Has toggle for removing fork/join or not, and doesn't draw relation arrows
latexTrace :: Bool -> Trace -> IO ()
latexTrace remFork = putStrLn . toLatex S.empty "" remFork . (if remFork then removeFork else id)

-- latex printing for example 1, using pre-defined set for arrows. Filters out Fork/Join
-- shows usage of toLatex: a Set with Relations, a relation name, a bool to decide if fork/join should be removed, and a valid trace must be passed as arguments
ex1Set = S.singleton ((wrE (Thread 1) (Var "x")){loc = Loc 2}, (rdE (Thread 1) (Var "x")){loc = Loc 4})
latexEx1 :: IO ()
latexEx1 = (putStrLn . toLatex ex1Set "PWR" True) ex1

-- latex printing for a relation function:
interactiveLatex :: (Trace -> Map Event (Set Event)) -> Trace -> IO ()
interactiveLatex f trace@(Trace t) = do
    let fPair = makePairs . f
    toMD False trace
    putStrLn "\nChoose a unique graph name:"
    name <- getLine
    putStrLn "Do you want to remove fork/join? (y/n)"
    decideFork <- getLine
    let relList = delSameThread $ if decideFork == "y" then delPairFork $ fPair trace else fPair trace
    putStrLn "Do you want to include all possible pairs? (y/n)"
    decideAll <- getLine
    if decideAll == "n"
        then do putStrLn "\nThese are the pairs of Events in the Relation. Type index of pairs to include in latex graphic, separated by a space.\n"
                let numberedList = zip [1..] (S.toList relList)
                putStrLn $ foldl (\r e -> r ++ show (fst e) ++ ": " ++ show (snd e) ++ "\n") "" numberedList
                nums <- getLine
                let numList = map read (words nums)
                let filteredList = filter (\e -> fst e `elem` numList) numberedList
                let filteredSet = foldl (\r e -> S.insert (snd e) r) S.empty filteredList
                putStrLn $ "\n\n" ++ toLatex filteredSet name False trace
        else do putStrLn $ "\n\n" ++ toLatex relList name True trace


---------- misc utility functions ----------

-- delete pairs that include fork/join from a set of relation pairs
delPairFork :: Set (Event, Event) -> Set (Event, Event)
delPairFork = S.filter (\e -> (not . isForkJoin . fst) e && (not . isForkJoin . snd) e)

-- delete pairs that are in relation because of program order (=in same thread)
delSameThread :: Set (Event, Event) -> Set (Event, Event)
delSameThread = S.filter (\e -> uncurry rowRelation e /= Same)

-- remove fork/join from trace - used before addLoc. Can't be applied to traces used in pwr computation
removeFork :: Trace -> Trace
removeFork = Trace . filter (not . isForkJoin) . unTrace

isForkJoin :: Event -> Bool
isForkJoin Event{op=(Fork _)} = True
isForkJoin Event{op=(Join _)} = True
isForkJoin _ = False

-- helper for set printing
setShow :: Set Event -> String
setShow s | s == S.empty = ""
          | otherwise = tail $ tail $ S.foldl (\r e -> r ++ ", " ++ show e) "" s

-- creates set of event pairs from mapping of event to set of event
makePairs :: Map Event (Set Event) -> Set (Event, Event)
makePairs = M.foldlWithKey (\r e s -> S.union r (S.map (\evt -> (evt, e)) s)) S.empty

---------- Markdown Printing ----------
-- (mostly inspired by existing work from sulzmann/source/Trace.hs)

-- Markdown Printing of Trace, with extra last column
toMDExtra :: (String, Event -> String) -> Trace -> String
toMDExtra (titleLast, genLast) es =
    let firstRow = 4
        rowS   = 10   -- longest string in trace should be fork(tXX), which is 9 characters
        m        = maximum $ map (unThread . thread) (unTrace es)
        adj s xs | length xs >= s = error "fix row size"
                 | otherwise = xs ++ replicate (s - length xs) ' '
    in
        foldl (\s e -> let l = show (loc e) ++ "."
                           i = unThread $ thread e
                           r = adj firstRow l ++ replicate (rowS * i) ' '
                               ++ adj rowS (show (op e)) ++ replicate (rowS * (m-i)) ' ' ++ genLast e
                       in s ++ "\n" ++ r)
        (replicate (firstRow - 1) ' ' ++ concat [adj rowS ("T" ++ show i) | i <- [0..m]] ++ titleLast) -- arbitrary length for last row title
        (unTrace es)

---------- Latex Printing ----------

-- NOTE: adding the name of the relation to arrows is currently disabled, because adding it in the right place is hard to automate.
-- it can be re-enabled by removing the comment of "name" in tikzL. In that function, adjustments to arrow formatting can be made as well.

-- string representation of latex code for trace.
-- arguments: Set of Events that should be shown as in relation (=add arrow); name of relation; bool to decide if fork/join should be included; trace.
toLatex :: Set (Event, Event) -> String -> Bool -> Trace -> String
toLatex pairs name removeFork es =
    let firstRow = 4
        m        = maximum (map (unThread . thread) (unTrace es))
        findRight tags = find (\t -> head t == 'r') tags
        findLeft tags = find (\t -> head t == 'l') tags
        tagListToStrings tags = case (findLeft tags, findRight tags) of
                                    (Just lt, Just rt) -> (markL lt ++ spaceL, markL rt)
                                    (Just lt, _)       -> (markL lt ++ spaceL, "")
                                    (_, Just rt)       -> ("", spaceL ++ markL rt)
                                    _                  -> ("", "")
    in
        unlines (zipWith (++) (["", ""] ++ map (\c -> show c ++ ". & ") [1..]) (lines $  -- to add correct line numbers
        foldl (\s e -> let i = unThread $ thread e
                           tags = S.foldl (\r rel ->  case (fst rel == e, snd rel == e) of  -- make tag if in relation set
                                                            (True, _) -> fst (uncurry (makeTag name) rel) : r
                                                            (_, True) -> snd (uncurry (makeTag name) rel) : r
                                                            _         -> r) [] pairs
                           (tagLeft, tagRight) = tagListToStrings tags
                           r = replicate i '&' ++ tagLeft ++ eventL e ++ tagRight ++ replicate (m-i) '&'
                       in if removeFork && isForkJoin e then s else s ++ "\n" ++ r ++ lineL)  -- don't add line if it's fork/join event
        (bdaL m ++ concat [" & " ++ threadL i | i <- [1..m+1]] ++ lineL ++ "\\hline")
        (unTrace es)))
        ++ edaL ++ S.foldl (\r evts -> uncurry tikzL evts name ++ r) "" pairs  -- adds tikzpicture code afterwards

---------- converters to latex code ----------

markL :: String -> String
markL s = " \\tikzmark{" ++ s ++ "}"

spaceL :: String
spaceL = " \\hspace{0.2em} "

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
eventL Event{op=(Fork t)} = "\\forkE{" ++ tail (show (nextThread t)) ++ "}"
eventL Event{op=(Join t)} = "\\joinE{" ++ tail (show (nextThread t)) ++ "}"


-- generate tikzpicture block for arrows. Relation name currently disabled, because positioning can't be automated easily
tikzL :: Event -> Event -> String -> String
tikzL e f name = "\n \\begin{tikzpicture}[overlay, remember picture, yshift=.25\\baselineskip, shorten >=.5pt, shorten <=.5pt]\n \\draw[->]" ++
                    "({pic cs:" ++ fst tags ++                        -- insert mark name 1
                    "}) [bend " ++ chooseBend (rowRelation e f) ++    -- calculate bend direction for arrow
                    "] node [below, yshift=" ++ "-0.1" ++             -- calculate yshift for caption (currently constant)
                    "cm, xshift=" ++ "0.5" ++                         -- calculate xshift for caption (currently constant)
                    "cm]{\\footnotesize{" ++ -- name ++               -- get relation name (currently disabled)
                    "}} to ({pic cs:" ++ snd tags ++                  -- insert mark name 2
                    "});\n \\end{tikzpicture} \n"
    where tags = makeTag name e f
          chooseBend FarR  = "left=100"
          chooseBend FarL  = "right=100"
          chooseBend NextR = "left"
          chooseBend _     = "right"

---------- helper functions for latex printing ----------

data RowRelation = FarR | FarL | NextR | NextL | Same  deriving Eq

-- determines if two events are in threads that would be next to each other in table
rowRelation :: Event -> Event -> RowRelation
rowRelation Event{thread=t1} Event{thread=t2} = case unThread t1 - unThread t2 of
                                                0    -> Same
                                                1    -> NextL
                                                (-1) -> NextR
                                                x -> if x > 0 then FarL else FarR

-- generates two tags based on which rows the events are in. Tags look like in this example: "l13" (=tag at the left of event 13)
makeTag :: String -> Event -> Event -> (String, String)
makeTag name e f = case rowRelation e f of
                FarR  -> ("r-" ++ name ++ show (loc e), "r-" ++ name ++ show (loc f))
                FarL  -> ("l-" ++ name ++ show (loc e), "l-" ++ name ++ show (loc f))
                NextR -> ("r-" ++ name ++ show (loc e), "l-" ++ name ++ show (loc f))
                NextL -> ("l-" ++ name ++ show (loc e), "r-" ++ name ++ show (loc f))
                Same  -> ("l-" ++ name ++ show (loc e), "l-" ++ name ++ show (loc f))