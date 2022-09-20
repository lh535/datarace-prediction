# Traces, PWR, and Printing
This Haskell library is part of a bachelor project and offers an interface for specifying traces, the ability to compute the PWR relation, and the ability to annotate and print traces in Markdown or Latex format.  
Further information on the PWR relation can be found in the paper "Efficient, Near Complete and Often Sound Hybrid Dynamic Data Race Prediction" (Martin Sulzmann & Kai Stadtmüller, 2020)

## Table of Contents
* [Modules](#modules)
* [Traces](#traces)
* [Printing](#printing)
  * [Markdown](#markdown)
  * [Latex](#latex)
    * [Imports and Macros](#imports)
* [PWR](#pwr)
* [Reordering](#reorder)
* [Benchmarking](#benchmarking)

## Modules
It is not necessary to build the project, the needed files can just be copied over.  
Here is an overview of what the available files contain:
- `Trace.hs` defines a language for creating traces. More details on how to define traces are in the next section. This file is imported by all others.
- `Examples.hs` contains examples of Traces that can be used for testing
- `PrintTrace.hs` contains functions for printing traces in markdown and latex, and the option to annotate these traces with information
from algorithm that compute a relation between events (such as PWR, Happens-Before or Lockset). It imports `Examples`.
- `PWR.hs` contains the algorithm for computing the PWR relation. It is generalized to work with both sets and vector clocks. There are also specialized versions of the printing functions defined in PrintTrace.hs, which use PWR as the algorithm for annotating the trace. It imports `PrintTrace`.


## Traces
Traces are a type `Trace [Event]`. There are six types of events used: Reading a Variable, Writing a Variable, Acquiring a Lock, Releasing a Lock, Forking a Thread, and Joining a Thread. These Events can be be created using the following functions:  
- `rdE t x` (read event with t=current thread, x=variable)
- `wrE t x` (write event with t=current thread, x=variable)
- `acqE t x` (acquire event with t=current thread, x=lock variable)
- `relE t x` (release event with t=current thread, x=lock variable)
- `forkE t1 t2` (fork event with t1=current thread, t2=created thread)
- `joinE t1 t2` (join event with t1=current thread, t2=joined thread)


We start counting threads at 0, where thread 0 is the main thread. Every Trace must contain a main thread. There are functions for creating threads:  
`mainThread` has no arguments and returns the main thread. `nextThread t` takes a thread t and returns a thread with an incremented index number.


Each Event also should have a location number, however the functions above do not automatically add this location number. To add correct location numbers to a trace and generate a correct trace, `addLoc t` can be used, which takes a list of events and returns a trace of type `Trace`.  


Lastly, an example on how to define a trace:  
```
ex1 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Var "x"
  in [ forkE t0 t1,
                    wrE t1 x,      -- w1
       wrE t0 x,                   -- w2
                    rdE t1 x       -- r3
     ]
```
More examples can be found in Examples.hs.

## Printing
General functions for printing traces in Markdown and Latex can be found in PrintTrace.hs. If the trace is supposed to be annotated with a relation, usually a function f that takes a trace and returns a map from events to a set of events has to be supplied.  
The following is an overview of the most important functions that are provided.

### Markdown
- `toMD remFork t` : prints a markdown table for a trace `t`. If `remFork` is True, fork/join is removed.
- `annotTrace f fShow name t` : like with `toMD`, a trace `t` is printed. Additionally, a function  
`f :: Show a => (Trace -> Map Event a)` should be supplied, which computes relations between events.  
`f_show :: a -> String` is a function to show the result of f, and can be just `show` if `a` is an instance of Show.  
Lastly, String `name` is used as a column name.  
All this information will be used for an additional column of information on the left of the trace. (Note: this is the only function where f can return a map to arbitrary types of values)
- `annotTraceSet f f_show name t` : the same as `annotTrace`, but without `fShow`. A predefined function for printing sets is used instead, therefore `f` also has to map to sets.
- `interactiveSet f name t` : The same as `annotTraceSet`, but as an interactive prompt where you can choose which events to show the relation for. The events can be chosen by inputing the corresponding location numbers after the prompt. For example, `1 3 10` would later show the fist, third and tenth event.  
Invalid input leads to an error! Invalid input includes letters, symbols, and numbers that do not have an associated event.  
Only the chosen events will be shown in the markdown table, and the relation set will also only contain events that were chosen.


Example of Markdown representation of ex1, annotated with PWR sets:  
``` 
> annotTraceSet (eventMap . pwr) "PWR Set" ex1

   T0        T1        PWR Set
1.  fork(t1)            fork(t1)_1
2.            wr(x)     fork(t1)_1, wr(x)_2
3.  wr(x)               fork(t1)_1, wr(x)_3
4.            rd(x)     fork(t1)_1, wr(x)_2, wr(x)_3, rd(x)_4
```

### Latex
A number of imports and macros is being used in the latex code. They are listed in the "Imports" section.
- `latexTrace remFork t` : prints latex code that displays the trace `t` as a string. If `remFork` is True, fork/join is omitted in the output.
- `interactiveLatex f t` : prints latex code for Trace `t`, but can add arrows between events that are in relation. The function `f` has to have the type `f :: [Event] -> Map Event (Set Event)` again. There are multiple prompts:  
The first asks for a name of the graph, which should not be the same as for previously generated graphs (the name is just used to avoid issues with conflicting graph marks and isn't shown).  
The next prompt asks if fork/join should be included, and the last one if arrows should be drawn for everything that is in relation (y) or not (n).  
If "n" is chosen for the third prompt, a numbered list of every relation pair is shown. The numbers can be used to choose which arrows should be drawn in the latex graph. Invalid input at this point leads to an error! Invalid input includes letters, symbols, and numbers that do not have an associated pair.  


Some additional notes on the latex output:  
Relations between events in the same thread are entirely omitted, as it is assumed that they are not of interest. This can technically be changed by removing `delSameThread` from `interactiveLatex`.  
The curving direction and strength of the bend of arrows is estimated loosely, and might need to be adjusted. Furthermore, labels are not added because they usually just end up covering part of the trace, but the automatic placement choice can be tested by writing some text in `\footnotesize{}`.  
Lastly, if more or different macros are used for events, the latex output for trace events can be changed in the function `eventL`.

### Imports
```
\usepackage{tikz}
\usetikzlibrary{tikzmark}
\usetikzlibrary{arrows,automata}
\usetikzlibrary{trees,shapes,decorations}

\newcommand{\thread}[2]{\ensuremath{#1 \sharp #2}}
\newcommand{\lockE}[1]{\ensuremath{acq(#1)}}
\newcommand{\unlockE}[1]{\ensuremath{rel(#1)}}
\newcommand{\readE}[1]{\ensuremath{r(#1)}}
\newcommand{\writeE}[1]{\ensuremath{w(#1)}}
\newcommand{\forkE}[1]{\ensuremath{fork(#1)}}
\newcommand{\joinE}[1]{\ensuremath{join(#1)}}

\newcommand{\ba}{\begin{array}}
\newcommand{\ea}{\end{array}}
\newcommand{\bda}{\[\ba}
\newcommand{\eda}{\ea\]}
```

## PWR
This module can be used to compute the PWR relation.  
`pwr` takes a trace as the only argument, and computes a result of type `R a`, which is a map from each event to the computed pwr-relation for it. `eventMap` is the unpacking function to get the map from the return type.  
The map has the following type: `PWRType a => Map Event a`. `PWRType` is a class with five functions that are needed for pwr and is defined at the end of the file. Two instances of `PWRType` are predefined: `VClock`, which is a vector clock representation, and `Set Event`.  
`pwrClock` and `pwrSet` can be used if either vector clocks or sets should explicitely be used.


There are also specialized functions for pwr printing:  
- `annotatedWithPWR t` prints a markdown table of the trace `t` and the pwr vector clocks
- `annotatedWithPWRSet t` prints a markdown table of the trace `t` and the pwr sets
- `interactivePWR t` is the PWR version of `interactiveSet` and can print markdown for only some events. See section about markdown printing for more details.
- `interactiveLatexPWR t` can print latex code of the trace `t`, with the option to add arrows for events that are in the pwr relation. See section about latex printing for more details. 

## Reordering
There are two files which compete all possible valid reorderings: ReorderNaive, which is based on work by Martin Sulzmann, and TraceReorder.  
ReorderNaive computes all possible permutations of a trace and checks them validity, before returning a list of valid traces.  
The function to test this reordering method is `reorderNaivePWR t`, where `t` is a trace.  
TraceReorder aims to do the same thing, but by using a backtracking algorithm. The function for this is `reorder t` for a trace `t`.

## Benchmarking
A benchmark and profiling report are already saved in `reorder-benchmark.html` and `profiling.prof`. To run the benchmarks yourself, you can run `cabal run cabal run reorder-benchmark -- --output reorder-benchmark.html`.  
For profiling, `cabal run profiling -- +RTS -P` can be used. Additional tests can be added in the corresponding files ReorderBenchmark.hs and Profiling.hs.