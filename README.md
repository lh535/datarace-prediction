# Traces, PWR, and Printing

This Haskell library offers an interface for specifying traces, the ability to compute the PWR relation, and the ability  
to annotate and print traces in Markdown or Latex format.

## Table of Contents
* [Modules](#modules)
* [Traces](#traces)
  * [Example](#example)
* [PWR](#pwr)
* [Printing](#printing)
  * [Markdown](#markdown)
  * [Latex](#latex)

## Modules
It is not necessary to build the project, the needed files can just be copied over.  
Here is an overview of what the available files contain:
- Trace.hs defines a language for creating traces. More details on how to define traces are in the next section.
- Examples.hs contains examples of Traces that can be used for testing
- PrintTrace.hs contains functions for printing traces in markdown and latex, and the option to annotate these traces with information
from a general race prediction algorithm (such as PWR, Happens-Before or Lockset).
- PWR.hs contains the algorithm for computing the PWR relation. It is generalized to work with both sets and vector clocks. There are also specialized versions of the printing functions defined in PrintTrace.hs, which use PWR as the algorithm for annotating the trace.


## Traces
Traces are represented as a list of Events. There are six types of events used: Reading a Variable, Writing a Variable, Acquiring a Lock, Releasing a Lock, Forking a Thread, and Joining a Thread. These Events can be be created using the following functions:  
- `rdE t x` (read event with t=current thread, x=variable)
- `wrE t x` (write event with t=current thread, x=variable)
- `acqE t x` (acquire event with t=current thread, x=lock variable)
- `relE t x` (release event with t=current thread, x=lock variable)
- `forkE t1 t2` (fork event with t1=current thread, t2=created thread)
- `joinE t1 t2` (join event with t1=current thread, t2=joined thread)


We start counting threads at 0, where thread 0 is the main thread. Every Trace must contain a main thread. There are functions for creating threads:  
`mainThread` has no arguments and returns the main thread. `nextThread` takes a thread and returns a thread with an incremented index number


Each Event also should have a location number, however the functions above do not automatically add this location number.  
To add correct location numbers to a trace, `addLoc` can be used, which takes and returns a trace (=list of events).  
Many of the printing functions apply `addLoc` automatically for convenience, however it also isn't problematic to apply it multiple times.

### Example

## PWR
This module can be used to compute the PWR relation.  
(TODO: finish writing this after generalizing pwr)

## Printing


### Markdown

### Latex