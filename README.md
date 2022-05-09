# datarace-prediction

% Implementing PWR and tools in Haskell


# Overview

We consider concurrent programs that make use of reads/writes, acquire/release and fork/join statements.

We consider a specific program run represented as a trace of events.

We consider trace-based analysis methods that derive some strict partial-order
to determine for each event which events must "happen-before".

We consider two happens-before relations.

* Lamport's happens-before relation, and

* the PWR relation.


## Background material

Some lecture notes that cover Lamport's happens-before relation

https://sulzmann.github.io/AutonomeSysteme/lec-data-race.html

The PWR relation is covered in the following paper

Efficient, Near Complete and Often Sound Hybrid Dynamic Data Race Prediction (extended version)

https://arxiv.org/abs/2004.06969

referred to as the "PWR paper"


# Vorgehen und Ziele


1. PWR Implementierung in Haskell basierend auf Trace.hs (https://github.com/sulzmann/source)

Beachte. In der internen Trace Darstellung gelten folgende Konventionen:

* There's always a main thread (given the thread id 0)

* All other threads are created via "fork"

2. Annotierung des "Traces" mit PWR Informationen.
   Für jedes Event Paar muss feststellbar sein, ob die PWR Relation gilt oder nicht.

Es sollte eine Haskell Bibliothek geben mit welcher man unter
verschiedenen Arten von Information wie auch Formate auswählen kann.

* Markdowm Format (als Beispiel siehe https://github.com/sulzmann/source)

* Latex Format, z.B. Pfeildarstellung zwischen Events welche in PWR Relation stehen.

* Gezielte Auswahl von Events fuer welche PWR Relationen dargestellt werden sollen.

* Vereinfachung, z.B. "fork" wenn moeglich weglassen.
Betrachte Beispiel 2.6 aus dem "PWR paper". Die implizite Annhame ist,
dass 1# ist der Main Thread welcher die anderen Threads 2# und 3# forked.

Beachte, das ganze sollte für eine beliebige happens-before Relation betrachtet werden.

3. Generierung von Traces.

Variante (a). Ausgangspunkt, ein gegebener Trace.

* Berechne PWR Relation

* Generierung alle Umordnungenen des Traces unter welcher PWR erhalten bleibt.

* Beachte. Lock Semantik und WRDs (write-read dependencies) müssen erhalten bleiben.
  Dies muss extra überprüft werden.
  Naive Version, siehe "Reorder.hs" in https://github.com/sulzmann/source

* Idee: Wende HB Relation auf umgeordneten Trace an und versuche so herauszufinden,
   ob Lock Semantik und WRDs erhalten geblieben sind.

Betrachte weitere Varianten, z.B.

(b) Ausgangspunkt wie (a). Füge Events hinzu, entferne Events.

(d) Ausgangspunkt ist ein nebenläufiges Program. Ausführung des Programms liefert Trace.
    Instrumentiere Programm geeignet, so das verschiedene Traces erzeugt werden.

und weitere ...

Zu Variante (c).
Natürlich könnte man einfach das Programm mehrfach laufen lassen und hoffen es kommen
verschiedenen Traces dabei heraus. Ineffizient und Zufallsprinzip.

Systematischer Ansatz siehe "Finding and Reproducing Heisenbugs in Concurrent Programs" (https://web.eecs.umich.edu/~weimerw/2019-481W/readings/chess.pdf)


In welcher Sprache? Am besten eine einfache Haskell DSL welche die Operationen
writes/reads, acquire/release, fork/join unterstützt.


Zu klärende Frage.
Inwieweit kann der QuickCheck Ansatz zur Generierung von Traces verwendet werden?

4. Verschärfung der PWR Relation um false positives zu eliminiern (optional falls noch Zeit vorhanden).


# Latex Sourcen Beispiel 2.6 "PWR paper"

Folgendes package wird verwendet

~~~~~~
\usepackage{tikz}
\usetikzlibrary{tikzmark}
\usetikzlibrary{arrows,automata}
\usetikzlibrary{trees,shapes,decorations}
~~~~~~~~~


Ein paar Macros.

~~~~~~~~~
\newcommand{\thread}[2]{#1 \sharp #2}
\newcommand{\lockE}[1]{\mathit{acq}(#1)}
\newcommand{\unlockE}[1]{\mathit{rel}(#1)}
\newcommand{\readE}[1]{r(#1)}
\newcommand{\writeE}[1]{w(#1)}
~~~~~~~~~~~



~~~~~~~~
  Consider
  \bda{llll}
    & \thread{1} & \thread{2} & \thread{3} \\ \hline
1.  & \lockE{z} &&
\\ 2.  &  \writeE{y_1} \tikzmark{wrd-1-1} &&
\\ 3.  & \writeE{x} &&
\\ 4.  & \unlockE{z} \tikzmark{pwrcs-1} &&
\\ 5.  && \tikzmark{wrd-1-2} \readE{y_1} &
\\ 6.  && \writeE{y_2} \tikzmark{wrd-2-1} &
\\ 7.  &&&    \lockE{z}
\\ 8.  &&&    \tikzmark{wrd-2-2}  \readE{y_2} \tikzmark{pwrcs-2}
\\ 9. &&&     \unlockE{z}
\\ 10. &&&    \writeE{x}
\eda
%
           \begin{tikzpicture}[overlay, remember picture, yshift=.25\baselineskip, shorten >=.5pt, shorten <=.5pt]
           \draw[->] ({pic cs:wrd-1-1}) [bend right] node [below, yshift=-0.1cm, xshift=0.5cm]{\footnotesize{WRD}} to ({pic cs:wrd-1-2});
           \end{tikzpicture}
           \begin{tikzpicture}[overlay, remember picture, yshift=.25\baselineskip, shorten >=.5pt, shorten <=.5pt]
           \draw[->] ({pic cs:wrd-2-1}) [bend right] node [below, yshift=-0.4cm, xshift=-0.3cm]{\footnotesize{WRD}} to ({pic cs:wrd-2-2});
           \end{tikzpicture}
           \begin{tikzpicture}[overlay, remember picture, yshift=.25\baselineskip, shorten >=.5pt, shorten <=.5pt]
           \draw[->] ({pic cs:pwrcs-1}) [bend left=100]node [below, yshift=-0.4cm, xshift=2.7cm]{\footnotesize{PWR}} to ({pic cs:pwrcs-2});
           \end{tikzpicture}
		   %

~~~~~~~~~~~~~
