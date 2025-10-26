# What is this project?

For a while now I've been doing research into the manner in which the lineage of modern languages influences the ways we think about what is possible or even desirable in a programming language. This led me to look at early Soviet languages which evolved in relative isolation from American and British languages. To better understand the choices they made and their impact on the design of languages, I've decided to revive the first full soviet compiler.

The compiler and language didn't really have a name then, instead compilers were called 'programming programmes' (programming referred to the act of writing the actual machine code out).
Because this program was created for the very first soviet computer, the [BESM](#besm), I've retro-named it PP-BESM.

This language is quite interesting, it predates FORTRAN by a year and features quite a few interesting developments: it included several optimizations like dead-store elimination, common sub-expression elimination and bouds-check elimination (all limited of course). It featured several different kinds of loop statements, and an interesting notion of auto-indexing variables (called variable addresses).

These features are all the fallout of the Lyuponovian Operator Schemas that were being developed in the theoretical CS community around Moscow University at the time as a framework to structure and understand programming (starting in 1951-1952).

## What is included in this project?

- [a VM implementation of the BESM](/besm-vm), it is not cycle accurate (obviously) but recreates the semantics of various operations as closely as possible, and more importantly, I've attempted to retro-engineer the values of various constant addresses in the Diode Store, which are critical to writing programs.
- [A 'Literal BESM' re-implemtation of the PP-BESM compiler](/compile-pp) working from various primary and secondary sources. The code is written in a Haskell BESM Assembler DSL.
- [A PP-BESM parser and encoder](/pp-besm) which allows users to prepare text files for input into the compiler (as the computer didn't have text input!).
- [Examples from original sources](/examples) from the various articles and books I've scoured. To better understand the syntax I recommend looking at [this](examples/chapter1-annotated.pp).

# PP-BESM

The brunt of the work in this project revolves around recreating the source code the BESM. Luckily archives of Ershov's (original author) include his drafts of the code during his PhD.
Unfortunately, this code is different from the documentation provided in the book he wrote on the BESM and includes several bugs.
Nonetheless, it provides an **invaluable** resource as it also shows the various programming tricks used to write the compiler like self-modifying code and also the values of various constants builtin to the machine.

The compiler is split into three phases creatively named PP-1, PP-2, and PP-3, each of which may include sub-phases like PP-1-1, PP-1-2, etc..

- PP-1 is responsible for translating and optimizing arithmetic operations. It leaves the loops untouched other than potentially adjusting the address of loop variables.
PP-1 must also handle 'logical operators' (switches in modern parlance).
- PP-2 is responsible for translating loops.
- PP-3 handles the final layout of the code and 'linking', outputs the resulting binary and prints checksum statistics.

## What has been done?

At the time of writing:

- [X] PP-1. As far as I can tell PP-1 is complete and correct.
- [x] PP-2. PP-2 is complete but there are doubtlessly several bugs still lurking.
- [x] PP-3. PP-3 is complete but there are several bugs *known* bugs lurking.


# BESM

> A brief description of BESM is given in the familiar publicaiton Bystrodeistvuiushchaia schetnaia mashina Akademii nauk SSSR (High speed electronic calculating machien of the Academy of Sciences of the U.S.S.R) under teh editorial supervision of Academician S.A. Lebedev. However since the publication of this work a long time has elapsed during which a number of changes have been carried out on the machine, in particular the addition of certain new operations. Besides this the method addressing storage cells has been changed. It is therefore useful to present here a brief description of the machien and its code of operation, as well as to describe certain special programming procedures used in the programming programme.

"Esrhov, A.P. _Programming Programme for the BESM Computer._ Translated by M. Nadler, Pergamon Press, 1959."

The BESM was the first full computer built by the Soviet Union and the first in Continental Europe. Completed in 1952, it used 5000 vacuum tubes to perform between 8-10k operations per second on average.

The BESM was a three-address machine that  used 39-bit words, and represented all numbers in floating-point format. It had 1023 words of read-write memory as well as 384 words of read-only memory. For additional storage, BESM had a Magnetic Drum split into five sections of 1024 words each readable by a seperate head.

Input and output were handled by a photoelectric sensor reading perforated tape into the Instruction Store (IS) and by printing to paper tape or magnetic drive.

## Resources to Find

Electronic Digital Machines

Electronic Computers

- https://catalog.lib.utexas.edu/record=b1571926~S29

Electronic Digital Computers and Programming

