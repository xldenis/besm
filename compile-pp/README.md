# compile-pp: recreation of the pp-besm compiler

PP-BESM is one of the very first programming languages developped in the Soviet Union. Developped by A. P. Ershov in 1955, it was first used in early 1956, on the BESM computer, one of two computers at the time (the other being the STRELA which had it's own PP-STRELA in 1955).

The source language is quite alien by todays standards, reflecting a different view on the objectives of programming in the early soviet computer era. Besides the syntactical differences, it featured several important innovations compared to contemporary languages (american or soviet). PP-BESM was the first language to feature actual optimizations, it was able to reduce the usage of working memory through a rudimentary liveness analysis, and was able to exploit the commutivity and associativity of addition and multiplication to eliminate common sub-expressions. It also included a comphrensive characterization of loops, their types and supported what would now be known as a `for` or `foreach` loop.

All these features were packed into 1200 hand-written assembly instructions and 450 constants. The compiler had to run in three passes because there was insufficient memory to hold both the input program and compiler.

This sub-project is a reimplementation of the compiler assembly, written as monadic DSL in Haskell. Each of the 14 subprograms of PP-BESM are divided into their own modules and implemented operator-by-operator. An assembler then attempts to minimize the explicit jumps when laying out the operators and constants before outputing a hex representation of the binary.
