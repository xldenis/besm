# BESM

> A brief description of BESM is given in the familiar publicaiton Bystrodeistvuiushchaia schetnaia mashina Akademii nauk SSSR (High speed electronic calculating machien of the Academy of Sciences of the U.S.S.R) under teh editorial supervision of Academician S.A. Lebedev. However since the publication of this work a long time has elapsed during which a number of changes have been carried out on the machine, in particular the addition of certain new operations. Besides this the method addressing storage cells has been changed. It is therefore useful to present here a brief description of the machien and its code of operation, as well as to describe certain special programming procedures used in the programming programme.

"Esrhov, A.P. _Programming Programme for the BESM Computer._ Translated by M. Nadler, Pergamon Press, 1959."

The BESM was the first full computer built by the Soviet Union and the first in Continental Europe. Completed in 1952, it used 5000 vacuum tubes to perform between 8-10k operations per second on average.

The BESM was a three-address machien that  used 39-bit words, and represented all numbers in floating-point format. It had 1023 words of read-write memory as well as 384 words of read-only memory. For additional storage, BESM had a Magnetic Drum split into five sections of 1024 words each readable by a seperate head.

Input and output were handled by a photoelectric sensor reading perforated tape into the Instruction Store (IS) and by printing to paper tape or magnetic drive.

# What is this project?

For a while now I've been doing research into the manner in which the lineage of modern languages influences the ways we think about what is possible or even desirable in a programming language. This led me to look at early Soviet languages which evolved in relative isolation from American and British languages. To better understand the choices they made and their impact on the design of languages, I've decided to revive the first full soviet compiler.

This project will eventually include:

- a VM for the BESM on which to run
- a program to transform input PP programs into their input format for the compiler
- a modern implementation of the PP compiler
- a BESM re-implementation of the compiler


Resources to Find

Electronic Digital Machines

Electronic Computers

- https://catalog.lib.utexas.edu/record=b1571926~S29

Electronic Digital Computers and Programming

