# concurrent-pascal-compiler
A Concurrent Pascal Compiler for Microcontrollers

#### THIS SITE IS UNDER CONSTRUCTION...

*This site is under construction as I learn GitHub and develop a CI system for this project. *

The goal of the project is to produce a Concurrent Pascal compiler for the subset of PIC18 microcontrollers that support the extended instruction set.  The intention is to be able to run the compiler in MicroChip's MPLABX IDE.

The compiler is being developed in Delphi, but is being written so as to also compile with Lazarus.  The initial implementation is for Windows, but Lazarus should eventually allow support for all environments supported by MPLABX.  

The compiler is designed to allow code emitters for other processor chips to be written as well, however there are no plans to actually do this at this point.

Documentation as it is written will appear at http://dhawk.github.io/concurrent-pascal-compiler

Status:


- the project is at alpha stage at this point.
- the compiler is mostly functional.
- a generator has been written to produce include files for the PIC18 chips that support the extended instruction set.
- very little documentation is available so far.

Email: cp@davidhawk.us
