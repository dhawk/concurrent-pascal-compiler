---
layout: default
title: PIC18x Concurrent Pascal User Guide
---

<h1><center>{{title}}</center></h1>

This document is intended for a user of the Concurrent Pascal implementation for MicroChip's PIC18<b><u>x</u></b> line of microcontrollers. Only PIC18 microcontrollers with the e<b><u>x</u></b>tended instruction set are supported. The extended instruction set adds support for the stack operations that allow re-entrant code. 

This document assumes the reader is already familiar with:

* Concurrent Pascal introductory material,
* Concurrent Pascal Language Reference,
* The MicroChip Datasheet for your PIC18x microcontroller.

# Table of Contents
{:.no_toc}

* TOC
{:toc}

# Processor Directive

The microcontroller is specified by a compiler directive in the first line in the source file:

~~~
{$processor 'pic18f2520'}
~~~

This directive causes the compiler to reference two files included with the Concurrent Pascal distribution:

* bin/pic18x/include/pic18f2520.inc
* bin/processor_definition_files/pic18f2520.xml


The pic18f2520.inc file is a Concurrent Pascal include file which specifies ioreg type definitions, ioregs, interrupt variables and prototypes for special compiler-implemented procedures for that microprocessor.

The pic18f2520.xml file provides microcontroller specific information for the compiler such as SFR addresses.


# General Purpose Registers (GPRs) 

GPRs are used as RAM and locations are assigned by the compiler for variables, system types, stacks and internal kernel data structures.

## Non-Contiguous GPR Regions

A few PIC18s have non-contiguous GPR regions.  The current implementation of the compiler only utilizes the first GPR region.

## Dual-Port GPRs

Some PIC18x microcontrollers (e.g. USB controllers) have dual-ported GPRs.  This is not currently supported by the compiler but is something that will need to be addressed in the future.


# Special Function Registers (SFRs)

The microprocessor include file referenced by the $processor directive contain type definitions for the SFRs.  

For example, the datasheet for the PIC18F2525 describes the BAUDCON SFR bits as follows:


![](pic18x-user-guide/baudcon.png){:hspace="50"}


The pic18f2525.inc include file supplies the following type definition and ioreg declaration for BAUDCON:

~~~
type
   tBAUDCON =
      overlay
         packed record
            ABDOVF: uint1;
            RCIDL: uint1;
            RXDTP: uint1;
            TXCKP: uint1;
            BRG16: uint1;
            -: uint1;
            WUE: uint1;
            ABDEN: uint1
         end;
         packed record
            -: uint1;
            RCMT: uint1;
            RXCKP: uint1;
            SCKP: uint1;
            -: uint4
         end
      end;

ioreg
   BAUDCON: tBAUDCON at $FB8;
~~~

In addition to the fields defined by the datasheet, there are also occassionally some additional overlaid fields taken from Microchip documentation such as the RCMT, RXCKP and SCKP fields above.

## Combo SFR types

Often there are adjacent groups of SFRs that can be treated as one type.  This can be for convenience and sometimes, when there are multiple such groups the combined type can be used for all.  The type can be handled as a parameter and one piece of code can be used for the multiple SFR groups.

An example would be the Analog to Digital Converter.  The AD: tAD variable combines the following SFRs into one type:
* A/D Result High Register (ADRESH)
* A/D Result Low Register (ADRESL)
* A/D Control Register 0 (ADCON0)
* A/D Control Register 1 (ADCON1)
* A/D Control Register 2 (ADCON2)

~~~
type
   tAD =
      overlay
         packed record
            ADRES: uint16;
            -: uint2;
            CHS: uint4;
            GO_nDONE: uint1;
            ADON: uint1;
            -: uint2;
            VCFG: uint2;
            PCFG: uint4;
            ADFM: uint1;
            -: uint1;
            ACQT: uint3;
            ADCS: uint3
         end;
         packed record
            ADRESH: uint8;
            ADRESL: uint8;
            -: uint2;
            CHS3: uint1;
            CHS2: uint1;
            CHS1: uint1;
            CHS0: uint1;
            GO: uint1;
            -: uint3;
            VCFG1: uint1;
            VCFG0: uint1;
            PCFG3: uint1;
            PCFG2: uint1;
            PCFG1: uint1;
            PCFG0: uint1;
            -: uint2;
            ACQT2: uint1;
            ACQT1: uint1;
            ACQT0: uint1;
            ADCS2: uint1;
            ADCS1: uint1;
            ADCS0: uint1
         end;
         packed record
            -: uint22;
            DONE: uint1;
            -: uint17
         end;
         packed record
            -: uint22;
            nDONE: uint1;
            -: uint17
         end;
         packed record
            -: uint22;
            GO_DONE: uint1;
            -: uint17
         end
      end;

ioreg
   AD: tAD at $FC0;
~~~
 
# EEPROM Support

Many PIC18x microcontrollers have internal EEPROM.  The compiler currently supports up to 256 bytes of internal EEPROM.  

# ROM Constants

The compiler gathers all ROM constants and constant strings into the first 64K of ROM memory.  Ensuring that the upper byte of the 24-bit program memory address for ROM constants is always clear slightly reduces the amount of code required to support ROM constants and interrupt overhead (TBLPTRU never needs to be set or saved).



# Floating Point Support

The PIC18x Concurrent Pascal compiler supports 32-bit real variables.  The floating point routines were adapted from the PIC17 routines developed by Frank J. Testa for Microchip.  This library is described in Microchip’s [Application Note 575: IEEE 754 Compliant Floating Point Routines](http://ww1.microchip.com/downloads/en/AppNotes/00575.pdf) and [Application Note 660: Floating Point Math Functions](http://ww1.microchip.com/downloads/en/AppNotes/00660.pdf).  The PIC17 routines were modified to be reentrant by using the stack rather than absolute addresses.  Some minor changes were also necessary to account for differences between the PIC17 and PIC18 instruction sets.

All of the 32-bit math operations (add, subtract, multiply, etc.) from AN754 as well as the comparison functions from AN660 were incorporated into the Concurrent Pascal compiler.  AN660 also provides implementations of more advanced functions such a sin, cos, exp, etc.  These may be incorporated in the future.

The compiler implements two 32-bit floating point types:

## real

The real type should be used for most purposes within a Concurrent Pascal program.  Despite the title of Microchip’s application note, the binary format of a real variable is not the same as an IEEE 754 single, but is instead slightly altered for better performance on PIC microcontrollers.

## ieee_single

The ieee_single type is provided for use in transmitting binary values from the microcontroller to or from an external computer that implements IEEE floating point.  ieee_single and real variables are assignment compatible and an assignment will convert between the two formats.  Normally all calculations should be carried out with real variables and then the result assigned to an ieee_single variable for transmission in binary format to the external computer.

The following code fragment shows how to use an overlay variable to access the individual binary bytes of an ieee_single variable:

~~~
var
   r: real;
   o: overlay
         ieee: ieee_single;
         packed record
            b0: uint8;   // lsb of ieee_single
            b1: uint8;
            b2: uint8;
            b3: uint8    // msb of ieee_single
         end
      end;
begin
   calculate r;
   o.ieee := r;
   transmit  o.b3..o.b0  to external computer
end.
~~~

Note that real and ieee_single types are little-endian as implemented in the PIC18x Concurrent Pascal compiler.

## mod operator implementation

The PIC18x Concurrent Pascal compiler’s mod operator implementation gives a result with the same sign as the dividend.  This is the same behavior implemented in Delphi, but is different than ISO Pascal where the result is always positive. 

## round and trunc functions

In lieu of the standard Pascal round and trunc functions which return “standard integer” results, the PIC18x Concurrent Pascal compiler (which does not have a “standard integer”) provides these functions:

```pascal
function round24 (r: real): int24;
function round32 (r: real): int32;
function trunc24 (r: real): int24;
function trunc32 (r: real): int32;
```

## Floating Point Error Handling

Floating Point errors are handled by setting the global error code.  The following errors are handled this way:

- Overflow (add, subtract, multiply, divide, integer to real conversion)
- Underflow (add, subtract, multiply, divide, integer to real conversion)
- Zero Divide (divide)
- Integer Overflow (real to integer conversion, round, trunc)


