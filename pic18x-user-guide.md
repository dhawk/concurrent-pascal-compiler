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




# Global ErrorCode Variable

PIC18 microcontrollers normally do not operate in an environment conducive to extensive run-time error reporting, however a rudimentary error reporting capability is supplied in the form of a single global ErrorCode that can be set when a run-time error occurs.  This capability is used extensively by the compiler and kernel to detect and report run-time errors.  The assert procedure can also be used by the programmer to report errors - it will set ErrorCode if the condition is not true:

~~~
assert (<boolean>, 'Error Message')
~~~

The global ErrorCode variable is a uint24 value that holds the program counter value for the location where the error occurred.  The Error Message itself is not embedded anywhere in the Microcontroller’s ROM but is instead saved in a file named <program_name>.err which lists all possible error locations and the associated error messages.  The .err file is only valid for the assembly and hex files produced at the same time by the compiler.

The first run-time error to occur sets ErrorCode.  Subsequent run-time errors do not overwrite that first run-time error code.  This prevents the initial error code from being over-written if that error results in a cascade of subsequent errors.

Setting ErrorCode does not abort anything, the program continues running.  The idea is to allow the program to limp on long enough to somehow transmit the ErrorCode back to the programmer so that the underlying bug can be fixed before the code is released.

The ErrorCode variable may be directly read by Concurrent Pascal code for reporting – perhaps via a communications link or displayed in LEDs.  Reading the ErrorCode variable clears it, after which the next run-time error can set it again.

# Process Priority Mapping

Process level 2 is mapped to the high priority interrupt level.  Processes and monitors running at priority level 2 run with interrupts off.

Process level 1 is mapped to the low priority interrupt level.  Processes and monitors running at priority level 1 run with the low priority interrupts off and high priority interrupts on.

Process and monitors running at levels 0 and below run with interrupts on.

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

Often there are several adjacent SFRs for a hardware module that can be combined together into a single variable.  When a PIC contains multiple instances of the hardware module, multiple variables can be defined with a common type definition.  This type can be used to define a parameter to a subroutine which allows that subroutine to work with all instances of the hardware module.

An example would be the Analog to Digital Converter (ADC) in the PIC18F2520.  AD: tAD combines five SFRs into a single variable:

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
            -: uint4;
            ADRES_12R: uint12;
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
            ADRES_8L: uint8;
            ADRES_8R: uint8;
            -: uint6;
            DONE: uint1;
            -: uint17
         end;
         packed record
            ADRES_10L: uint10;
            -: uint12;
            nDONE: uint1;
            -: uint17
         end;
         packed record
            -: uint6;
            ADRES_10R: uint10;
            -: uint6;
            GO_DONE: uint1;
            -: uint17
         end;
         packed record
            ADRES_12L: uint12;
            -: uint28
         end;
         packed record
            ADRESH: uint8;
            ADRESL: uint8;
            -: uint24
         end
      end;

ioreg
   AD: tAD at $FC0;
~~~

### Special ADC Result Fields

In addition to ADRES, ADRESH and ADRESL, the following special fields are defined for the analog to digital converter (ADC):
~~~
            ADRES_8L: uint8;
            ADRES_8R: uint8;
            ADRES_10L: uint10;
            ADRES_10R: uint10;
            ADRES_12L: uint12;
            ADRES_12R: uint12;
~~~
Each of these fields provides precise access to a particular configurable ADC result.  Depending on the microcontroller, ADCs can be configured to provide 8, 10 or 12 bit results either left or right justified.  Using one of these fields instead of the 16 bit ADRES field will allow the compiler to generate more efficient code than if a 16-bit result field (ADRES) were used.

Note that the compiler does not automatically configure the ADC to load a specific result field, it assumes the programmer has done so before accessing that field.  Note also that only a few of the PICs have 12-bit ADCs - presence of a 12-bit ADRES field in the include file does not guarantee that a particular PIC has a 12-bit ADC (see the datasheet!). 

<table style="border: 1px solid black; width: 100%; background: #212121;">
   <tr>
      <td style="padding: 25px;">
         <p style="color: #fff; font-size: 32px;">Help Wanted!</p>
         <p style="color: #fff;">It is recognized that far from all useful combo SFR types have been identified.  If you have experience with a particular PIC18x hardware module, please submit suggestions for additional combo SFR types or special fields to 
         <a href="mailto:cpc@davidhawk.us" style="color: #0090FF; text-decoration: underline;">cp@davidhawk.us</a>
         </p>
      </td>
   </tr>
</table>

## Normal and Reversed Combo SFR Types

A perusal of the datasheets will reveal that Microchip's chip designers helpfully lay out some SFR groups as big-endian (MSB at lowest address) and others as little-endian (LSB at lowest address).  For example see these two SFR pairs from the PIC18F65J94 datasheet:

<table border="0" cellpadding="5">
    <tr>
        <td align="center">Normal</td>
        <td align="center">Reversed</td>
    </tr>
    <tr>
        <td><img src="pic18x-user-guide/ufrm.png" width="208" height="68"></td>
        <td><img src="pic18x-user-guide/tmr1.png" width="208" height="68"></td>
    </tr>
    <tr>
        <td align="center">big-endian</td>
        <td align="center">little-endian</td>
    </tr>
</table>

The compiler normally lays out multi-byte ordinal variables in big-endian format.  Little-endian SFR combinations are considered to be "Reversed byte order".

The compiler transparently handles both normal and reversed order SFR combinations.  All type definitions are laid out as  big-endian and the compiler generates the correct code to handle reversed byte order types.  Reversed combo SFR types are flagged in the processor definition xml file.

## Shared Address SFRs

Some PIC18 microcontrollers assign different SFRs to the same physical address and distinguish them by using the ADSHR bit in the WDTCON register.  The following explanation is from the PIC18F65J50 data sheet:
 
 
# General Purpose Registers (GPRs) 

GPRs are used as RAM and locations are assigned by the compiler for variables, system types, stacks and internal kernel data structures.

## Non-Contiguous GPR Regions

A few PIC18s have non-contiguous GPR regions.  The current implementation of the compiler only utilizes the first GPR region.

## Dual-Port GPRs

Some PIC18x microcontrollers (e.g. USB controllers) have dual-ported GPRs.  This is not currently supported by the compiler but is something that will need to be addressed in the future.


# EEPROM Support

Many PIC18x microcontrollers have internal EEPROM.  The compiler currently supports up to 256 bytes of internal EEPROM.  

# ROM Constants

The compiler gathers all ROM constants and constant strings into the first 64K of ROM memory.  Ensuring that the upper byte of the 24-bit program memory address for ROM constants is always clear slightly reduces the amount of code required to support ROM constants and interrupt overhead (TBLPTRU never needs to be set or saved).

# Integer Math Support

Multi-byte integer math code is generated with sufficiently sized operands to handle each integer math operation in the Concurrent Pascal program.  These operations cannot overflow.  The results are range checked and the global ErrorCode variable is set for out-of-range results.  Any out-of-range result is replaced with an arbitrary legal value and the program continues.


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

~~~
function round24 (r: real): int24;
function round32 (r: real): int32;
function trunc24 (r: real): int24;
function trunc32 (r: real): int32;
~~~

## Floating Point Error Handling

Floating Point errors are handled by setting the global ErrorCode.  The following errors are handled this way:

* Overflow (add, subtract, multiply, divide, integer to real conversion)
* Underflow (add, subtract, multiply, divide, integer to real conversion)
* Zero Divide (divide)
* Integer Overflow (real to integer conversion, round, trunc)

After setting ErrorCode, invalid floating point results are replaced by an arbitrary legal value and execution continues.

# String Support

Strings may contain a maximum of 255 characters.  The 0’th byte contains the current string length.  Actual bytes in the string are indexed 1..maxstrlen, where maxstrlen is the original string dimension from the type declaration.


## maxstrlen for undimensioned string parameters

The compiler will automatically add a hidden maxstrlen parameter for each undimensioned string parameter passed by reference.  Undimensioned strings passed as constants will not have a hidden maxstrlen parameter and the maxstrlen attribute will retrieve the actual strlen value instead.

~~~
procedure p (// the following parameters will have
             //   an additional hidden maxstrlen
             //   parameter passed:
                var sv: string;
                eeprom se: string;
             // the following parameters will report 
             //   the actual strlen for maxstrlen:
                s: string;
                rom sr: string
            );
~~~

