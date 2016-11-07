---
layout: default
title: PIC18x Concurrent Pascal User Guide
---

<h1><center>{{title}}</center></h1>

This document is intended for the user of the Concurrent Pascal compiler for MicroChip's PIC18<b><u><font color="#FF0000">x</font></u></b> line of microcontrollers. Only PIC18 microcontrollers with the e<b><u><font color="#FF0000">x</font></u></b>tended instruction set are supported. The extended instruction set adds support for the stack operations required for allow re-entrant code. 

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
* bin/pic18x/processor_definition_files/pic18f2520.xml


The pic18f2520.inc file is a Concurrent Pascal include file that specifies ioreg type definitions, ioregs, interrupt variables and prototypes for special compiler-implemented procedures for that microprocessor.

The pic18f2520.xml file provides microcontroller specific information for the compiler such as SFR addresses, memory layout and sizes, and so forth.

# Configuration Bits

The include file supplies a type definition for a structured constant for the MicroController’s configuration bits (aka fuses). The structured constant type defines a record containing all configuration bytes for the microcontroller. Below is a partial listing of the type definition  for the PIC18F2520:

~~~
type
   tPIC18F2520_configuration_bits =
      record
         -: uint8;
         CONFIG1H:
            packed record
               IESO:
                  (CONFIG1H_IESO_OFF = 0,
                   CONFIG1H_IESO_ON  = 1
                  );
               FCMEN:
                  (CONFIG1H_FCMEN_OFF = 0,
                   CONFIG1H_FCMEN_ON  = 1
                  );
               -: uint2;
               OSC:
                  (CONFIG1H_OSC_LP      = $0,
                   CONFIG1H_OSC_XT      = $1,
                   CONFIG1H_OSC_HS      = $2,
                   CONFIG1H_OSC_RC      = $3,
                   CONFIG1H_OSC_EC      = $4,
                   CONFIG1H_OSC_ECIO6   = $5,
                   CONFIG1H_OSC_HSPLL   = $6,
                   CONFIG1H_OSC_RCIO6   = $7,
                   CONFIG1H_OSC_INTIO67 = $8,
                   CONFIG1H_OSC_INTIO7  = $9
                  ) 
            end;
         CONFIG2L:
            packed record
               BORV:
                  (CONFIG2L_BORV_0 = 0,
                   CONFIG2L_BORV_1 = 1,
                   CONFIG2L_BORV_2 = 2,
                   CONFIG2L_BORV_3 = 3
                  );
               BOREN:
                  (CONFIG2L_BOREN_OFF     = 0,
                   CONFIG2L_BOREN_ON      = 1,
                   CONFIG2L_BOREN_NOSLP   = 2,
                   CONFIG2L_BOREN_SBORDIS = 3
                  );
               PWRT:
                  (CONFIG2L_PWRT_ON  = 0,
                   CONFIG2L_PWRT_OFF = 1
                  ) 
            end;
         ...
~~~

The program itself will normally contain a structured constant specifying the desired configuration bits. A program containing an appropriately named structured constant will include the configuration bits in the binary files produced by the compiler. The constant <b>must</b> be named xxxx_configuration_bits where xxxx is the PIC18 microcontroller specified in the compiler directive at the beginning of the program.

~~~
const
   PIC18F2520_configuration_bits: tPIC18F2520_configuration_bits =
      (CONFIG1H =
          (IESO = CONFIG1H_IESO_OFF,
              // Internal/External Oscillator Switchover bit:
              //    OFF = Oscillator Switchover mode disabled
           FCMEN = CONFIG1H_FCMEN_OFF,
              // Fail-Safe Clock Monitor Enable bit:
              //    OFF = Fail-Safe Clock Monitor disabled
           OSC = CONFIG1H_OSC_RCIO6
              // Oscillator Selection bits:
              //    RCIO6 = External RC oscillator, port function on RA6
          ),
       CONFIG2L =
          (BORV = CONFIG2L_BORV_3,
              // Brown Out Reset Voltage bits:
              //    3 = Minimum setting
           BOREN = CONFIG2L_BOREN_SBORDIS,
              // Brown-out Reset Enable bits:
              //    SBORDIS = Brown-out Reset enabled in hardware only (SBOREN is disabled)
           PWRT = CONFIG2L_PWRT_OFF
              // Power-up Timer Enable bit:
              //    OFF = PWRT disabled
          ),
       ...
~~~ 

## Configuration Bits Editor

The Configuration Bits Editor (pic18x_config_bit_editor.exe) is used to easily construct a custom configuration bits constant for a project. This constant is typically saved in its own include file in the project directory and included into the main source file:  
~~~
{$processor 'pic18f2520'}
{$include 'pic18f2520_config_bits.inc'}
...
~~~

The desired micro-controller is selected when a new include file is created.  The initial version of the constant with standard default values appears in the Main tab.  There is a tab for each configuration byte of the microcontroller and individual fields can be set using radio buttons.

![](pic18x-user-guide/config-bit-editor.png){:hspace="50"}

For each field the default value is shown in bold font.  

Switching back to the Main tab shows the current value of the constant.

Once the configuration constant is fully specified it should be saved to the project source directory.  This file can be re-opened in the editor at a later time if modifications are necessary.

# General Purpose Registers (GPRs) 

GPRs are used as RAM and locations are assigned by the compiler for variables, system types, stacks and internal kernel data structures.

## Non-Contiguous GPR Regions

A few PIC18s have non-contiguous GPR regions.  The current implementation of the compiler only utilizes the first GPR region.

## Dual-Port GPRs

Some PIC18x microcontrollers (e.g. USB controllers) have dual-ported GPRs.  This is not currently supported by the compiler.

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

In addition to the fields defined by the datasheet, there are also occassionally some additional overlaid fields taken from Microchip documentation such as the RCMT, RXCKP and SCKP fields above.  These may or may not be useful.  Sometimes they are alternate field names used in earlier PIC microcontrollers that may be helpful when studying source code examples in C or assembler.

## Combo SFR types

Often there are two or more adjacent SFRs for a hardware module that are combined together into a single variable - this is specified by a combo SFR type definition.  When a PIC contains multiple instances of the hardware module, multiple variables can be defined with a common type definition.  This type can be used to define a parameter to a subroutine which allows that subroutine to work with all instances of the hardware module.

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

### Special AD Result Fields

In addition to ADRES, ADRESH and ADRESL, the following special fields are defined for the analog to digital converter:

~~~
   ADRES_8L: uint8;
   ADRES_8R: uint8;
   ADRES_10L: uint10;
   ADRES_10R: uint10;
   ADRES_12L: uint12;
   ADRES_12R: uint12;
~~~

Each of these fields provides access to the exact bits of a particular configurable ADC result.  Depending on the microcontroller, ADCs can be configured to provide 8, 10 or 12 bit results either left or right justified.  Using one of these fields instead of the 16 bit ADRES field will allow the compiler to generate more compact code than if a 16-bit result field (ADRES) were used.

Note that the compiler does not automatically configure the ADC to load a specific result field, it assumes the programmer has previously done so before accessing that field.  Note also that only a few of the PICs have 12-bit ADCs - presence of a 12-bit ADRES field in the type definition does not guarantee that a particular PIC has a 12-bit ADC (see the datasheet!). 

<table style="border: 1px solid black; width: 100%; background: #212121;">
   <tr>
      <td style="padding: 25px;">
         <p style="color: #fff; font-size: 32px;">Help Wanted!</p>
         <p style="color: #fff;">Identifying useful combinations of SFRs is a manual process requiring knowledge of the PIC hardware module.  Given the large number of PIC hardware modules and our lack of experience with most of them, only a fraction of useful combo SFR types have been identified.  If you have experience with a particular hardware module and have suggetions for additional combo SFR types or special fields, please contact 
         <a href="mailto:cpc@davidhawk.us?subject=pic18 combo sfr suggestion" style="color: #0090FF; text-decoration: underline;">cp@davidhawk.us</a>
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
 

![](pic18x-user-guide/shared-sfrs.png){:hspace="50"}

The Concurrent Pascal compiler treats ADSHR as a 13th address bit ($1---).  The addresses for the ioreg variables using the SFRs in Table 5-4 above are (TMR1 and ODCON happen to be combo-SFRs each containing three SFRs):

~~~
ioreg
   OSCCON: tOSCCON at $FD3;
   REFOCON: tREFOCON at $1FD3;
   TMR1: tTMR16 at $FCD;
   ODCON: tODCON at $1FCD;
~~~
 
Note that the “13th address bit” is set for all “Alternate” SFRs in Table 5-4.  The compiler will generate the necessary code to set ADSHR when an alternate SFR is accessed and then clear it immediately after.  The programmer should never need to set or clear the ADSHR bit directly.

## Atomicity of ioreg operations

The compiler ensures that **individual ioreg <u>field</u>** operations are **atomic**.  That means that a stray interrupt will not compromise a single ioreg field read or write operation.  Interrupts are turned off for any ioreg field operation that takes more than a single instruction - this includes SFR reads, masking, shifts and writes for the single field (plus any necessary setting and clearing of ADSHR for alternate address SFRs).

Although each ioreg field operation is atomic, it should be noted that **sequences** of field operations can be interrupted and are not guaranteed to be atomic unless placed within a single process or monitor and all of those fields are accessed exclusively by that process or monitor.

## Order of Multi-Byte SFR Reads or Writes

For some combo SFRs the order in which individual SFR reads or writes are done is important.  An example is the 16-bit timer in many PIC18 microcontrollers.  This timer can be configured to count clock cycles.  Special provisions are required to allow a consistent snapshot of its rapidly changing 16-bit value to be correctly read or written byte-wise by several instructions over the 8-bit bus.  The following block diagram is of TMR1 from the PIC18F2520 datasheet:

![](pic18x-user-guide/tmr1-latches.png){:hspace="50"}

The blocks labeled “TMR1L” and “TMR1 High Byte” are 8-bit segments of the 16-bit timer.  The block labeled “TMR1H” is a latch provided by the chip designer to facilitate complete 16-bit transfers to or from the timer over the 8-bit data bus.  

To correctly read the entire 16-bit value TMR1L must be read first.  This simultaneously latches the current value in the upper byte of the timer into the TMR1H latch for later retrieval (by the time TMR1H is retrieved the upper byte of the actual timer may have changed).

Similarly, to correctly write a 16-bit value to TMR1 the upper byte is written into the TMR1H latch first and then when TMR1L is written both it and the latched value is simultaneously transferred to the actual timer counter.

For multi-byte SFR read operations the Concurrent Pascal compiler always emits code that reads the low byte first and writes the low byte last (this is true for both normal and reversed types).

# Process Priority Mapping

Process level 2 is mapped to the high priority interrupt level.  Processes and monitors running at priority level 2 run with interrupts off.

Process level 1 is mapped to the low priority interrupt level.  Processes and monitors running at priority level 1 run with the low priority interrupts off and high priority interrupts on.

Process and monitors running at levels 0 and below run with interrupts on.


# Interrupt Variables

The include files define a set of all possible interrupt variables for each microcontroller.  For example a microcontroller that implements TMR3 will have two interrupt variables, one for high priority (2) and one for low priority (1), similar to the following (PIC18F2520):

~~~
var
   TMR3I_prio2_interrupt:
      interrupt priority 2;
         function signaled: boolean;
            begin
               if (PIE2.TMR3IE = 1)
                  and
                  (PIR2.TMR3IF = 1)
               then
                  begin
                     PIR2.TMR3IF := 0;
                     result := true
                  end
            end;
      begin
         PIE2.TMR3IE := 1
      end;

   TMR3I_prio1_interrupt:
      interrupt priority 1;
         function signaled: boolean;
            begin
               if (PIE2.TMR3IE = 1)
                  and
                  (PIR2.TMR3IF = 1)
               then
                  begin
                     PIR2.TMR3IF := 0;
                     result := true
                  end
            end;
      begin
         IPR2.TMR3IP := 0;
         PIE2.TMR3IE := 1
      end;
~~~

Only those interrupt variables attached to an interrupt process in the program are instantiated.  All other interrupt variables in the include file are ignored.  

The signaled function tests the enable and interrupt flags and, if both are set, sets signaled to true and clears the interrupt flag (if it is clearable - see below).  The initial statement clears the priority flag for low priority interrupts and enables the interrupt.

Interrupts that do not have a priority bit run at high priority (2) and therefore do not have a low priority interrupt variable defined.

## Special Case Interrupt Variables

For most interrupts the interrupt flag is cleared simply by setting it to 0.  The code for this is  contained in the include file in the signaled function for the interrupt variable as in the examples above.  

For some special cases other action is required to clear the interrupt flag. A probably incomplete list of these special cases is: 

* IOCI (Interrupt on Change) - read or write PORTx to clear
* LINKI (Link Change Interrupt) - read PHIR register to clear
* PKTI
* RCI (USART Receive Interrupt) - read RCREG
* TXI (USART Transmit Interrupt) - write TXREG
* UERR

For these special cases no code is included in the interrupt variable signaled function to clear the flag bit since it wouldn't accomplish anything.  Instead the programmer **must** include code in the interrupt process after the await interrupt call that has the effect of clearing the interrupt flag.  <b>*Failure to do so will cause the program to hang!*</b>

# EEPROM Support

Many PIC18x microcontrollers have internal EEPROM.  The compiler currently supports up to 256 bytes of internal EEPROM.  Support for 1024 byte EEPROM spaces may implemented in the future. 

# ROM Constants

The compiler gathers all ROM constants and constant strings into the first 64K of ROM memory.  Ensuring that the upper byte of the 24-bit program memory address for ROM constants is always clear slightly reduces the amount of code required to support ROM constants and interrupt overhead (TBLPTRU never needs to be set or saved).

# Integer Math Support

Multi-byte integer math code is generated with sufficiently sized operands to handle each integer math operation in the Concurrent Pascal program.  These operations cannot overflow.  The results are range checked and the global ErrorCode variable is set for out-of-range results.  Any out-of-range result is replaced with an arbitrary legal value and the program continues.


# Floating Point Support

The PIC18x Concurrent Pascal compiler supports 32-bit real variables.  The floating point routines were adapted from the PIC17 routines developed by Frank J. Testa for Microchip.  This library is described in Microchip’s [Application Note 575: IEEE 754 Compliant Floating Point Routines](http://ww1.microchip.com/downloads/en/AppNotes/00575.pdf) and [Application Note 660: Floating Point Math Functions](http://ww1.microchip.com/downloads/en/AppNotes/00660.pdf).  The PIC17 routines were modified to be reentrant by using the PIC18x stack rather than absolute addresses.  Some minor changes were also necessary to account for differences between the PIC17 and PIC18 instruction sets.

All of the 32-bit math operations (add, subtract, multiply, etc.) from AN754 as well as the comparison functions from AN660 were incorporated into the Concurrent Pascal compiler.  AN660 also provides implementations of more advanced functions such a sin, cos, exp, etc but these are not implemented at present.  These may be incorporated in the future.

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
   transmit  o.b3..o.b0  to external computer in desired byte order
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

# Special Procedures & Functions

## reset_TMRn_cycle (cycle_count: uint16)

These procedures, defined for each 16-bit timer in the microcontroller, can be used to give accurate instruction counter-driven process cycle times.

~~~
var
   p: process priority 2;
         begin
            // initialize TMRn
            await interrupt;
            cycle 
               …
               await interrupt;
               reset_TMRn_cycle (10000);
               …
            repeat
         end interrupt TMRnI_interrupt2;
~~~

In this example the TMRn interrupt will occur exactly every 10,000 instruction cycles assuming that the timer is initialized as follows:

•	the timer is configured to its full width (16 bits),
•	the clock source is set to internal instruction clock,
•	the pre-scaler is disabled (x1),
•	the timer is enabled.

With this configuration the 16-bit timer will continuously count instruction cycles and generate an interrupt when the count reaches $FFFF.  The timer overflows to $0000 and keeps counting.  The reset_TMRn_cycle routine subtracts the cycle_count parameter (with a small adjustment for the number of instruction cycles necessary to implement the subtraction) from the current timer value to reset it below $FFFF again.  The next counter overflow and interrupt will occur exactly cycle_count instruction cycles after the previous timer overflow and interrupt.

The controlling TMRn interrupt cycle will be exactly 10,000 instructions in the above example, however there will still be some jitter for the instructions within the process cycle itself due to other parts of the Concurrent Pascal program running at higher priority or with interrupts off.

Cycle times that are too low for a given application will result in frequent or infrequent “TMRn cycle count exceeded” error code.  The error code is generated when the subtraction of cycle_count from the timer yields a result that is still above $0000.  In such cases process priorities will need to be juggled or a longer cycle time chosen.  It may be good practice to experimentally lower this cycle time until the error code begins to appear and then raise it to a higher value to give some margin.

The first await interrupt statement (the one before the cycle statement) in the above example is to prevent an extraneous “TMRn cycle count exceeded” error code at the first iteration due to the timer being in an unknown state due to a possibly lengthy system initialization time in a given application.  This pattern can result in two complete cycles of the timer (at 65,536 instruction cycles each) occurring after reset before the desired cycling time commences.

## ClearWatchdogTimer

Clears the watchdog timer. 
