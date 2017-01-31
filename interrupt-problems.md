---
layout: default
title: The Interrupt Problem
---

<h1><center>{{title}}</center></h1>

Interrupts are a mechanism by which a hardware module signals the CPU that an operation has completed.  An embedded system that correctly utilizes interrupts can be very responsive since the appropriate software is tightly coupled to the hardware it controls.

This article will examine three approaches to interrupts:

1. **Busy-loop Approach:** here interrupts are avoided altogether.  This is often advised for users of traditional non-concurrent programming languages such as C or Basic.  This is fairly safe if performance requirements are modest.
2. **Interrupt Subroutine Approach:** Some traditional non-concurrent programming languages such as C or Basic implement "interrupt subroutines" that are "called by the hardware" when an interrupt occurs.  This can work but requires careful attention to detail and a keen understanding of the potential pitfalls.
3. **Concurrent Pascal:** Concurrent Pascal makes interrupts easy.  It supports multiple cyclic processes with multiple priority levels. A process may be tightly coupled to a hardware module by using the interrupt as the synchronization mechanism.  A high priority interrupt process will respond quickly when its associated hardware operation completes and will preempt lower priority processes already running. Monitors provide the means for safe inter-process communication.

To illustrate these approaches we will implement a simplistic fragment of a cruise control module with a PIC18 microcontroller. The PIC's Analog to Digital Converter (ADC) will be utilized to measure the vehicle's velocity in units of inches per second (in/sec).  A timer (TMR0) will be used to time periodic updates of the current velocity.  A `control_throttle` routine will calculate the throttle setting based on the desired speed and the current velocity.  *In these highly simplified examples many necessary details will be omitted in order to focus attention on the essential points.*

## The Busy Loop Approach  

Users of traditional non-concurrent programming languages such as C and Basic are often advised to forgo using interrupts altogether and instead implement a single-threaded program that busy-loops around looking at hardware completion flags and running a handler when a flag is set.  

The outline of a cruise control module implemented with this approach would look something like this:

~~~
void control_throttle (int vel)
{
    ...    
}

int main(int argc, char** argv)
{
    int velocity;
    ...
    while(1)
    {   
        if (INTCONbits.T0IF)   // test TMR0 flag
        {
            INTCONbits.T0IF = 0;
            velocity = (ADRESH<<8) + ADRESL;
            ...
        }
        if (hw_module_2.flag)
        {
            hw_module_2.flag = 0; 
            ... hw module 2 handler ...
        }
        ...
        control_throttle (velocity);
        ...
    }
}
~~~

The first if statement in the main loop checks the TMR0 flag.  If the flag is set it means that the periodic ADC measurement has completed and the velocity value can be read from the ADRES registers[^shift-needed].

[^shift-needed]: The byte-level operations are needed here to construct a 16-bit value because the XC8 compiler is little-endian and the ADRESH and ADRESL SFRs are arranged in big-endian order in the PIC18 hardware.

For illustrative purposes a second hardware module flag is tested and handled in the second if statement of the main loop.

Lastly the `control_throttle` subroutine is called with the latest velocity reading.

The busy-loop approach can be adequate for simple applications but may be insufficient for hardware modules requiring quick response times.  A common problem will be that some other code (a low priority hardware handler or the control_throttle calculation) is already executing when a high priority hardware operation completes, but the high priority hardware flag won't even be examined until the other code finishes and the main loop cycles back around to look at it.  Ideally the higher priority handler would run immediately, but in a single threaded language that is difficult to do without resorting to spaghetti code.

## The Interrupt Subroutine Approach

Although C and Basic are single-threaded (non-concurrent) languages, some implementations provide an "interrupt subroutine" that can be "called from the hardware" in response to an interrupt.  The compiler generates special code for these interrupt subroutines that saves all CPU registers on entry and re-loads them on exit.  When an interrupt occurs the main program thread is briefly suspended while the interrupt subroutine runs and then resumed when it completes.  The register saving guarantees that the interupted main program thread is not corrupted.  Adding interrupt subroutines to a single-threaded language provides a limited concurrency capability.

An abbreviated outline of the cruise control embedded software using this approach is as follows:

~~~
volatile int velocity;

void interrupt low_priority __isr_handler(void) 
{
    if (INTCONbits.TMR0IE && INTCONbits.T0IF)  // TMR0 interrupt?
    {
        INTCONbits.T0IF = 0;
        velocity = (ADRESH<<8) + ADRESL;
        ...
    }
}

void control_throttle (int vel)
{
    ...    
}

int main(int argc, char** argv)
{
    ...
    while(1)
    {   
        ...
        control_throttle (velocity);
        ...
    }
}
~~~

The most recent velocity measurement is kept in a global variable `velocity` *(more on* `volatile` *later).*

TMR0 has been set up to interrupt upon the completion of each velocity measurement period *(details not shown)*.  Upon each of these interrupts the ADC value is saved to the global `velocity` variable[^shift-needed].

The main code continuously updates the throttle setting based on the latest velocity reading and desired speed setting.

Seems simple enough. *What could possibly go wrong?*

We'll start by examining the assembly code generated by Microchip's XC8 compiler for the call to `control_throttle` in `main`.  Here the current value of the velocity variable is transferred to the location of the parameter variable `vel` and then the subroutine itself is called:

~~~
                  ;example.c: control_throttle (velocity);
0000A0  C016  F00F   movff	_velocity,control_throttle@vel      
0000A4  C017  F010   movff	_velocity+1,control_throttle@vel+1  
0000A8  EC5A  F000   call	_control_throttle	
~~~

In this code fragment the LSB is transferred in the first movff instruction and the MSB is transferred in the second[^little-endian].

[^little-endian]: The XC8 compiler is little-endian.  The LSB is stored at the variable's address and the MSB is stored at address+1.

Now suppose the vehicle is traveling at 20 mph and the cruise control setting is turned down to 10 mph.  Imagine that the ADC read interrupt randomly happens to occur between the two movff instructions just as the vehicle is slowing from 14.55 mph ($0100 in/sec) to 14.49 mph ($00FF in/sec). The following sequence of operations will occur:

- the `velocity` variable already contains the previous value of $0100 in/sec.
- the first movff instruction has just set the LSB of the `control_throttle` parameter to $00.
- the interrupt occurs and a new value of $00FF in/sec is written to the `velocity` variable.
- the second movff instruction sets the MSB of the `control_throttle` parameter to $00.
- the `control_throttle` routine is called with a parameter value of $0000.

You can imagine what the `control_throttle` routine might do.  It has a setpoint of 10 mph and is being told the vehicle is currently stopped (even though it is actually moving at over 14 mph) - the throttle is opened wide and the vehicle lurches forward...  *Unintended acceleration, anyone?*

This is the essential problem with interrupts.  An interrupt that occurs at the wrong time in the wrong place can lead to unpredictable results.  The programmer needs to realize that conventional testing is unlikely to reveal this kind of bug.  One could have tested this many times without the interrupt ever occurring at the critical spot between the two movff instructions.  Even if the interrupt does occur at the critical point during testing it will usually appear benign - joining the MSB of one reading to the LSB of the previous reading will often yield a "reasonable" value that might not be noticed in the testing.  Once deployed, however, the system may occasionally *glitch* and behave erratically and the product may achieve an undesired reputation for being *flaky* or worse.  

> ***Interrupts should be the first of the usual suspects examined when glitchy or flaky behavior is observed in an embedded system using interrupt subroutines.***

In preventing this kind of problem the programmer must first recognize that the `velocity` variable is shared between two concurrent sites of activity (the main thread and the interrupt subroutine).  In an embedded system all such concurrently shared variables will need to be identified.  In this simple example the `velocity` variable consists of only two bytes, but often concurrently shared data structures will be more complex.

A change to a concurrently shared data structure constitutes a *critical section* that needs to be performed as an *atomic* (indivisible) operation.  This operation must be implemented so as to ensure that the variable is internally consistent after each atomic  change.  In the cruise control example the two movff instructions constitute a critical section that must be performed atomically[^16bit_solution].

[^16bit_solution]: If this exact same program was implemented on a CPU with 16 bit words the problem would be "solved" since only a single instruction would be required to read the ADC value and the single instruction would be atomic. However the general problem remains for non-trivial concurrently shared data structures.

There are several mechanisms by which a programmer can correctly protect critical sections for concurrent data sharing.  One can surround the critical sections with semaphores, spin locks, or simply turn interrupts off.  This does require that the programmer understands the problem, correctly identifies all critical sections, and implements a consistent mechanism to protect each shared data structure.  This can be difficult enough for a lone programmer, but is even more difficult in a large system with multiple programmers, or when the initial programmer(s) on a project are gone and a new programmer inherits the project at a later time.  

### Why not C?

C is a low level language that was originally developed as an assembler replacement that provides constructs that map efficiently to typical CPU instructions.  It was popular because it was widely available and supported many computer architectures.  Some implementations provide interrupt subroutines.  These interrupt subroutines add primitive concurrency but without a mechanism for concurrent data sharing.  The widely misunderstood `volatile` keyword does **not** provide that mechanism[^volatile].  Indeed the XC8 compiler generates exactly the same code in the critical section examined above whether the velocity variable is declared as volatile or not.  

The C language has no means of identifying concurrently accessed data structures and therefore the compiler cannot automatically identify critical sections and insert the necessary protection mechanisms.  This must be done by the programmer.  A C program with incomplete, inconsistent, or totally missing critical section code will happily compile with no error messages or warnings.

[^volatile]: See the discussion of the `volatile` keyword in C and C++: <https://en.wikipedia.org/wiki/Volatile_(computer_programming)>

## Concurrent Pascal

Now we will examine how the above problem would be implemented using Concurrent Pascal.  First we identify the sites of concurrency in our cruise control implementation.  In this case there are two: the `ADC_reader` (the interrupt routine above) and `main`.  Each of these will be implemented as a *cyclic sequential process*.  Then we will identify the shared data - in this case the `velocity` reading.  This will be implemented as a *monitor*.  Thus our example will be implemented with three components - two processes and a monitor. Below is a directed graph showing the *access rights* for these three components:

![](interrupt-problems/access-rights.png)

First we will examine the monitor code:

~~~
type
   t_velocity =
      monitor
         var v: int16;
      public
         property velocity: int16;
            set: begin v := velocity end;
            get: begin velocity := v end;
      begin
      end;

var
   velocity: t_velocity;
~~~

The monitor keeps the current velocity value in private variable `v`.  It can only be read or written through the `velocity` property.  Each set or get operation is a critical section and the compiler generates appropriate code to ensure that the  operation is atomic.

Next we will look at the ADC reader process:

~~~
var
   adc_reader:
      process (v: t_velocity); priority 1;
      begin
         ...
         cycle
            ...
            await interrupt;
            v.velocity := AD.ADRES;
            ...
         repeat
      end interrupt TMR0I_prio1_interrupt;
~~~

This is an example of an interrupt process.  It accomplishes the same thing as the interrupt subroutine but has an important paradigm shift.  Instead of being a subroutine "called by  the hardware" it is instead a cyclic sequential process synchronized with the hardware via the interrupt (interrupts are best understood as being a signal that the hardware has completed an operation).  At the beginning of the cycle the ADC is set up to perform a measurement *(details not shown)*. The process thread then blocks at the `await interrupt` statement until the TMR0 interrupt signals that the ADC measurement is ready.  At that point the ADC reading[^either-endian-SFRs] is written to the velocity monitor (passed as to the process as parameter `v` at system initialization) and then the cycle repeats.  The specific interrupt an interrupt process is synchronized to is specified at the end of the process variable type declaration (here `TMR0I_prio1_interrupt`).  This process runs at priority 1 which is equivalent to the PIC18 low priority interrupt (the high priority interrupt is priority 2).

[^either-endian-SFRs]: The PIC18x Concurrent Pascal compiler can handle either big-endian or little-endian SFR layouts (both can occur in the same microcontroller!), so the byte shifting required by XC8 is unnecessary.

Next we will look at the main process.

~~~
var
   main:
      process (v: t_velocity); priority 0;
         procedure control_throttle (v: int16);
            begin
               ...
            end;
         begin
            cycle 
               ...
               control_throttle (v.velocity);
               ... 
            repeat
         end; 
~~~

It runs at a lower priority (priority 0) than the interrupt process.  It too is a cyclic sequential process that loops continuously feeding the latest velocity measurement to the `control_throttle` algorithm.

A component has access to another component only if it is passed as a parameter in an init statement. The three components are connected to each other in the system initialization section of the program:

~~~
begin
   init velocity;
   init adc_reader (velocity);
   init main (velocity)
end.
~~~

In Concurrent Pascal there is no such thing as a global variable with unrestricted concurrent access.  Instead the only access to shared data is via monitors.  This is important because the compiler can correctly generate consistent and safe shared variable access code for all critical sections.  A Concurrent Pascal program that attempts unsafe shared variable access via global variables won't even compile!

>  ***There are no global variables involved in inter-process communication in a Concurrent Pascal program.***

<hr>

### Footnotes:
  

    



 

