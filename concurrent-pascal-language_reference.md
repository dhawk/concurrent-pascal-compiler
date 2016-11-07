# Global ErrorCode Variable

PIC18 microcontrollers normally do not operate in an environment conducive to extensive run-time error reporting, however a rudimentary error reporting capability is supplied in the form of a single global ErrorCode that can be set when a run-time error occurs.  This capability is used extensively by the compiler and kernel to detect and report run-time errors.  The assert procedure can also be used by the programmer to report errors - it will set ErrorCode if the condition is not true:

~~~
assert (<boolean>, 'Error Message')
~~~

The global ErrorCode variable is a uint24 value that holds the program counter value for the location where the error occurred.  The Error Message itself is not embedded anywhere in the Microcontroller’s ROM but is instead saved in a file named <program_name>.err which lists all possible error locations and the associated error messages.  The .err file is only valid for the assembly and hex files produced at the same time by the compiler.

The first run-time error to occur sets ErrorCode.  After ErrorCode is set subsequent run-time errors do not overwrite it.  This prevents a cascade of subsequent erors from overwriting the initial error code.

Setting ErrorCode does not abort anything, the program continues running.  The idea is to allow the program to limp on long enough to somehow transmit the ErrorCode back to the programmer so that the underlying bug can be fixed before the code is released.  Ideally ErrorCode will never be set in released code.

The ErrorCode variable may be directly read by Concurrent Pascal code for reporting – perhaps via a communications link or displayed in LEDs.  Reading the ErrorCode variable clears it, after which the next run-time error can set it again.
