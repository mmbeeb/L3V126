# L3V126
Acorn Level 3 File Server Version 1.26 

Version 1.06 source code (uploaded to stardot forum by Alan Williams), reformatted for BeebAsm 1.09, updated to version 1.24,
with further changes made by myself, some of which are:

Improvements made to the time/date functions which now handle 21st century dates.
No longer are there separate versions for the dongle etc.
Instead, it first tests for the dongle.  If that fails it tries OSWORD 14 to read any onboard clock.
If that fails, it asks the user to enter the data & time, and maintains it using the system clock.

Further byte saving code changes.

Some slight changes to how the network is dealt with as to improve how my FSEM emulator will deal with network rx/tx timing.

To assemble, in a Command Prompt window (Windows), select directory, and enter: beebasm -i FS3.asm

(I put a copy of beebasm.exe in the directory.)
