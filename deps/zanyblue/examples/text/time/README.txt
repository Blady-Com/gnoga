This example is a simple text based application that prints the current time
and date is various CLDR defined formats, short, medium, long, etc.  The
messages are externalized to the file "curtime.properties" which is first
compiled to the Ada package Curtime_Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -v Curtime_Messages curtime
This is ZBMCompile, Version 0.1.0 ALPHA (r1462) at 9:05 AM on 8/24/10
Copyright (c) 2009-2010, Michael Rohan.  All rights reserved
Loaded 10 messages for the facility "curtime" (1 locales)
Loaded 1 facilities, 10 keys, 1 locales and 10 messages
Created the spec "Curtime_Messages" in the file "curtime_messages.ads"
Created the body "Curtime_Messages" in the file "curtime_messages.adb"
ZBMCompile completed at 9:05 AM on 8/24/10, elapsed time 0:00:00.056
gprbuild -p -aP../../../lib -P xcurtime.gpr
creating auto.cgpr
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 xcurtime.adb
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 curtime_messages.adb
gprbind xcurtime.bexch
gnatbind xcurtime.ali
gcc-4.4 -c b__xcurtime.adb
gcc-4.4 xcurtime.o -o xcurtime

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -P xcurtime.gpr
creating auto.cgpr
gprbuild: "xcurtime" up to date
../../../bin/xcurtime
Current time (short):       9:05 AM
Current time (medium):      9:05:36 AM
Current time (long):        9:05:36 AM -0700
Current time (full):        9:05:36 AM -0700
Current date (short):       8/24/10
Current date (medium):      Aug 24, 2010
Current date (long):        August 24, 2010
Current date (full):        Tuesday, August 24, 2010
Current date/time (locale): 9:05 AM 8/24/10
Current date/time (custom): Tue Aug 24 09:05:36 2010

$ make run_ru
gprbuild -p -aP../../../lib -P xcurtime.gpr
creating auto.cgpr
gprbuild: "xcurtime" up to date
ZB_LANG=ru ../../../bin/xcurtime
Current time (short):       9:05
Current time (medium):      9:05:50
Current time (long):        9:05:50 -0700
Current time (full):        9:05:50 -0700
Current date (short):       24.08.10
Current date (medium):      24.08.2010
Current date (long):        24 августа 2010 г.
Current date (full):        вторник, 24 августа 2010 г.
Current date/time (locale): 9:05 24.08.10
Current date/time (custom): Вт авг. 24 09:05:50 2010
