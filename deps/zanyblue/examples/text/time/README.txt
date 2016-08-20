This example is a simple text based application that prints the current time
and date is various CLDR defined formats, short, medium, long, etc.  The
messages are externalized to the file "curtime.properties" which is first
compiled to the Ada package Curtime_Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "zbx".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -v Curtime_Messages curtime
This is ZBMCompile, Version 1.3.0 BETA (r3038:3039M) on 7/30/16 at 12:30 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 12 messages for the facility "curtime" (1 locales)
Performing consistency checks for the facility "curtime"
Loaded 1 facilities, 12 keys, 1 locales and 12 messages
Loaded total 664 characters, stored 664 unique characters, 0% saving
Wrote the spec "Curtime_Messages" to the file "./curtime_messages.ads"
Wrote the body "Curtime_Messages" to the file "./curtime_messages.adb"
ZBMCompile completed on 7/30/16 at 12:30 PM, elapsed time 0:00:00.017
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_curtime.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 zbx_curtime.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 curtime_messages.adb
gprbind zbx_curtime.bexch
gnatbind zbx_curtime.ali
gcc -c b__zbx_curtime.adb
gcc zbx_curtime.o -o zbx_curtime

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_curtime.gpr
gprbuild: "zbx_curtime" up to date
../../../bin/zbx_curtime
This is CURTIME, Version 1.3.0 - BETA
Current time (short):       12:31 PM
Current time (medium):      12:31:00 PM
Current time (long):        12:31:00 PM -0700
Current time (full):        12:31:00 PM -0700
Current date (short):       7/30/16
Current date (medium):      Jul 30, 2016
Current date (long):        July 30, 2016
Current date (full):        Saturday, July 30, 2016
Current date/time (locale): 7/30/16, 12:31 PM
Current date/time (custom): Sat Jul 30 12:31:00 2016

$ make run_ru
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_curtime.gpr
gprbuild: "zbx_curtime" up to date
../../../bin/zbx_curtime -lru
This is CURTIME, Version 1.3.0 - BETA
Current time (short):       12:31
Current time (medium):      12:31:04
Current time (long):        12:31:04 -0700
Current time (full):        12:31:04 -0700
Current date (short):       30.07.16
Current date (medium):      30 июля 2016 г.
Current date (long):        30 июля 2016 г.
Current date (full):        суббота, 30 июля 2016 г.
Current date/time (locale): 30.07.16, 12:31
Current date/time (custom): сб июля 30 12:31:04 2016
