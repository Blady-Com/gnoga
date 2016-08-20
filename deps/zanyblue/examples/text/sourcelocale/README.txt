This example is a simple text based application that prints the current day
name with and without source message locales enabled.  The day name printed
is either in English (regardless of the selected locale) if source message
locales is enabled or the localized day name if source message locales is
disabled.

It is normally a better user experience to have consistent language usage
within message (this is the default handling for arguments, i.e., source
message locales is enabled).

Following the standard ZB convention, example applications are prefixed
with "zbx".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -G prints -s en -v SLocale_Messages Day
This is ZBMCompile, Version 1.3.0 BETA (r3038:3039M) on 7/30/16 at 12:30 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 3 messages for the facility "Day" (1 locales)
Performing consistency checks for the facility "Day"
Performing consistency checks for the accessor package generation
Loaded 1 facilities, 3 keys, 2 locales and 3 messages
Loaded total 145 characters, stored 145 unique characters, 0% saving
Wrote the spec "SLocale_Messages" to the file "./slocale_messages.ads"
Wrote the body "SLocale_Messages" to the file "./slocale_messages.adb"
Generated accessor package spec "SLocale_Messages.Day_Prints" to "./slocale_messages-day_prints.ads"
Generated accessor package body "SLocale_Messages.Day_Prints" to "./slocale_messages-day_prints.adb"
ZBMCompile completed on 7/30/16 at 12:30 PM, elapsed time 0:00:00.012
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_slocale.gpr
gcc -c -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 zbx_slocale.adb
gcc -c -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 slocale_messages.adb
gcc -c -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 slocale_messages-day_prints.adb
gprbind zbx_slocale.bexch
gnatbind zbx_slocale.ali
gcc -c b__zbx_slocale.adb
gcc zbx_slocale.o -o zbx_slocale

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_slocale.gpr
gprbuild: "zbx_slocale" up to date
../../../bin/zbx_slocale
This is SOURCELOCALE, Version 1.3.0 - BETA
Source message locale enabled:  Today is Saturday
Source message locale disabled: Today is Saturday

$ make run_ru
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_slocale.gpr
gprbuild: "zbx_slocale" up to date
../../../bin/zbx_slocale -lru
This is SOURCELOCALE, Version 1.3.0 - BETA
Source message locale enabled:  Today is Saturday
Source message locale disabled: Today is суббота
