This example is a simple text based application that prints the current day
name with and without source message locales enabled.  The day name printed
is either in English (regardless of the selected locale) if source message
locales is enabled or the localized day name if source message locales is
disabled.

It is normally a better user experience to have consistent language usage
within message (this is the default handling for arguments, i.e., source
message locales is enabled).

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -G wprints -s en -v SLocale_Messages Day
This is ZBMCompile, Version 0.2.0 ALPHA (r2557:2560M) at 5:36 PM on 1/16/12
Copyright (c) 2009-2012, Michael Rohan.  All rights reserved
Loaded 2 messages for the facility "Day" (1 locales)
Performing consistency checks for the facility "Day"
Performing consistency checks for the accessor package generation
Loaded 1 facilities, 2 keys, 2 locales and 2 messages
Loaded total 98 characters, stored 98 unique characters, 0% saving
Wrote the spec "SLocale_Messages" to the file "./slocale_messages.ads"
Wrote the body "SLocale_Messages" to the file "./slocale_messages.adb"
Generated accessor package spec "SLocale_Messages.Day_Wide_Prints" to "./slocale_messages-day_wide_prints.ads"
Generated accessor package body "SLocale_Messages.Day_Wide_Prints" to "./slocale_messages-day_wide_prints.adb"
ZBMCompile completed at 5:36 PM on 1/16/12, elapsed time 0:00:00.042
gnatmake -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -aP../../../lib -aP../../../src -P x_slocale.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 -I- -gnatA /home/mrohan/zb/effectivelocale/examples/text/sourcelocale/x_slocale.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 -I- -gnatA /home/mrohan/zb/effectivelocale/examples/text/sourcelocale/slocale_messages.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 -I- -gnatA /home/mrohan/zb/effectivelocale/examples/text/sourcelocale/slocale_messages-day_wide_prints.adb
gnatbind -I- -x /home/mrohan/zb/effectivelocale/examples/text/sourcelocale/x_slocale.ali
gnatlink /home/mrohan/zb/effectivelocale/examples/text/sourcelocale/x_slocale.ali -L/home/mrohan/zb/effectivelocale/lib/ -lzanyblue -Wl,-rpath,/usr/gnat/lib/gcc/i686-pc-linux-gnu/4.3.6/adalib/ -o /home/mrohan/zb/effectivelocale/bin/x_slocale

-----------------------------------------------------------------------------
$ make run
gnatmake -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -aP../../../lib -aP../../../src -P x_slocale.gpr
gnatmake: "/home/mrohan/zb/effectivelocale/bin/x_slocale" up to date.
../../../bin/x_slocale
Source message locale enabled:  Today is Monday
Source message locale disabled: Today is Monday

$ make run_ru
gnatmake -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -aP../../../lib -aP../../../src -P x_slocale.gpr
gnatmake: "/home/mrohan/zb/effectivelocale/bin/x_slocale" up to date.
../../../bin/x_slocale -lru
Source message locale enabled:  Today is Monday
Source message locale disabled: Today is понедельник
