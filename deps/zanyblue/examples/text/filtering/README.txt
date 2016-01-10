This example shows how to use a custom message filter to handle verbose vs.
non-verbose messages based on the convention that message keys which begin
with the digit '2' are verbose messages, other messages are informational
or error/warning messages.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -G wprints -i -S messages.stamp -v Messages Xmpl
This is ZBMCompile, Version 1.2.0 BETA (r2803M) on 11/6/13 at 1:15 PM
Copyright (c) 2009-2013, Michael Rohan.  All rights reserved
Loaded 4 messages for the facility "Xmpl" (1 locales)
Performing consistency checks for the facility "Xmpl"
Performing consistency checks for the accessor package generation
Loaded 1 facilities, 4 keys, 1 locales and 4 messages
Loaded total 156 characters, stored 156 unique characters, 0% saving
Wrote the spec "Messages" to the file "./messages.ads"
Wrote the body "Messages" to the file "./messages.adb"
Generated accessor package spec "Messages.Xmpl_Wide_Prints" to "./messages-xmpl_wide_prints.ads"
Generated accessor package body "Messages.Xmpl_Wide_Prints" to "./messages-xmpl_wide_prints.adb"
Created the stamp file "messages.stamp" containing the time stamp 11/6/13, 1:15 PM
ZBMCompile completed on 11/6/13 at 1:15 PM, elapsed time 0:00:00.011
gnatmake -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -aP../../../lib/gnat -aP../../../src -P x_filtering.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 -I- -gnatA /u/mrohan/zb/examples/text/filtering/x_filtering.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 -I- -gnatA /u/mrohan/zb/examples/text/filtering/messages.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 -I- -gnatA /u/mrohan/zb/examples/text/filtering/messages-xmpl_wide_prints.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 -I- -gnatA /u/mrohan/zb/examples/text/filtering/my_filter.adb
gnatbind -x /u/mrohan/zb/examples/text/filtering/x_filtering.ali
gnatlink /u/mrohan/zb/examples/text/filtering/x_filtering.ali /u/mrohan/zb/lib/zanyblue/libzanyblue.a -Wl,-rpath,/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/4.7.4/adalib/ -o /u/mrohan/zb/bin/x_filtering

-----------------------------------------------------------------------------
$ make run
gnatmake -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -aP../../../lib/gnat -aP../../../src -P x_filtering.gpr
gnatmake: "/u/mrohan/zb/bin/x_filtering" up to date.
../../../bin/x_filtering
Error, this is an example error message
Warning, this is an example warning message
This is the example message filtering application

$ make run_v
gnatmake -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -aP../../../lib/gnat -aP../../../src -P x_filtering.gpr
gnatmake: "/u/mrohan/zb/bin/x_filtering" up to date.
../../../bin/x_filtering -v
Error, this is an example error message
Warning, this is an example warning message
This is the example message filtering application
This is a verbose message

