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
../../../bin/zbmcompile -G prints -i -S messages.stamp -v Messages Xmpl
This is ZBMCompile, Version 1.3.0 BETA (r3038:3039M) on 7/30/16 at 12:18 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 5 messages for the facility "Xmpl" (1 locales)
Performing consistency checks for the facility "Xmpl"
Performing consistency checks for the accessor package generation
Loaded 1 facilities, 5 keys, 1 locales and 5 messages
Loaded total 200 characters, stored 200 unique characters, 0% saving
Wrote the spec "Messages" to the file "./messages.ads"
Wrote the body "Messages" to the file "./messages.adb"
Generated accessor package spec "Messages.Xmpl_Prints" to "./messages-xmpl_prints.ads"
Generated accessor package body "Messages.Xmpl_Prints" to "./messages-xmpl_prints.adb"
Created the stamp file "messages.stamp" containing the time stamp 7/30/16, 12:18 PM
ZBMCompile completed on 7/30/16 at 12:18 PM, elapsed time 0:00:00.022
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_filtering.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 zbx_filtering.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 messages.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 messages-xmpl_prints.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 my_filter.adb
gprbind zbx_filtering.bexch
gnatbind zbx_filtering.ali
gcc -c b__zbx_filtering.adb
gcc zbx_filtering.o -o zbx_filtering

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_filtering.gpr
gprbuild: "zbx_filtering" up to date
../../../bin/zbx_filtering
This is FILTERING, Version 1.3.0 - BETA
This is the example message filtering application
Warning, this is an example warning message
Error, this is an example error message

$ make run_v
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_filtering.gpr
gprbuild: "zbx_filtering" up to date
../../../bin/zbx_filtering -v
This is FILTERING, Version 1.3.0 - BETA
This is a verbose message
This is the example message filtering application
Warning, this is an example warning message
Error, this is an example error message
