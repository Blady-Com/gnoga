This example is a simple text based application that loads a .properites
file at run time and display the small set of messages defined by it.  The
generated application must be run in the same directory as the .properties
file, i.e., this directory: the example does not include logic to locate
.properties files.

Following the standard ZB convention, example applications are prefixed
with "zbx".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_load_facility.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 zbx_load_facility.adb
gprbind zbx_load_facility.bexch
gnatbind zbx_load_facility.ali
gcc -c b__zbx_load_facility.adb
gcc zbx_load_facility.o -o zbx_load_facility

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_load_facility.gpr
gprbuild: "zbx_load_facility" up to date
../../../bin/zbx_load_facility
This is LOAD_FACILITY, Version 1.3.0 - BETA
Loaded 3 messages for 1 locale(s)
