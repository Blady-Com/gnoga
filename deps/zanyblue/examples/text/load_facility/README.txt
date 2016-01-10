This example is a simple text based application that loads a .properites
file at run time and display the small set of messages defined by it.  The
generated application must be run in the same directory as the .properties
file, i.e., this directory: the example does not include logic to locate
.properties files.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
gprbuild -p -aP../../../lib -P xload_facility.gpr
creating auto.cgpr
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 xload_facility.adb
gprbind xload_facility.bexch
gnatbind xload_facility.ali
gcc-4.4 -c b__xload_facility.adb
gcc-4.4 xload_facility.o -o xload_facility

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -P xload_facility.gpr
creating auto.cgpr
gprbuild: "xload_facility" up to date
../../../bin/xload_facility
This is TEXT_XMPL V1.0.0, ALPHA
Loaded 3 messages for 1 locale(s)
