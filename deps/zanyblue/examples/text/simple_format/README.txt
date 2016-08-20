This example is a simple text based application that prints a few in-line
messages using the formatting functionality of ZanyBlue.Text.  The messages
are not externalized: they used in the same fashion as C print-style format
strings.  Since the ZanyBlue.Text package was created to support localization,
using non-externalized strings, as in this example, defeats this purpose.

This final message printed is expected to raise an exception.

Following the standard ZB convention, example applications are prefixed
with "zbx".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_simple.gpr
gcc -c -g -O3 -gnata -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 zbx_simple.adb
gcc -c -g -O3 -gnata -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 definitions.ads
gprbind zbx_simple.bexch
gnatbind zbx_simple.ali
gcc -c b__zbx_simple.adb
gcc zbx_simple.o -o zbx_simple

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_simple.gpr
gprbuild: "zbx_simple" up to date
../../../bin/zbx_simple
This is SIMPLE_FORMAT, Version 1.3.0 - BETA
X is 32100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0
Here is an "a value" embedded string
1964 base 16 in a field of width 10 is |       7ac|
A NaN: NaN
Expect a missing argument exception for the following
The exception "ZANYBLUE.TEXT.NO_SUCH_ARGUMENT_ERROR" was raised
