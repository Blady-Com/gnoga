This example is a simple text based application that prints a few in-line
messages using the formatting functionality of ZanyBlue.Text.  The messages
are not externalized: they used in the same fashion as C print-style format
strings.  Since the ZanyBlue.Text package was created to support localization,
using non-externalized strings, as in this example, defeats this purpose.

This final message printed is expected to raise an exception.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
gprbuild -p -aP../../../lib -P xsimple.gpr
creating auto.cgpr
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 xsimple.adb
gprbind xsimple.bexch
gnatbind xsimple.ali
gcc-4.4 -c b__xsimple.adb
gcc-4.4 xsimple.o -o xsimple

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -P xsimple.gpr
creating auto.cgpr
gprbuild: "xsimple" up to date
../../../bin/xsimple
This is TEXT_XMPL V1.0.0, ALPHA
Here is an "a value" embedded string
1964 base 16 in a field of width 10 is |       7ac|
Expect a missing argument exception for the following

raised ZANYBLUE.TEXT.NO_SUCH_ARGUMENT_ERROR : 1
make: *** [run] Error 1
