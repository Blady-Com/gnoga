This example is a simple encoding conversion utility.  It converts an
input file with input encoding to an output file with a, likely, different
output encoding.  The input encoding is specified via the "-f" command line
option (from), the output encoding is sepcified via the "-t" command line
option (to), the input file name is the first non-option command line argument,
the output is the second non-option command line argument, e.g., to convert
the file "test.dat" encoded using "ISO8859-4" to the output file "test.out"
encoded as "UTF-8" the command line would be:

    $ zbx_econv -f ISO8859-4 -t UTF-8 test.dat test.out

WARNING: This is an example of encoding/decoding strings.  It is not an
efficient implementation as the entire input file is read as a single
string, decoded from input encoding, encoded to the output encoding and
then written to the output file.  This make the code simple to understand
but should not be used for general file encoding handling.

Following the standard ZB convention, example applications are prefixed
with "zbx".

This example has no third party dependencies.

Typical build

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -O -G prints -G exceptions -i -v Messages -d mesg Econv
This is ZBMCompile, Version 1.4.0 BETA (r3052M) on 8/5/16 at 10:55 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 9 messages for the facility "Econv" (1 locales)
Performing consistency checks for the facility "Econv"
Performing consistency checks for the accessor package generation
Optimizing the catalog for locale order access
Accumulated messages for the locale "" (9 messages)
Loaded 1 facilities, 9 keys, 1 locales and 9 messages
Loaded total 435 characters, stored 435 unique characters, 0% saving
Wrote the spec "Messages" to the file "./messages.ads"
Wrote the body "Messages" to the file "./messages.adb"
Generated accessor package spec "Messages.Econv_Exceptions" to "./messages-econv_exceptions.ads"
Generated accessor package body "Messages.Econv_Exceptions" to "./messages-econv_exceptions.adb"
Generated accessor package spec "Messages.Econv_Prints" to "./messages-econv_prints.ads"
Generated accessor package body "Messages.Econv_Prints" to "./messages-econv_prints.adb"
ZBMCompile completed on 8/5/16 at 10:55 PM, elapsed time 0:00:00.046
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=4 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_econv.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 zbx_econv.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 messages.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 messages-econv_exceptions.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 messages-econv_prints.adb
gprbind zbx_econv.bexch
gnatbind zbx_econv.ali
gcc -c b__zbx_econv.adb
gcc zbx_econv.o -o zbx_econv
