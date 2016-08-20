This example is a simple text based application that prints integers with
varying formatings and field widths.  The messages are externalized to the
file "xformatting.properties" which is first compiled to the Ada package
XFormatting_Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "zbx".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -v XFormatting_Messages xformatting
This is ZBMCompile, Version 1.3.0 BETA (r3038:3039M) on 7/30/16 at 12:19 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 27 messages for the facility "xformatting" (1 locales)
Performing consistency checks for the facility "xformatting"
Loaded 1 facilities, 27 keys, 1 locales and 27 messages
Loaded total 1425 characters, stored 1425 unique characters, 0% saving
Wrote the spec "XFormatting_Messages" to the file "./xformatting_messages.ads"
Wrote the body "XFormatting_Messages" to the file "./xformatting_messages.adb"
ZBMCompile completed on 7/30/16 at 12:19 PM, elapsed time 0:00:00.032
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_formatting.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 zbx_formatting.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 definitions.ads
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 xformatting_messages.adb
gprbind zbx_formatting.bexch
gnatbind zbx_formatting.ali
gcc -c b__zbx_formatting.adb
gcc zbx_formatting.o -o zbx_formatting

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_formatting.gpr
gprbuild: "zbx_formatting" up to date
../../../bin/zbx_formatting
This is FORMATTING, Version 1.3.0 - BETA
Various formats for "1964", "Simple positive value"
   Decimal:     1964
   Binary:      11110101100
   Octal:       3654
   Hexadecimal: 7ac
   Decimal (with base decorator):     1964
   Binary (with base decorator):      2#11110101100#
   Octal (with base decorator):       8#3654#
   Hexadecimal (with base decorator): 16#7ac#
   Decimal (left in 40 spaces):     |1964                                    |
   Binary (left in 40 spaces):      |11110101100                             |
   Octal (left in 40 spaces):       |3654                                    |
   Hexadecimal (left in 40 spaces): |7ac                                     |
   Decimal (right in 40 spaces):     |                                    1964|
   Binary (right in 40 spaces):      |                             11110101100|
   Octal (right in 40 spaces):       |                                    3654|
   Hexadecimal (right in 40 spaces): |                                     7ac|
   Decimal (center in 40 spaces):     |                  1964                  |
   Binary (center in 40 spaces):      |               11110101100              |
   Octal (center in 40 spaces):       |                  3654                  |
   Hexadecimal (center in 40 spaces): |                   7ac                  |
Various formats for "-1964", "Simple negative value"
   Decimal:     -1964
   Binary:      -11110101100
   Octal:       -3654
   Hexadecimal: -7ac
   Decimal (with base decorator):     -1964
   Binary (with base decorator):      -2#11110101100#
   Octal (with base decorator):       -8#3654#
   Hexadecimal (with base decorator): -16#7ac#
   Decimal (left in 40 spaces):     |-1964                                   |
   Binary (left in 40 spaces):      |-11110101100                            |
   Octal (left in 40 spaces):       |-3654                                   |
   Hexadecimal (left in 40 spaces): |-7ac                                    |
   Decimal (right in 40 spaces):     |                                   -1964|
   Binary (right in 40 spaces):      |                            -11110101100|
   Octal (right in 40 spaces):       |                                   -3654|
   Hexadecimal (right in 40 spaces): |                                    -7ac|
   Decimal (center in 40 spaces):     |                  -1964                 |
   Binary (center in 40 spaces):      |              -11110101100              |
   Octal (center in 40 spaces):       |                  -3654                 |
   Hexadecimal (center in 40 spaces): |                  -7ac                  |
Various formats for "-2147483648", "First integer value"
   Decimal:     -2147483648
   Binary:      -10000000000000000000000000000000
   Octal:       -20000000000
   Hexadecimal: -80000000
   Decimal (with base decorator):     -2147483648
   Binary (with base decorator):      -2#10000000000000000000000000000000#
   Octal (with base decorator):       -8#20000000000#
   Hexadecimal (with base decorator): -16#80000000#
   Decimal (left in 40 spaces):     |-2147483648                             |
   Binary (left in 40 spaces):      |-10000000000000000000000000000000       |
   Octal (left in 40 spaces):       |-20000000000                            |
   Hexadecimal (left in 40 spaces): |-80000000                               |
   Decimal (right in 40 spaces):     |                             -2147483648|
   Binary (right in 40 spaces):      |       -10000000000000000000000000000000|
   Octal (right in 40 spaces):       |                            -20000000000|
   Hexadecimal (right in 40 spaces): |                               -80000000|
   Decimal (center in 40 spaces):     |               -2147483648              |
   Binary (center in 40 spaces):      |    -10000000000000000000000000000000   |
   Octal (center in 40 spaces):       |              -20000000000              |
   Hexadecimal (center in 40 spaces): |                -80000000               |
Various formats for "2147483647", "Last integer value"
   Decimal:     2147483647
   Binary:      1111111111111111111111111111111
   Octal:       17777777777
   Hexadecimal: 7fffffff
   Decimal (with base decorator):     2147483647
   Binary (with base decorator):      2#1111111111111111111111111111111#
   Octal (with base decorator):       8#17777777777#
   Hexadecimal (with base decorator): 16#7fffffff#
   Decimal (left in 40 spaces):     |2147483647                              |
   Binary (left in 40 spaces):      |1111111111111111111111111111111         |
   Octal (left in 40 spaces):       |17777777777                             |
   Hexadecimal (left in 40 spaces): |7fffffff                                |
   Decimal (right in 40 spaces):     |                              2147483647|
   Binary (right in 40 spaces):      |         1111111111111111111111111111111|
   Octal (right in 40 spaces):       |                             17777777777|
   Hexadecimal (right in 40 spaces): |                                7fffffff|
   Decimal (center in 40 spaces):     |               2147483647               |
   Binary (center in 40 spaces):      |     1111111111111111111111111111111    |
   Octal (center in 40 spaces):       |               17777777777              |
   Hexadecimal (center in 40 spaces): |                7fffffff                |
Various formats for "-9223372036854775808", "First long integer first value"
   Decimal:     -9223372036854775808
   Binary:      -1000000000000000000000000000000000000000000000000000000000000000
   Octal:       -1000000000000000000000
   Hexadecimal: -8000000000000000
   Decimal (with base decorator):     -9223372036854775808
   Binary (with base decorator):      -2#1000000000000000000000000000000000000000000000000000000000000000#
   Octal (with base decorator):       -8#1000000000000000000000#
   Hexadecimal (with base decorator): -16#8000000000000000#
   Decimal (left in 40 spaces):     |-9223372036854775808                    |
   Binary (left in 40 spaces):      |-1000000000000000000000000000000000000000000000000000000000000000|
   Octal (left in 40 spaces):       |-1000000000000000000000                 |
   Hexadecimal (left in 40 spaces): |-8000000000000000                       |
   Decimal (right in 40 spaces):     |                    -9223372036854775808|
   Binary (right in 40 spaces):      |-1000000000000000000000000000000000000000000000000000000000000000|
   Octal (right in 40 spaces):       |                 -1000000000000000000000|
   Hexadecimal (right in 40 spaces): |                       -8000000000000000|
   Decimal (center in 40 spaces):     |          -9223372036854775808          |
   Binary (center in 40 spaces):      |-1000000000000000000000000000000000000000000000000000000000000000|
   Octal (center in 40 spaces):       |         -1000000000000000000000        |
   Hexadecimal (center in 40 spaces): |            -8000000000000000           |
Various formats for "9223372036854775807", "Last long integer last value"
   Decimal:     9223372036854775807
   Binary:      111111111111111111111111111111111111111111111111111111111111111
   Octal:       777777777777777777777
   Hexadecimal: 7fffffffffffffff
   Decimal (with base decorator):     9223372036854775807
   Binary (with base decorator):      2#111111111111111111111111111111111111111111111111111111111111111#
   Octal (with base decorator):       8#777777777777777777777#
   Hexadecimal (with base decorator): 16#7fffffffffffffff#
   Decimal (left in 40 spaces):     |9223372036854775807                     |
   Binary (left in 40 spaces):      |111111111111111111111111111111111111111111111111111111111111111|
   Octal (left in 40 spaces):       |777777777777777777777                   |
   Hexadecimal (left in 40 spaces): |7fffffffffffffff                        |
   Decimal (right in 40 spaces):     |                     9223372036854775807|
   Binary (right in 40 spaces):      |111111111111111111111111111111111111111111111111111111111111111|
   Octal (right in 40 spaces):       |                   777777777777777777777|
   Hexadecimal (right in 40 spaces): |                        7fffffffffffffff|
   Decimal (center in 40 spaces):     |           9223372036854775807          |
   Binary (center in 40 spaces):      |111111111111111111111111111111111111111111111111111111111111111|
   Octal (center in 40 spaces):       |          777777777777777777777         |
   Hexadecimal (center in 40 spaces): |            7fffffffffffffff            |
Various formats for "-9223372036854775808", "First long long integer value"
   Decimal:     -9223372036854775808
   Binary:      -1000000000000000000000000000000000000000000000000000000000000000
   Octal:       -1000000000000000000000
   Hexadecimal: -8000000000000000
   Decimal (with base decorator):     -9223372036854775808
   Binary (with base decorator):      -2#1000000000000000000000000000000000000000000000000000000000000000#
   Octal (with base decorator):       -8#1000000000000000000000#
   Hexadecimal (with base decorator): -16#8000000000000000#
   Decimal (left in 40 spaces):     |-9223372036854775808                    |
   Binary (left in 40 spaces):      |-1000000000000000000000000000000000000000000000000000000000000000|
   Octal (left in 40 spaces):       |-1000000000000000000000                 |
   Hexadecimal (left in 40 spaces): |-8000000000000000                       |
   Decimal (right in 40 spaces):     |                    -9223372036854775808|
   Binary (right in 40 spaces):      |-1000000000000000000000000000000000000000000000000000000000000000|
   Octal (right in 40 spaces):       |                 -1000000000000000000000|
   Hexadecimal (right in 40 spaces): |                       -8000000000000000|
   Decimal (center in 40 spaces):     |          -9223372036854775808          |
   Binary (center in 40 spaces):      |-1000000000000000000000000000000000000000000000000000000000000000|
   Octal (center in 40 spaces):       |         -1000000000000000000000        |
   Hexadecimal (center in 40 spaces): |            -8000000000000000           |
Various formats for "9223372036854775807", "Last long long integer value"
   Decimal:     9223372036854775807
   Binary:      111111111111111111111111111111111111111111111111111111111111111
   Octal:       777777777777777777777
   Hexadecimal: 7fffffffffffffff
   Decimal (with base decorator):     9223372036854775807
   Binary (with base decorator):      2#111111111111111111111111111111111111111111111111111111111111111#
   Octal (with base decorator):       8#777777777777777777777#
   Hexadecimal (with base decorator): 16#7fffffffffffffff#
   Decimal (left in 40 spaces):     |9223372036854775807                     |
   Binary (left in 40 spaces):      |111111111111111111111111111111111111111111111111111111111111111|
   Octal (left in 40 spaces):       |777777777777777777777                   |
   Hexadecimal (left in 40 spaces): |7fffffffffffffff                        |
   Decimal (right in 40 spaces):     |                     9223372036854775807|
   Binary (right in 40 spaces):      |111111111111111111111111111111111111111111111111111111111111111|
   Octal (right in 40 spaces):       |                   777777777777777777777|
   Hexadecimal (right in 40 spaces): |                        7fffffffffffffff|
   Decimal (center in 40 spaces):     |           9223372036854775807          |
   Binary (center in 40 spaces):      |111111111111111111111111111111111111111111111111111111111111111|
   Octal (center in 40 spaces):       |          777777777777777777777         |
   Hexadecimal (center in 40 spaces): |            7fffffffffffffff            |
Various formats for "9223372036854773843", "Random long long integer value"
   Decimal:     9223372036854773843
   Binary:      111111111111111111111111111111111111111111111111111100001010011
   Octal:       777777777777777774123
   Hexadecimal: 7ffffffffffff853
   Decimal (with base decorator):     9223372036854773843
   Binary (with base decorator):      2#111111111111111111111111111111111111111111111111111100001010011#
   Octal (with base decorator):       8#777777777777777774123#
   Hexadecimal (with base decorator): 16#7ffffffffffff853#
   Decimal (left in 40 spaces):     |9223372036854773843                     |
   Binary (left in 40 spaces):      |111111111111111111111111111111111111111111111111111100001010011|
   Octal (left in 40 spaces):       |777777777777777774123                   |
   Hexadecimal (left in 40 spaces): |7ffffffffffff853                        |
   Decimal (right in 40 spaces):     |                     9223372036854773843|
   Binary (right in 40 spaces):      |111111111111111111111111111111111111111111111111111100001010011|
   Octal (right in 40 spaces):       |                   777777777777777774123|
   Hexadecimal (right in 40 spaces): |                        7ffffffffffff853|
   Decimal (center in 40 spaces):     |           9223372036854773843          |
   Binary (center in 40 spaces):      |111111111111111111111111111111111111111111111111111100001010011|
   Octal (center in 40 spaces):       |          777777777777777774123         |
   Hexadecimal (center in 40 spaces): |            7ffffffffffff853            |
   Floating point                     |1.23457E+10|
   Floating point (Precision=0)     |1E+10|
   Floating point (Precision=1)     |1.2E+10|
   Floating point (Precision=2)     |1.23E+10|
   Floating point (Precision=3)     |1.235E+10|
   Floating point (Precision=4)     |1.2346E+10|
   Floating point (Precision=5)     |1.23457E+10|
   Floating point (Precision=6)     |1.234570E+10|
   Floating point (Precision=7)     |1.2345700E+10|
   Floating point (Precision=8)     |1.23457000E+10|
Some missing arguments, 0: ⁅0⁆, 1: ⁅1⁆
