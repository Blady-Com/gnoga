This example is a simple text based application that prints integers with
varying formatings and field widths.  The messages are externalized to the
file "xformatting.properties" which is first compiled to the Ada package
XFormatting_Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -v XFormatting_Messages xformatting
This is ZBMCompile, Version 0.1.0 ALPHA (r1462) at 8:34 AM on 8/24/10
Copyright (c) 2009-2010, Michael Rohan.  All rights reserved
Loaded 36 messages for the facility "xformatting" (1 locales)
Loaded 1 facilities, 36 keys, 1 locales and 36 messages
Created the spec "XFormatting_Messages" in the file "xformatting_messages.ads"
Created the body "XFormatting_Messages" in the file "xformatting_messages.adb"
ZBMCompile completed at 8:34 AM on 8/24/10, elapsed time 0:00:00.127
gprbuild -p -aP../../../lib -P xformatting.gpr
creating auto.cgpr
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 xformatting.adb
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 xformatting_messages.adb
gprbind xformatting.bexch
gnatbind xformatting.ali
gcc-4.4 -c b__xformatting.adb
gcc-4.4 xformatting.o -o xformatting

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -P xformatting.gpr
creating auto.cgpr
gprbuild: "xformatting" up to date
../../../bin/xformatting
This is TEXT_FORMATTING V1.0.0, ALPHA
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
Various formats for "-2147483648", "First long integer first value"
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
Various formats for "2147483647", "Last long integer last value"
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
