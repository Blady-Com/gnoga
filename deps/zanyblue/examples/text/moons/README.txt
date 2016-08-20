This example is a simple text based application that asks for the name of
a planet and prints the corresponding number of moons.  This is the example
used in the documentation.
The messages are externalized to the files "moons*.properties" which are first
compiled to the Ada package Moons_Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "zbx".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -O -i -v -S messages.stamp Moons_Messages moons
This is ZBMCompile, Version 1.3.0 BETA (r3038:3039M) on 7/30/16 at 12:25 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 20 messages for the facility "moons" (4 locales)
Performing consistency checks for the facility "moons"
Optimizing the catalog for locale order access
Accumulated messages for the locale "" (5 messages)
Accumulated messages for the locale "de" (5 messages)
Accumulated messages for the locale "es" (5 messages)
Accumulated messages for the locale "fr" (5 messages)
Loaded 1 facilities, 5 keys, 4 locales and 20 messages
Loaded total 677 characters, stored 677 unique characters, 0% saving
Wrote the spec "Moons_Messages" to the file "./moons_messages.ads"
Wrote the body "Moons_Messages" to the file "./moons_messages.adb"
Created the stamp file "messages.stamp" containing the time stamp 7/30/16, 12:25 PM
ZBMCompile completed on 7/30/16 at 12:25 PM, elapsed time 0:00:00.019
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_moons.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 zbx_moons.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 definitions.ads
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 moons_messages.adb
gprbind zbx_moons.bexch
gnatbind zbx_moons.ali
gcc -c b__zbx_moons.adb
gcc zbx_moons.o -o zbx_moons

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_moons.gpr
gprbuild: "zbx_moons" up to date
../../../bin/zbx_moons
This is MOONS, Version 1.3.0 - BETA
Please enter a planet: earth
There is 1 known moon orbiting "EARTH".
Please enter a planet: jupiter
There are 63 known moons orbiting "JUPITER".
Please enter a planet: 
OK, goodbye.

$ make run_de
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_moons.gpr
gprbuild: "zbx_moons" up to date
../../../bin/zbx_moons de
Dies ist MOONS, Version 1.3.0 - BETA
Bitte geben Sie einen Planeten: earth
Es gibt 1 bekannt Mond umkreisen "EARTH".
Bitte geben Sie einen Planeten: jupiter
Es gibt 63 bekannte Monde umkreisen "JUPITER".
Bitte geben Sie einen Planeten: 
OK, auf Wiedersehen.

