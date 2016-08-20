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
../../../bin/zbmcompile -G prints -i -S messages.stamp -v Messages Moons
This is ZBMCompile, Version 1.3.0 BETA (r3038:3039M) on 7/30/16 at 12:27 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 25 messages for the facility "Moons" (4 locales)
Performing consistency checks for the facility "Moons"
Performing consistency checks for the accessor package generation
Loaded 1 facilities, 7 keys, 4 locales and 25 messages
Loaded total 829 characters, stored 829 unique characters, 0% saving
Wrote the spec "Messages" to the file "./messages.ads"
Wrote the body "Messages" to the file "./messages.adb"
Generated accessor package spec "Messages.Moons_Prints" to "./messages-moons_prints.ads"
Generated accessor package body "Messages.Moons_Prints" to "./messages-moons_prints.adb"
Created the stamp file "messages.stamp" containing the time stamp 7/30/16, 12:27 PM
ZBMCompile completed on 7/30/16 at 12:27 PM, elapsed time 0:00:00.035
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_amoons.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 zbx_amoons.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 definitions.ads
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 messages.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 messages-moons_prints.adb
gprbind zbx_amoons.bexch
gnatbind zbx_amoons.ali
gcc -c b__zbx_amoons.adb
gcc zbx_amoons.o -o zbx_amoons

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

