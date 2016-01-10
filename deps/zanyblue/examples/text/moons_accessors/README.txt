This example is a simple text based application that asks for the name of
a planet and prints the corresponding number of moons.  This is the example
used in the documentation.
The messages are externalized to the files "moons*.properties" which are first
compiled to the Ada package Moons_Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -v Moons_Messages moons
This is ZBMCompile, Version 0.1.0 ALPHA (r1462) at 8:50 AM on 8/24/10
Copyright (c) 2009-2010, Michael Rohan.  All rights reserved
Loaded 16 messages for the facility "moons" (4 locales)
Loaded 1 facilities, 4 keys, 4 locales and 16 messages
Created the spec "Moons_Messages" in the file "moons_messages.ads"
Created the body "Moons_Messages" in the file "moons_messages.adb"
ZBMCompile completed at 8:50 AM on 8/24/10, elapsed time 0:00:00.063
gprbuild -p -aP../../../lib -P xmoons.gpr
creating auto.cgpr
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 xmoons.adb
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 moons_messages.adb
gprbind xmoons.bexch
gnatbind xmoons.ali
gcc-4.4 -c b__xmoons.adb
gcc-4.4 xmoons.o -o xmoons

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -P xmoons.gpr
creating auto.cgpr
gprbuild: "xmoons" up to date
../../../bin/xmoons
Please enter a planet: earth
There is 1 known moon orbiting "EARTH".
Please enter a planet: mars
There are 2 known moons orbiting "MARS".
Please enter a planet: ^D
OK, goodbye.

$ make run_de
gprbuild -p -aP../../../lib -P xmoons.gpr
creating auto.cgpr
gprbuild: "xmoons" up to date
ZB_LANG=de ../../../bin/xmoons
Bitte geben Sie einen Planeten: earth
Es gibt 1 bekannt Mond umkreisen "EARTH".
Bitte geben Sie einen Planeten: mars
Es gibt 2 bekannte Monde umkreisen "MARS".
Bitte geben Sie einen Planeten: ^D
OK, auf Wiedersehen.

