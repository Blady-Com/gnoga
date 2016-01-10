This example is a simple Gtk based application that uses the messages from
Oracle's JDBC driver (Java).  A window giving a menu of avialable locales
is displayed.  Selecting a particular locale will create a window the text
of a subset of the messages from the JDBC drivers message set.

Since this example is based on an existing Java .properties file set, the
messages are externalized and are first compiled to the Ada package
OJDBC.Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has a third party dependency on the GtkAda library.  The
gprbuild project path might need to be adjusted to locate the gtkada.gpr
file.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -v AppMsg -d mesg appmsg
This is ZBMCompile, Version 0.1.0 ALPHA (r1462) at 8:41 AM on 8/24/10
Copyright (c) 2009-2010, Michael Rohan.  All rights reserved
Loaded 7 messages for the facility "appmsg" (2 locales)
Loaded 1 facilities, 4 keys, 2 locales and 7 messages
Created the spec "AppMsg" in the file "appmsg.ads"
Created the body "AppMsg" in the file "appmsg.adb"
ZBMCompile completed at 8:41 AM on 8/24/10, elapsed time 0:00:00.046
../../../bin/zbmcompile -i -v OJDBC.Messages -d ../ojdbc/mesg ojdbc
This is ZBMCompile, Version 0.1.0 ALPHA (r1462) at 8:41 AM on 8/24/10
Copyright (c) 2009-2010, Michael Rohan.  All rights reserved
Loaded 9693 messages for the facility "ojdbc" (29 locales)
Loaded 1 facilities, 338 keys, 29 locales and 9693 messages
Created the spec "OJDBC.Messages" in the file "ojdbc-messages.ads"
Created the body "OJDBC.Messages" in the file "ojdbc-messages.adb"
ZBMCompile completed at 8:41 AM on 8/24/10, elapsed time 0:00:17.294
gprbuild -p -aP../../../lib -P xgojdbc.gpr
creating auto.cgpr
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 xgojdbc.adb
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 appmsg.adb
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 locale_buttons.adb
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 ojdbc.ads
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 ojdbc-messages.adb
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 text_display.adb
gprbind xgojdbc.bexch
gnatbind xgojdbc.ali
gcc-4.4 -c b__xgojdbc.adb
gcc-4.4 xgojdbc.o -o xgojdbc

-----------------------------------------------------------------------------
$ make run
> Displays the menu with English language and territory names.

$ make run_fr
> Displays the menu with French language and territory names.

$ make run_el
> Displays the menu with Greek language and territory names.

etc.
