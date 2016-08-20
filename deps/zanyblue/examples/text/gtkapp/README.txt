This example is a simple Gtk based application that uses the messages from
Oracle's JDBC driver (Java).  A window giving a menu of avialable locales
is displayed.  Selecting a particular locale will create a window the text
of a subset of the messages from the JDBC drivers message set.

Since this example is based on an existing Java .properties file set, the
messages are externalized and are first compiled to the Ada package
OJDBC.Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "zbx".

This example has a third party dependency on the GtkAda library.  The
gprbuild project path might need to be adjusted to locate the gtkada.gpr
file.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -v AppMsg -d mesg appmsg
This is ZBMCompile, Version 1.3.0 BETA (r3038:3039M) on 7/30/16 at 12:21 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 8 messages for the facility "appmsg" (2 locales)
Performing consistency checks for the facility "appmsg"
Loaded 1 facilities, 5 keys, 2 locales and 8 messages
Loaded total 150 characters, stored 115 unique characters, 23% saving
Wrote the spec "AppMsg" to the file "./appmsg.ads"
Wrote the body "AppMsg" to the file "./appmsg.adb"
ZBMCompile completed on 7/30/16 at 12:21 PM, elapsed time 0:00:00.010
../../../bin/zbmcompile -i -v Jenkins.Messages -d ../jenkins/mesg Jenkins
This is ZBMCompile, Version 1.3.0 BETA (r3038:3039M) on 7/30/16 at 12:21 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 405 messages for the facility "Jenkins" (16 locales)
Performing consistency checks for the facility "Jenkins"
Loaded 1 facilities, 37 keys, 16 locales and 405 messages
Loaded total 14857 characters, stored 14261 unique characters, 4% saving
Wrote the spec "Jenkins.Messages" to the file "./jenkins-messages.ads"
Wrote the body "Jenkins.Messages" to the file "./jenkins-messages.adb"
ZBMCompile completed on 7/30/16 at 12:21 PM, elapsed time 0:00:00.209
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_gjenkins.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 zbx_gjenkins.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 appmsg.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 button_cb.ads
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 display_strings.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 jenkins.ads
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 jenkins-messages.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 locale_buttons.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 text_display.adb
gprbind zbx_gjenkins.bexch
gnatbind zbx_gjenkins.ali
gcc -c b__zbx_gjenkins.adb
gcc zbx_gjenkins.o -L/usr/gnat/2016/lib -L/usr/gnat/2016/bin -Wl,--export-dynamic -pthread -L/usr/gnat/2016/lib -lgtk-3 -lgdk-3 -latk-1.0 -lgio-2.0 -lpangocairo-1.0 -lgdk_pixbuf-2.0 -lcairo-gobject -lpango-1.0 -lcairo -lgobject-2.0 -lgmodule-2.0 -lrt -lglib-2.0 -lfontconfig -lfreetype -L/usr/gnat/2016/lib -L/usr/gnat/2016/bin -Wl,--export-dynamic -pthread -L/usr/gnat/2016/lib -lgtk-3 -lgdk-3 -latk-1.0 -lgio-2.0 -lpangocairo-1.0 -lgdk_pixbuf-2.0 -lcairo-gobject -lpango-1.0 -lcairo -lgobject-2.0 -lgmodule-2.0 -lrt -lglib-2.0 -lfontconfig -lfreetype -o zbx_gjenkins

-----------------------------------------------------------------------------
$ make run
> Displays the menu with English language and territory names.

$ make run_fr
> Displays the menu with French language and territory names.

$ make run_el
> Displays the menu with Greek language and territory names.

etc.
