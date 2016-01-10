This example is a simple text based application that uses the messages from
the TomCat server.  This example shows multiple facilities being compiled to
a single message package (the messages are also optimized for locale based
access, i.e., the messages for each locale are stored together).  A subset
of the message set is displayed for the current locale when run.

Since this example is based on an existing Java .properties file set, the
messages are externalized and are first compiled to the Ada package
Apache.Tomcat.Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

WARNING: Windows command windows do not support Unicode chararters.  This
example will display "garbage" if run on Windows for non-English locales.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -O -i -v Apache.Tomcat.Messages -d mesg ajp authenticator connector core ha ha-session ha-tcp host http11 http jasper jsse loader manager mbeans membership naming naming-res realm security servlet servlets session startup transport tribes users util-buf util-http util-net util util-threads valves webapps
This is ZBMCompile, Version 0.1.0 ALPHA (r1462) at 9:06 AM on 8/24/10
Copyright (c) 2009-2010, Michael Rohan.  All rights reserved
Loaded 48 messages for the facility "ajp" (2 locales)
Loaded 55 messages for the facility "authenticator" (4 locales)
Loaded 140 messages for the facility "connector" (4 locales)
Loaded 745 messages for the facility "core" (4 locales)
Loaded 3 messages for the facility "ha" (3 locales)
Loaded 161 messages for the facility "ha-session" (2 locales)
Loaded 128 messages for the facility "ha-tcp" (2 locales)
Loaded 114 messages for the facility "host" (2 locales)
Loaded 95 messages for the facility "http11" (4 locales)
Loaded 32 messages for the facility "http" (4 locales)
Loaded 1296 messages for the facility "jasper" (4 locales)
Loaded 6 messages for the facility "jsse" (4 locales)
Loaded 144 messages for the facility "loader" (4 locales)
Loaded 420 messages for the facility "manager" (5 locales)
Loaded 10 messages for the facility "mbeans" (2 locales)
Loaded 2 messages for the facility "membership" (2 locales)
Loaded 46 messages for the facility "naming" (4 locales)
Loaded 83 messages for the facility "naming-res" (4 locales)
Loaded 218 messages for the facility "realm" (4 locales)
Loaded 4 messages for the facility "security" (4 locales)
Loaded 12 messages for the facility "servlet" (4 locales)
Loaded 72 messages for the facility "servlets" (4 locales)
Loaded 267 messages for the facility "session" (4 locales)
Loaded 289 messages for the facility "startup" (4 locales)
Loaded 122 messages for the facility "transport" (2 locales)
Loaded 3 messages for the facility "tribes" (3 locales)
Loaded 20 messages for the facility "users" (4 locales)
Loaded 12 messages for the facility "util-buf" (4 locales)
Loaded 179 messages for the facility "util-http" (4 locales)
Loaded 56 messages for the facility "util-net" (4 locales)
Loaded 35 messages for the facility "util" (4 locales)
Loaded 12 messages for the facility "util-threads" (4 locales)
Loaded 216 messages for the facility "valves" (4 locales)
Loaded 140 messages for the facility "webapps" (5 locales)
Loaded 34 facilities, 1522 keys, 7 locales and 5185 messages
Optimizing the catalog for locale order access
Collected the messages for the locale "" (1590 messages, 78088 characters)
Collected the messages for the locale "es" (1519 messages, 83125 characters)
Collected the messages for the locale "fr" (939 messages, 54039 characters)
Collected the messages for the locale "ja" (1003 messages, 30490 characters)
Collected the messages for the locale "de" (78 messages, 2666 characters)
Collected the messages for the locale "en" (28 messages, 584 characters)
Collected the messages for the locale "pt" (28 messages, 673 characters)
Created the spec "Apache.Tomcat.Messages" in the file "apache-tomcat-messages.ads"
Created the body "Apache.Tomcat.Messages" in the file "apache-tomcat-messages.adb"
ZBMCompile completed at 9:07 AM on 8/24/10, elapsed time 0:00:12.147
gprbuild -p -aP../../../lib -P xtomcat.gpr
creating auto.cgpr
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 xtomcat.adb
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 apache.ads
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 apache-tomcat.ads
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 apache-tomcat-messages.adb
gprbind xtomcat.bexch
gnatbind xtomcat.ali
gcc-4.4 -c b__xtomcat.adb
gcc-4.4 xtomcat.o -o xtomcat

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -P xtomcat.gpr
creating auto.cgpr
gprbuild: "xtomcat" up to date
../../../bin/xtomcat
Error pausing endpoint
Configuration error:  Must be attached to a Context
Protocol handler resume failed
createSession: Too many active sessions
Cant load data replication sender mapping list
Cannot invoke host manager servlet through invoker
Error initializing socket factory
Negative Length given in write method
IMPORTANT: Do not modify the generated servlets
Cannot auto-reload unless our Container is a Context
Deploy directory or WAR file located on server
No naming context bound to this thread
Resources has not yet been started
An exception occurs when running the PrivilegedExceptionAction block.
Cannot call invoker servlet with a named dispatcher
The database connection is null or was found to be closed. Trying to re-open it.
Error closing application web.xml
Unable to perform replication request.
User database has been configured to be read only. Changes cannot be saved
Bad hexadecimal digit
Non-Authoritative Information
Caught exception trying to close socket
Web Application Manifest
Semaphore valve has already been started
Request Parameters Example
Path ../example.dat does not start with a "/" character
MBean xmpl already registered!
Alias name xyz does not identify a key entry
The JMX connector server could not be created or failed to start for the www.example.com server
MBean xmpl already registered!
JAAS LoginContext created for username "mrohan"
Not an ISO 8859-1 character: ٤
MBean xmpl already registered!
Caught exception (NullPointerException) executing WorkerThread, terminating thread
