This example is a simple text based application that uses the messages from
the TomCat server.  This example shows multiple facilities being compiled to
a single message package (the messages are also optimized for locale based
access, i.e., the messages for each locale are stored together).  A subset
of the message set is displayed for the current locale when run.

Since this example is based on an existing Java .properties file set, the
messages are externalized and are first compiled to the Ada package
Apache.Tomcat.Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "zbx".

This example has no third party dependencies.

WARNING: Windows command windows do not support Unicode chararters.  This
example will display "garbage" if run on Windows for non-English locales.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -O -F -i -v Apache.Tomcat.Messages -d mesg authenticator connector core manager realm security servlets startup users util valves zbtomcat
This is ZBMCompile, Version 1.3.0 BETA (r3038:3039M) on 7/30/16 at 12:31 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 79 messages for the facility "authenticator" (4 locales)
Loaded 128 messages for the facility "connector" (4 locales)
"/u/mrohan/zb/ref/examples/text/tomcat/mesg/core_es.properties", line 52, invalid (non ISO-644) character, use \u escapes instead
Loaded 503 messages for the facility "core" (4 locales)
"/u/mrohan/zb/ref/examples/text/tomcat/mesg/manager_fr.properties", line 89, duplicate key "managerServlet.startFailed" previous defined on line 86
"/u/mrohan/zb/ref/examples/text/tomcat/mesg/manager_fr.properties", line 90, duplicate key "managerServlet.stopped" previous defined on line 87
"/u/mrohan/zb/ref/examples/text/tomcat/mesg/manager_fr.properties", line 91, duplicate key "managerServlet.undeployed" previous defined on line 88
Loaded 406 messages for the facility "manager" (5 locales)
Loaded 199 messages for the facility "realm" (4 locales)
Loaded 14 messages for the facility "security" (4 locales)
Loaded 37 messages for the facility "servlets" (4 locales)
Loaded 291 messages for the facility "startup" (4 locales)
Loaded 25 messages for the facility "users" (4 locales)
Loaded 58 messages for the facility "util" (4 locales)
Loaded 209 messages for the facility "valves" (4 locales)
Loaded 2 messages for the facility "zbtomcat" (1 locales)
Performing consistency checks for the facility "authenticator"
Performing consistency checks for the facility "connector"
Error, no reference definition for the key "mapperListener.unregisterHost" in the facility "connector"
Warning, the key "mapperListener.unregisterHost" in the facility "connector" is defined for the locale "es"
Error, no reference definition for the key "mapperListener.registerHost" in the facility "connector"
Warning, the key "mapperListener.registerHost" in the facility "connector" is defined for the locale "es"
Error, no reference definition for the key "coyoteRequest.noLoginConfig" in the facility "connector"
Warning, the key "coyoteRequest.noLoginConfig" in the facility "connector" is defined for the locale "es"
Error, no reference definition for the key "mapperListener.unknownDefaultHost" in the facility "connector"
Warning, the key "mapperListener.unknownDefaultHost" in the facility "connector" is defined for the locale "es"
Performing consistency checks for the facility "core"
Error, no reference definition for the key "defaultInstanceManager.restrictedListenersResources" in the facility "core"
Warning, the key "defaultInstanceManager.restrictedListenersResources" in the facility "core" is defined for the locale "es"
Error, too many arguments for key "applicationServletRegistration.setServletSecurity.ise" in facility "core" for locale "es"
Performing consistency checks for the facility "manager"
Performing consistency checks for the facility "realm"
Performing consistency checks for the facility "security"
Performing consistency checks for the facility "servlets"
Performing consistency checks for the facility "startup"
Error, too many arguments for key "contextConfig.servletContainerInitializerFail" in facility "startup" for locale "es"
Error, too many arguments for key "contextConfig.contextClose" in facility "startup" for locale "es"
Performing consistency checks for the facility "users"
Performing consistency checks for the facility "util"
Performing consistency checks for the facility "valves"
Performing consistency checks for the facility "zbtomcat"
12 errors detected, forcing generation of Ada sources
Optimizing the catalog for locale order access
Accumulated messages for the locale "" (775 messages)
Accumulated messages for the locale "es" (527 messages)
Accumulated messages for the locale "fr" (310 messages)
Accumulated messages for the locale "ja" (269 messages)
Accumulated messages for the locale "de" (67 messages)
Loaded 12 facilities, 779 keys, 5 locales and 1948 messages
Loaded total 105715 characters, stored 101129 unique characters, 4% saving
Wrote the spec "Apache.Tomcat.Messages" to the file "./apache-tomcat-messages.ads"
Wrote the body "Apache.Tomcat.Messages" to the file "./apache-tomcat-messages.adb"
ZBMCompile completed on 7/30/16 at 12:31 PM, elapsed time 0:00:03.150
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_tomcat.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 zbx_tomcat.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 apache.ads
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 apache-tomcat.ads
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 apache-tomcat-messages.adb
gprbind zbx_tomcat.bexch
gnatbind zbx_tomcat.ali
gcc -c b__zbx_tomcat.adb
gcc zbx_tomcat.o -o zbx_tomcat

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_tomcat.gpr
gprbuild: "zbx_tomcat" up to date
../../../bin/zbx_tomcat
This is TOMCAT, Version 1.3.0 - BETA
Configuration error:  Must be attached to a Context
Authorizing connector provided user [mrohan] via Tomcat Realm
Protocol handler resume failed
Exception while attempting to add an entry to the access log
Path ../example.dat does not start with a "/" character
Exception thrown by attributes event listener
Deploy directory or WAR file located on server
FAIL - War file "../example.dat" cannot be uploaded if context is defined in server.xml
JAAS LoginContext created for username "mrohan"
Principal "mrohan" is a valid user class. We will use this as the user Principal.
An exception occurs when running the PrivilegedExceptionAction block.
Failed to parse value [mask] as a valid umask.
JAXP initialization failed
The requested resource (resource) is not available
The required Server component failed to start so Tomcat is unable to start.
alt-dd file ../example.dat not found
User database has been configured to be read only. Changes cannot be saved
Null or zero length role name specified. The role will be ignored.
Web Application Manifest
The destroy() method was called on component [component] after destroy() had already been called. The second call will be ignored.
Exception performing insert access entry
Failed to open access log file [../example.dat]

$ make run_es
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_tomcat.gpr
gprbuild: "zbx_tomcat" up to date
../../../bin/zbx_tomcat -les
This is TOMCAT, Version 1.3.0 - BETA
Error de Configuración: Debe de estar unido a un Contexto
Authorizing connector provided user [mrohan] via Tomcat Realm
Ha fallado el rearranque del manejador de protocolo
Excepción al intentar añadir una entrada al historial de acceso
La Trayectoria ../example.dat no comienza con carácter "/"
Excepción lanzada por escuchador de eventos de atributos
Desplegar directorio o archivo WAR localizado en servidor
FALLO - El fichero war "../example.dat" no se puede cargar si se define el contexto en server.xml
JAAS LoginContext creado para nombre de usuario "mrohan"
El Principal "mrohan" es una clase válida de usuario. La vamos a usar como usuario Principal.
Ha tenido lugar una excepción al ejecutar el bloque PrivilegedExceptionAction.
No pude anallizar el valor [mask] como in válido umask.
Falló la inicialización de JAXP
El recurso requerido resource no se encuentra disponible
The required Server component failed to start so Tomcat is unable to start.
fichero alt-dd ../example.dat no hallado
User database has been configured to be read only. Changes cannot be saved
Se ha especificado un nombre rol nulo o de tamaño cero. Se ignora el rol.
Manifiesto de Aplicación Web
The destroy() method was called on component [component] after destroy() had already been called. The second call will be ignored.
Excepción realizando entrada de acceso a inserción
Failed to open access log file [../example.dat]
