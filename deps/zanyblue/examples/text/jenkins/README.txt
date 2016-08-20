This example is a simple text based application that uses the messages from
Jenkins continuous integration system's (http://www.jenkins-ci.org).  The
set of message files used here were taken from the location

    jenkins/maven-plugin/src/main/resources/hudson/maven

A random selection with a reasonable language scope.  The example application
simply displays the messages.

Since this example is based on an existing Java .properties file set, the
messages are already externalized and are first compiled to the Ada package
Jenkins.Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "zbx".

This example has no third party dependencies.

WARNING: Windows command windows do not support Unicode chararters.  This
example will display "garbage" if run on Windows for non-English locales.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -O -v Jenkins.Messages -d mesg App Jenkins
This is ZBMCompile, Version 1.3.0 BETA (r3038:3039M) on 7/30/16 at 12:24 PM
Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
Loaded 3 messages for the facility "App" (1 locales)
Loaded 405 messages for the facility "Jenkins" (16 locales)
Performing consistency checks for the facility "App"
Performing consistency checks for the facility "Jenkins"
Optimizing the catalog for locale order access
Accumulated messages for the locale "" (40 messages)
Accumulated messages for the locale "bg" (36 messages)
Accumulated messages for the locale "da" (21 messages)
Accumulated messages for the locale "de" (34 messages)
Accumulated messages for the locale "es" (27 messages)
Accumulated messages for the locale "fr" (34 messages)
Accumulated messages for the locale "it" (25 messages)
Accumulated messages for the locale "ja" (34 messages)
Accumulated messages for the locale "nl" (11 messages)
Accumulated messages for the locale "pl" (11 messages)
Accumulated messages for the locale "pt" (34 messages)
Accumulated messages for the locale "pt_BR" (32 messages)
Accumulated messages for the locale "ru" (9 messages)
Accumulated messages for the locale "tr" (9 messages)
Accumulated messages for the locale "zh_CN" (21 messages)
Accumulated messages for the locale "zh_TW" (30 messages)
Loaded 2 facilities, 40 keys, 16 locales and 408 messages
Loaded total 15578 characters, stored 14909 unique characters, 4% saving
Wrote the spec "Jenkins.Messages" to the file "./jenkins-messages.ads"
Wrote the body "Jenkins.Messages" to the file "./jenkins-messages.adb"
ZBMCompile completed on 7/30/16 at 12:24 PM, elapsed time 0:00:00.360
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_jenkins.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 zbx_jenkins.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 jenkins.ads
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat2012 -gnatW8 jenkins-messages.adb
gprbind zbx_jenkins.bexch
gnatbind zbx_jenkins.ali
gcc -c b__zbx_jenkins.adb
gcc zbx_jenkins.o -o zbx_jenkins

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_jenkins.gpr
gprbuild: "zbx_jenkins" up to date
../../../bin/zbx_jenkins
This is JENKINS, Version 1.3.0 - BETA
Whitespace can no longer be used as the separator. Please Use ‘,’ as the separator instead.
‘This’ doesn’t match anything, but ‘That’ does. Perhaps that’s what you mean?
‘This’ doesn’t match anything, although ‘That’ exists
‘This’ doesn’t match anything: ‘That’ exists but not ‘Other’
‘This’ doesn’t match anything
‘This’ doesn’t match anything: even ‘That’ doesn’t exist
Wildcard is not allowed here
‘This’ is not a file
‘This’ is not a directory
No such file: ‘This’
No such directory: ‘This’
This plugin doesn’t support dynamic loading. Jenkins needs to be restarted for the update to take effect
This plugin is already installed. Jenkins needs to be restarted for the update to take effect
‘~’ is only supported in a Unix shell and nowhere else.
Plugin Manager
Port is not a number
Port doesn’t range from 10 to 100
About Jenkins
See the version and license information.

$ make run_fr
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -XV_MAJOR=1 -XV_MINOR=3 -XV_PATCH=0 -XV_STATUS=Beta -aP../../../lib/gnat -aP../../../src -P zbx_jenkins.gpr
gprbuild: "zbx_jenkins" up to date
../../../bin/zbx_jenkins -lfr
This is JENKINS, Version 1.3.0 - BETA
Les espaces ne peuvent plus être utilisés comme séparateurs. Merci d'utiliser maintenant ',' comme séparateur à la place.
'This' ne correspond à rien, mais 'That' oui. Peut-être est-ce cela que vous voulez dire?
'This' ne correspond à rien, même si 'That' existe
'This' ne correspond à rien : 'That' existe mais pas 'Other'
'This' ne correspond à rien
'This' ne correspond à rien : même 'That' n'existe pas
L'utilisation des Wildcard n'est pas autorisée ici
'This' n'est pas un fichier
'This' n'est pas un répertoire
Aucun fichier correspondant : 'This'
Aucun répertoire correspondant : 'This'
Le plugin This ne supporte pas le chargement dynamique. Jenkins doit être redémarré pour que la mise à jour soit effective.
Le plugin This est déjà installé. Jenkins doit être redémarré pour que la mise à jour soit effective.
'~' n'est supporté que sur les shells Unix.
Gestion des plugins
Le port n'est pas un nombre
Le port n'est pas dans l'intervalle de 10 à 100
A propos de Jenkins
Afficher les informations de version et de licence
