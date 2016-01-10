NOTICE: The patches and instructions here are not endorsed by Ada Core.  This
is an independent development.  The code created after applying the patches
should be considered example code and not used for production versions of
GPS.

= OVERVIEW =

The patches here convert GPS 4.4.1, 5.0.1, 5.1.1 and 5.2.1 releases to an
externalized strings version using the ZanyBlue Text library.  The conversion
was driven by the existing internationalization mechanism in GPS where strings
to be localized were identified by the "-" operator via the GPS.Intl package.

Each top level component within GPS was independently externalized to ZanyBlue
.properties files within a "mesg" directory of the component, e.g., the
component "refactoring" messages were externalized to

	refractoring/mesg/Refactoring

Since this was an semi-automated conversion, simple numeric style keys were
used, e.g., instead of defining a message like

	File_Not_File=The file "{0}" was not found

the message was defined as

	00034=The file "{0}" was not found

This organization of messages into component level .properties files make
real sense, a more correct implementation would also introduce a shared
.properties file for strings shared across all components, e.g., the various
menu item names appear in multiple .properties files rather a single shared
file.  Such duplication would obviously lead to issues if the same menu
item is localized to different strings in different components.  This can
be seen in the generated executables when full Unicode displaced pseudo
translation is used (--pseudo=h or --pseudo=e) where the standard menu
items are defined by Gtk, e.g., "File", "Edit", etc and the GPS code
a localized version of these strings to create sub-items.  E.g., the
string "File" (from Gtk) will match the localized string "FILE" when
using uppercased pseudo translations (--pseudo=u) but will not match
"Ｆｉｌｅ" when full width forms psuedo translations are used (--pseudo=h).

Applying the patches (see below) creates a code base that, when built and
executed, is essentially the same as the non ZanyBlue-ized version.  Since
pseudo translation is built into the ZanyBlue Text library, it is possible
to see how the interface reacts to various "level" of pseudo translation
via the new "gps" command line option "--pseudo" which requires a single
character style parameter.  The styles supported are

	--pseudo=u    Convert strings to uppercase
	--pseudo=l    Convert strings to lowercase
	--pseudo=e    Convert strings to enclosed alpha-numeric
	--pseudo=h    Convert strings to halfwidth forms

The "e" and "h" options push the code the furthest from the original English
text and cause the most disruption to the UI (they also require the supporting
font files are installed).  The "u" and "l" options easily allow the
identification of strings that have not been identified as localizable
and the GtkAda library appears to correctly match menu items like "FILE" to
"File", i.e., the search appears to be case insensitive.

Typical examples of the non-localized, fixed, strings are in the
builder/src/builder_facility_module-scripts.adb source file for the
build options.

= BUILDING =

NOTE: The build instructions here are only for Linux system, the code was
not built or tested on Windows systems.  The following instructions assume
the ZanyBlue library and application have already been build and the top
level "zanyblue-VERSION" directory is pointed to by the environment variable
ZBDIR, e.g.,

    export ZBDIR=$HOME/zanyblue-1.1.0b

The ZanyBlue bin directory will need to be on the PATH and the lib directory
needs to be on the ADA_PROJECT_PATH:

   $ export PATH=.:$ZBDIR/bin:$PATH
   $ export ADA_PROJECT_PATH=$ZBDIR/lib/gnat:/usr/gnat/lib/gnat

Note, the variable ZBDIR is only used to simplify the definition of these
two environment variables.  It is not used by the ZanyBlue library or apps.

To build, the patches need to be applied to a base GPS release.  In the
following, replace "4.4.1" with "5.0.1" or "5.1.1" if building other GPS
versions.

The "5.1.1" version was built using GNAT 2011 rather than GNAT 2012.  The
"configure" and "gnatlib/configure" scripts were manually updated to enable
"gtkada" regardless of the results of the test for the installation of the
package.

The "5.2.1" version was built using GNAT 2012 rather than GNAT 2013.  The
"gnatlib/configure" scripts was manually updated to enable "gtkada" regardless
of the results of the test for the installation of the package.  For reference,
the options configured were

configure: --------- Summary for GNAT Components --------------
configure:   Shared libraries:       yes (default: static)
configure:   Gtk+:                   yes (requires pkg-config and gtkada.gpr)
configure:   Python:                 yes /usr (see --with-python)
configure:   PyGtk:                  no  (see --enable-pygtk)
configure:   PyGObject:              no (see --enable-pygobject)
configure:   Syslog:                 yes (see --enable-syslog)
configure:   Readline (GPL license): no (see --with-readline --enable-gpl)
configure:   gmp:                    no (see --with-gmp)
configure:   PostgreSQL:             yes -L/usr/lib (see --with-postgresql)
configure:   Sqlite:                 embedded  (see --with-sqlite)
configure:   Projects:               yes
configure: --------------------------------------------

1) Download the release and extract the release bundle:

  << Download gps-4.4.1-gpl-src.tgz from http://libre.adacore.com >>
  $ tar xzf gps-4.4.1-gpl-src.tgz

2) The GNAT2010-4.4.1.PATCH should be applied to fix some minor issues with
   the source to allow it compile using the GNAT2010 release:

   $ cd gps-4.4.1-src/
   $ gzip -d < $ZBDIR/examples/text/gps/GNAT2010-4.4.1.PATCH.gz | patch -p1
   patching file ada_module/src/ada_module.adb
   ...
   patching file widgets/src/collapsing_pane.adb

   This patch is not needed for the 5.1.1 or 5.2.1 GPS releases.

3) The ZBIZED-4.4.1.PATCH should then be applied to switch to ZanyBlue text
   formatting and add the .properties files:

   $ gzip -d < $ZBDIR/examples/text/gps/ZBIZED-4.4.1.PATCH.gz | patch -p1
   patching file action_editor/action_editor.gpr
   patching file action_editor/mesg/Action_Editor.properties
   ...
   patching file widgets/src/switches_chooser-gtkada.adb
   patching file widgets/widgets.gpr
   patching file zbmessages.mk
   patching file zrebuild.sh

4) The various .properties files need to compiled using zbmcompile to generate
   the message text Ada package and the various accessor packages.  The utility
   make file "zbmessages.mk" was created by the ZBIZED-4.4.1.PATCH file to
   do this compilation (it assumes "zbmcompile" is on the PATH):

   $ make -f zbmessages.mk
   zbmcompile -v -i -a -S zbstamp.gps -o gps_text GPS_Text -d action_edi....
   This is ZBMCompile, Version 0.2.0 ALPHA (r2138M) at 10:27 AM on 9/13/11
   Copyright (c) 2009-2011, Michael Rohan.  All rights reserved
   Loaded 15 messages for the facility "Action_Editor" (1 locales)
   Loaded 60 messages for the facility "Ada_Module" (1 locales)
   ...
   Generated accessor package body "GPS_Text.Widgets_Wide_Strings" to "gps....
   Created the stamp file "zbstamp.gps" containing the time stamp 10:28 AM 9/13/11
   ZBMCompile completed at 10:28 AM on 9/13/11, elapsed time 0:01:03.206

5) With the accessor and message packages in place, a standard configure
   should be run:

   $ ./configure --prefix=$HOME/GPS
   checking build system type... i686-pc-linux-gnu
   checking host system type... i686-pc-linux-gnu
   checking target system type... i686-pc-linux-gnu
   checking for xmlada.gpr... with_xmlada
   ...
   config.status: creating distrib/gnatcoll_gmp.gpr
   config.status: creating docs/gnatcoll_include.texi

6) The GPS executable built:

   $ make
   make -C gps default
   make[1]: Entering directory `/home/mrohan/GPS/4.4/src/gps-4.4.1-src/gps'
   make -s LIBRARY_TYPE=static ENABLE_SHARED=no \
             -C ../templates_parser setup
   ...
   ar cr libgps.a ...
   ranlib libgps.a
   gcc gps-main.o
   make[1]: Leaving directory `/home/mrohan/GPS/4.4/src/gps-4.4.1-src/gps'

7) Install the generated executable:

   $ make install
   mkdir -p /home/mrohan/GPS/bin
   mkdir -p /home/mrohan/GPS/share
   mkdir -p /home/mrohan/GPS/share/examples/gps
   ...
   make[2]: Leaving directory `/home/mrohan/GPS/4.4/src/gps-4.4.1-src/kernel/src_info/sn/snsrc'
   make[1]: Leaving directory `/home/mrohan/GPS/4.4/src/gps-4.4.1-src/kernel/src_info/sn/snsrc'

8) Run the executable.  Running with no options gives the standard GPS
   interface:

   $ $HOME/GPS/bin/gps

   See the image gps-screenshot-normal.jpg

   Running with uppercased pseudo translation gives a similar interface but
   the GPS text is uppercased (since Gtk will associate "FILE" with "File",
   the menus are not disturbed).  Substituted, runtime, messages arguments
   are delimited by chevrons (French quote characters):

   $ $HOME/GPS/bin/gps --pseudo=u
   Enabling pseudo-translations

   See the image gps-screenshot-pseudou.jpg

   Running with Halfwidth and Fullwidth Forms gives an interface that is
   quite different as "File" will no longer match "Ｆｉｌｅ":

   $ $HOME/GPS/bin/gps --pseudo=h
   Enabling pseudo-translations

   See the image gps-screenshot-pseudoh.jpg

   The message compilation includes the message text as a comment on the
   generated accessor routines.  See the image gps-formatdesc.jpg for an
   example of the display with GPS of the GPS source.
