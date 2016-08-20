.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

.. _zb-intro:

Introduction
************

This manual describes the use of the ZanyBlue libraries, an Ada framework for
finite element analysis with supporting libraries which might be of general
use in other projects.  The initial implementation contains the ``Text``
package (and supporting packages and utilities) supplying functionality
mirroring the Java ``MessageFormat`` package.  The ``Text``
functionality allows the application messages to be externalized in
``.properties`` files and localized into additional languages.  As a
major side effect the formatting of arguments within messages is needed and
implemented.

This, naturally, led to a need for a testing framework.  Ahven is used for
unit testing of the libraries, however, testing of the command line utilities
proved more difficult.  While DejaGNU was available, it seemed a little more
involved than was needed for this project.  A new testing application was
written, ``zbtest``, which is a hierarchical testing framework supporting
definition scopes.

.. _zb-intro-build:

Building the Library and Applications
=====================================

Building the ZanyBlue library and applications does not require a configure
step (at this time).  The build requirements are similar on Unix (Linux)
and Windows (the Makefiles use the environment variable ``OS`` to determine
whether the build is Linux (the variable is not defined) or Windows (where it
is defined with the value ``Windows_NT``).

.. _zb-intro-build-reqs:

Requirements
------------

The build requires

* A recent version of GNAT (e.g., GPL 2010, 2011) from AdaCore.
* GNU make.
* The XMLAda package needs to be installed to build the ZanyBlue
  testing application, ``zbtest``.
* The unit tests require the Ahven package

.. _zb-intro-build-building:

Building
--------

Once the required environment is in place, building is simply a matter of
issuing ``make`` in the ``src`` directory, e.g.,::

    $ make -C src

This produces the library in the ``lib/zanyblue`` directory,
and the two executables ``zbmcompile``, the message compiler,
``zbtest`` the ZanyBlue testing application and ``zbinfo`` a utility
to query built-in library configuration.  The ZanyBlue
package spec files and generic bodies are copied to the
``include/zanyblue`` directory.

For example, to examine the built-in locale data, e.g., to see the date, time,
numeric, etc data for Swedish, use the ``zbinfo`` command::

    $ zbinfo --dump-locale sv

.. _zb-intro-gnat:

.. _zb-intro-build-gnat:

Using with GNAT
---------------

A ZanyBlue GNAT ``gprbuild`` project file is available in the
directory ``lib/gnat`` directory.  Adding this directory to
your ``ADA_PROJECT_PATH`` should allow the use of the ZanyBlue
library with ``gprbuild`` projects.  See the text examples and
the GPS build instructions.

.. _zb-intro-testing:

Testing
=======

Testing is via the ``check`` target.  From the ``src``
directory this will run both the unit and system tests.  If experimenting,
running these tests independently is probably more useful (the combined
testing is normally only run from under Jenkins where the summary XML files
are loaded as the test results for the build)::

    $ make -C src/test/unittest check
    $ make -C src/test/system check

.. _zb-intro-examples:

Examples
========

The examples include a Gtk example.  If Gtk is not installed, this example
will fail to build (but the other should be OK).  The ``dumplocale`` from
previous releases has been replaced by the ``zbinfo`` utility.

.. _zb-intro-windows:

Windows Issues
==============

The installation of the Windows `GNU Win32 version of make
<http://gnuwin32.sourceforge.net/packages/make.htm>`_, does not update
the Path environment. This should be done manually after installing
via the Control Panel -> System -> Advanced
-> Environment Variables -> Path and adding the
path ``C:\Program Files\GnuWin32\bin``.

The majority of the example application are text based. This is
problematic on Windows systems as the standard Windows command console
does not understand Unicode output. This makes the Gtk example application
the only fully functional example application on Windows systems. To
build this application, the GtkAda package should be installed, again from
AdaCore and, as for AUnit, the gprbuild project path environment variable
will need to be set if GtkAda is not installed in the default location.
