.. -*- coding: utf-8 -*-
   Copyright © 2018, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

.. _zb-ahven:

The Ahven Package
=================

The ZanyBlue unit testing is implemented against the `Ahven
Package <http://ahven.stronglytyped.org/>`_.

If running on a system using the OS packaged GCC/GNAT compiler,
the Ahven package is frequently also available, e.g., Fedora or
Ubuntu.

If using the GNAT community edition, Ahven needs to be built from
sources.  The package is available for `Download
<http://ahven.stronglytyped.org/#download>`_.
For GNAT 2018, and likely later versions, the as downloaded
package needs to be patched (to handle stricter type checking).
This patch is available in the ``src/admin`` directory as
``ahven-gnat2018.patch``.  The build process used by ZanyBlue is basically:

#. Extract the Ahven tar bundle::

    $ tar xvzf ahven-2.6.tar.gz

#. Copy the file ``ahven-gnat2018.patch`` from ZanyBlue ``src/admin``
   directory to the ``ahven-2.6`` directory.

#. Apply the patch::

    $ cd ahven-2.6
    $ patch -p1 < ahven-gnat2018.patch

.. note::
   This patch disables the Sphinx based build of the documentation as this
   requires the installation of Sphinx.  If documentation is needed, Sphinx
   will need to be installed (along with the ``sphinxcontrib-adadomain``
   package and ``html`` Makefile target should also be built.

#. Configure and build Ahven.  The ``prefix`` Makefile variable should
   be set to the location where GNAT is installed.  On an Ubuntu system,
   the following can be used::

    $ GNATEXE=$(which gnat)
    $ GNATBINDIR=$(dirname $GNATEXE)
    $ GNATDIR=$(dirname $GNATBINDIR)
    $ make prefix=$GNATDIR all

   If GNAT 2018 was installed in the standard location on Linux, this should
   be equivalent to::

    $ make prefix=/opt/GNAT/2018 all

#. Once built, the binaries should be installed.  This will normally require
   ``root`` access, e.g., ``sudo`` here::

    $ sudo make install

.. note::
   If using the Ubuntu packages GNAT compiler, the ``prefix`` will be
   ``/usr``.  This will install the GPR file in ``/usr/share/gpr``.
   Since ``gprbuild`` does not appear to check this directory, the
   environment variable ``GPR_PROJECT_PATH`` should be defined with
   this directory value.
