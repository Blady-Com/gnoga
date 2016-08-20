.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

.. _zb-root:

The Root Package
================

The ZanyBlue root package, ``ZanyBlue`` contains definitions
about the library, primiarly version information.  The version
numbers associated with the library are available via the
functions

* ``Version_Major``, the major version number for the ZanyBlue
      release.
* ``Version_Minor``, the minor version number for the ZanyBlue
      release.
* ``Version_Patch``, the patch number for the ZanyBlue release.

Each release also has a release status given by the function:

.. code-block:: ada

   function Version_Status_Type return Version_Status_Type;

The return value is an enumeration type: Alpha, Beta, Production.

To futher refine the release identification, a source code
revision value (a wide string) is returned by the function
``Revision``.  The source code system is currently Subversion
and this string has the format of the letter ``r`` followed by the
Subversion revision number, e.g., ``r2500``.

Finally, the copyright year for the bundle is given by the function:

.. code-block:: ada

   function Copyright_Year return Positive;

This is current year at the time of the build.
