.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

ZanyBlue Library and Tools
==========================

This documentation covers version |version| of the ZanyBlue library and
applications.  For download information see :ref:`zb-download`.  If
you encounter bugs in this software, please file reports in the
`Source Forge Bug Tracker <http://sourceforge.net/tracker/?group_id=225351>`_.
You can also visit the `Source Forge ZanyBlue Project Page
<http://sourceforge.net/projects/zanyblue/>`_.

.. only:: html

   This documentation is also available as a `single PDF file <ZanyBlue.pdf>`_.

Globalization Support
---------------------

The initial functionality covers globalization support for Ada.  Globalization,
(normally abbreviated to simply g11n) covers support within the source code
for multiple languages (this is termed internationalization or i18n) and the
ability to supply translations for strings used in an application
(localization or l10n).  Globalization is supported by the ZanyBlue
text library by providing routines to access and format messages in Ada
sources (the i18n aspect of g11n) and localization via the externalization
of messages strings to properties files.  For example, the following properties
file defines a simple message refered to in the source code via the key
``Hello``::

    Hello=Hello World, I am {0}

An Ada application code could print this message using the accessor routine

.. code-block:: ada

    Name : constant String := "Michael";
    ...
    Print_Hello (+Name);

See :ref:`zb-text` documentation and :ref:`zb-text-zbmcompile` application.

Parameter Handling
------------------

To support command line handling and user defined parameters and scopes
in the ``zbtest`` application, the ``Parameters`` packages are
available, e.g.,

.. code-block:: ada

    Parameters : Parameter_Set_Type;
    ...
    Parameters.Set_Boolean ("verbose", True);
    if Parameters.Get_Boolean ("verbose") then
        Print_Debug_Banner;
    end if;

Testing Application
-------------------

The stability of the code based is improved via a large set of tests.  These
tests are split between AUnit based unit-tests and a set of tests designed to
exercise the ZanyBlue utilities, more black-box style testing.  These tests
use the ``zbtest`` utility.  There is no formal documentation on this
at this time other than the built-in help and the example test scripts in
the ``tests`` directory.

License
-------

ZanyBlue is distributed under the terms of a BSD-style license.  See 
:ref:`zb-license` for details.

ZanyBlue Documentation
======================

.. toctree::
   :maxdepth: 2

   license
   download
   intro
   releases
   root
   text/index.rst
   utilities/index.rst
   docs
   contribs
   notices
   references

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

