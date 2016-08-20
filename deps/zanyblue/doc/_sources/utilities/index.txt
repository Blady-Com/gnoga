.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

.. _zb-utilities:

Command Line Utilities
======================

The command line utilites available are generally covered in other areas
of the documentation.  The full documentation for some general utilities
is given here, the other utilities will simply refer back to the section
that covers them.

.. _zb-utilities-zbinfo:

The ``zbinfo`` Utility
----------------------

The ``zbinfo`` utility prints information on data built into the ZanyBlue
library.  The utility usage is

.. command-output:: zbinfo -h

The ``zbmcompile`` Utility
--------------------------

The ``zbmcompile`` utility compiles message properties files into Ada
sources for use in applications.  It is described in the section
:ref:`zb-text-zbmcompile`.  It's usage is

.. command-output:: zbmcompile -h

The ``zbtest`` Utility
----------------------

The ``zbtest`` utility is a simple application testing utility which
compares expected output (with regex masking) for a commmand with the
output actually generated.  It has a hierarchical test script structure
based on the testing directory structure.  At this point, the only
documentation of this utility is the source and the test scripts in
the ``src/test/system`` directory.  It's usage is

.. command-output:: zbtest -h
