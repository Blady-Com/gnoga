.. -*- coding: utf-8 -*-
   Copyright (c) 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

.. _zb-zbtest:

The ``zbtest`` Utility
======================

The ZanyBlue project uses both unit testing (via the :ref:`zb-notice-Ahven-2.6`
library) and system testing using the ZanyBlue testing utility ``zbtest``
utility.  This utility exercises the command line ZanyBlue utilities 
by executing them with different command line options and inputs and comparing
the generated output and files.  The ``zbtest`` utility uses the directory
tree to organize the tests into areas to test with tests deeper in the
directory tree being more specific.

A simple example will make this clearer::

    +- myapp
        +- myapp.zbt
        +- area1
            +- area1.zbt
            +- area1.in
            +- area1-01.log
            +- area1-02.log
        +- area2
            +- area2.zbt
            +- area2.tar.bz2
            +- area2-01.log
            +- area2-02.log
            +- area2-03.log

Here, the tests for the application ``myapp`` are contained in the directory
named for the application.  This contains the ``zbtest`` driver script, with
the same name as the directory, ``myapp.zbt`` along with two sub-directories
containing tests specific to the two areas of the application: ``area1`` and
``area2``.

ZBTest Documentation
--------------------

.. toctree::
   :maxdepth: 2

   command/index.rst
   function/index.rst
