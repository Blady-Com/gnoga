.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Message 'Metrics'
-----------------

Testing of internationalized applications (globalization, g11n, testing) is
slightly different from normal functional testing.  It is generally assumed
the core functionality of the application is not dependent on the the messaging.
Core testing is frequently driven by coverage data derived from the test
cases.  Performing testing in a globalized application to meet the same
coverage to generally not something that is needed.  The globalization
testing needs to verify the application is fully localized in the target
language and can handle localized input.  This is generally a subset of
the overall testing applied to an application.

To facilitate globalization testing, the library keeps track of the number
of times a message is used giving a coverage number, from a message point
of view.  The accumulated usage information can be saved to an XML file
using the ``Write_Usage`` routines in the text ``Metrics`` package.

The example application ``moons_accessors`` will generate a message
usage report if given two command line arguments (the first being the locale
name to use), e.g., for a simple execution with a usage file
``moons.zbmx`` given on the command line::

   $ x_amoons en moons.zbmx
   This is MOONS, Version 1.3.0 - BETA
   Please enter a planet: mars
   There are 2 known moons orbiting "MARS".
   Please enter a planet: 
   OK, goodbye.

The generate usage information looks like

.. code-block:: xml

   <?xml version="1.0" encoding="utf-8"?>
   <zanyblue-message-usage>
     <message facility="Moons" locale="de" key="0001" count="0" />
     <message facility="Moons" locale="" key="0003" count="1" />
     <message facility="Moons" locale="fr" key="0001" count="0" />
     <message facility="Moons" locale="de" key="0005" count="0" />
     <message facility="Moons" locale="" key="0004" count="0" />
     ...
     <message facility="Moons" locale="es" key="0001" count="0" />
     <message facility="Moons" locale="es" key="0003" count="0" />
   </zanyblue-message-usage>

The contents of the file has been truncated for display purposes.
