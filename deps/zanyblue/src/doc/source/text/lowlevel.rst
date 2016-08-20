.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Example Using Low Level Functions
=================================
   
As an introduction to the ``Text`` package, a simple example, which
continues the example based on the number of moons orbiting the various
planets, is developed here.

**Note**, the preferred way to access messages is using accessor
packages described in the next section.   The example here uses direct
calls using strings to identify facilities and key to demonstrate the
concepts involved.

The task is to ask the user for a planet name and print the number of currently
known moons for the planet.  The source for this simple example
is available in the directory ``examples/text/moons`` and is listed here:

.. code-block:: ada

   with Ada.Wide_Text_IO;
   with Moons_Messages;
   with ZanyBlue.Text.Formatting;

   procedure Moons is

      type Planet_Names is (Mercury, Venus, Earth, Mars,
                            Jupiter, Saturn, Uranus, Neptune);
      package Planet_Name_IO is
         new Ada.Wide_Text_IO.Enumeration_IO (Planet_Names);

      use Ada.Wide_Text_IO;
      use Planet_Name_Formatting;
      use ZanyBlue.Text.Formatting;

      Moons  : array (Planet_Names) of Natural := (
                      Earth => 1, Mars => 2, Jupiter => 63,
                      Saturn => 62, Uranus => 27, Neptune => 13,
                      others => 0);
      Planet : Planet_Names;

   begin
      loop
         Print ("moons", "0001");
         Planet_Name_IO.Get (Planet);
         if Moons (Planet) /= 1 then
            Print_Line ("moons", "0002", +Moons (Planet),
                                         +Planet_Names'Image (Planet));
         else
            Print_Line ("moons", "0003", +Planet_Names'Image (Planet));
         end if;
      end loop;
   exception
   when End_Error | Data_Error =>
       New_Line;
       Print_Line ("moons", "0004");
   end Moons;

The example source code uses the ``Generic_Enumerations`` ZanyBlue
package for planet name arguments.  Generic ZanyBlue packages should
be instanciated at the library level.

Here the messages for the application are referred to by message id or
key, i.e., numeric strings, e.g., ``0001`` for the facility
``moons``.

.. note:
   As a side note, the key is a general case sensitive string, however, the
   assignment of more meaningful names becomes more difficult as an application
   grows and it is simpler to abandon such names initially and use numeric
   string codes.

The text associated with these message keys are externalized to a
``.properties`` file.  For this example, the root properties file,
``moons.properties``, containing English, is::

   0001=Please enter a planet:
   0002=There are {0} known moons orbiting "{1}".
   0003=There is 1 known moon orbiting "{0}".
   0004=OK, goodbye.

A German translation of this properties, ``moons_de.properties``,
file would be (via Google Translate)::

   0001=Bitte geben Sie einen Planeten:
   0002=Es gibt {0} bekannte Monde umkreisen "{1}".
   0003=Es gibt 1 bekannt Mond umkreisen "{0}".
   0004=OK, auf Wiedersehen.

French and Spanish Google translated version of this file in the examples
directory.

The properties and application are tied together by "compiling" the
properties files into an Ada package and simply **with**'ing the
compiled package in the application (normally in the unit containing
the main procedure).  In this example, the generated package referenced
in the source above is ``Moons_Messages`` which is created using
the ``zbmcompile`` ZanyBlue message compiler utility:

.. command-output:: zbmcompile -i -v Moons_Messages moons
   :cwd: moons

This generates an Ada specification containing the name of the facility,
``moons`` in this case, and the definition of a single initialization routine.
The body file contains the message text data, the initialization routine which
adds the message data to a shared message pool.  Since the ``-i``
command line option was used, the generated package body includes a call
to the initialization routine allowing the message data to be included
in the application via a simple **with** of the package, i.e., no
explicit call to the initialization routine is needed.

The execution of the generated application will now display messages selected
for the run-time locale, e.g., in an English locale::

    $ make run
    ../../../bin/x_moons
    This is MOONS, Version 1.3.0 - BETA
    Please enter a planet: mars
    There are 2 known moons orbiting "MARS".
    Please enter a planet: earth
    There is 1 known moon orbiting "EARTH".
    Please enter a planet: mercury
    There are 0 known moons orbiting "MERCURY".
    Please enter a planet: ^D
    OK, goodbye.

And running the application selecting a German locale displays the German
messages::

    $ make run_de
    ../../../bin/x_moons de
    Dies ist MOONS, Version 1.3.0 - BETA
    Bitte geben Sie einen Planeten: mars
    Es gibt 2 bekannte Monde umkreisen "MARS".
    Bitte geben Sie einen Planeten: earth
    Es gibt 1 bekannt Mond umkreisen "EARTH".
    Bitte geben Sie einen Planeten: mercury
    Es gibt 0 bekannte Monde umkreisen "MERCURY".
    Bitte geben Sie einen Planeten: 
    OK, auf Wiedersehen.

The locale is selected via a command line argument here, however, the
environment variables can be used instead, see section
:ref:`zb-text-changelocale` for more details on selecting locales.

This example application did not attempt to localize the planet names.  A
more involved example would add another facility for the planet names with
message keys matching the ``Image`` of the enumeration values.
