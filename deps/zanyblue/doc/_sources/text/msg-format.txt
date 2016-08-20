.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Message Formatting
------------------
   
From the simple ``moons`` example above, it can be seen that simply
externalizing just the text used to in an application without the argument
formatting is not enough to fully support localization.  The messages
externalized must be the complete messages, i.e., sentences, with embedded
place holders for the arguments substituted at runtime.

The ZanyBlue Text library currently uses a mixture of Java and Python styles
for embedded arguments.  Arguments to the message are referenced by index
(zero based) and enclosed in chain brackets.

From the ``moons`` example, message ``0002`` has the definition::

   0002=There are {0} known moons orbiting "{1}".

Here the first argument (argument 0) is an integer and second argument
(argument 1) is an enumeration value giving the planet name (but this is
not explicitly defined in this message, see :ref:`zb-text-format-type` for
information on giving explicit type information).

At runtime, arguments to messages are "boxed" into a tagged type with
dispatching methods that perform the formatting to strings.  Each type has
it's own implementation (see :ref:`zb-text-argtypes`).  Boxing occurs by
creating a boxed object for the argument value and passed to the various
message formatting or printing routines.  The boxing function has a standard
``Create`` function and a renaming ``+`` making formatting or printing calls
look more natural.  For example, the message text above could be formatted
with an integer argument, 2, and a string argument, ``Mars``, as

.. code-block:: ada

    Message : Wide_String := Format ("moons", "0002", +2, +String'("Mars"));

or, if the accessor package is used,

.. code-block:: ada

    Message : Wide_String := Format_0002 (+2, +String'("Mars"));

The explicit type coercion to ``String`` is required as both standard
``String`` and ``Wide_String`` are supported.

The formatting or printing routines accept up to 5 optional boxed arguments.
The implementation gathers the supplied boxed arguments into an argument
list and then calls the underlying formatting routine with the argument
list.  For argument numbers beyond 5, the underlying argument list type must be
used and the arguments must be explicitly appended, e.g., the above
example could be rephrased in terms of the lower level argument list based
formatting routine as

.. code-block:: ada

   declare
      Arguments : Argument_List;
   begin
      Arguments.Append (1);
      Arguments.Append (String'("Mars"));
      Display_Message (Format ("moons", "0002", Arguments));
   end;

This 5 argument limit does *not* apply to the functions and procedures
generated for accessor packages.  These routines have arguments lists that
match the number of expected arguments without the 5 argument limit.
