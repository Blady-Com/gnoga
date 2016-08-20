.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

.. _zb-text-zbmcompile:

The zbmcompile Utility
----------------------

The ``zbmcompile`` utility compiles ``.properties`` files into
an Ada representation allowing easier access to the message text via
lookup in a catalog.

The simplest usage of the utility defines the name of the package to be
created and the facility to be compiled, e.g., for the moons example
application::

   $ zbmcompile -i -v Moons_Messages moons
   This is ZBMCompile, Version 1.3.0 BETA (r3009M) on 6/20/16 at 8:50 AM
   Copyright (c) 2009-2016, Michael Rohan.  All rights reserved
   Loaded 20 messages for the facility "moons" (4 locales)
   Performing consistency checks for the facility "moons"
   Loaded 1 facilities, 5 keys, 4 locales and 20 messages
   Loaded total 677 characters, stored 677 unique characters, 0% saving
   Wrote the spec "Moons_Messages" to the file "./moons_messages.ads"
   Wrote the body "Moons_Messages" to the file "./moons_messages.adb"
   ZBMCompile completed on 6/20/16 at 8:50 AM, elapsed time 0:00:00.033

Here the generated package ``Moons_Messages`` is compiled from the
``.properties`` files for the ``moons`` facility in the current
directory.

The utility gives usage information when given the ``-h`` option:

.. command-output:: zbmcompile -h

Controlling Status Output
^^^^^^^^^^^^^^^^^^^^^^^^^

The ``zbmcompile`` utility supports options to control the amount
of status information printed:

``-q``
    Reduced the amount of output to just error and warning messages.

``-v``
    Increase the amount of output generated.

``-D``
    Increase the amount of output to aid debugging.

Definition of Properties Directory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The default usage assumes the ``.properties`` files are located
in the current directory.   To locate the files in another directory,
the ``-d`` option can be used, e.g.,::

   $ zbmcompile -i -v -d mesg Moons_Messages moons

would locate the properties associated with the ``moons`` facility
in the ``mesg`` directory.

Properties File Extension
^^^^^^^^^^^^^^^^^^^^^^^^^

The default file extension used when locating properties files is
``.properties``.  This can be change using the ``-e`` option.
E.g., to load all ``.msg`` files,::

   $ zbmcompile -i -v -e .msg Moons_Messages moons

Auto-Initialization
^^^^^^^^^^^^^^^^^^^

The generate Ada code include an initialization routine which loads
the messages into a catalog (defaulting to the standard global
catalog).  The ``-i`` option includes a call to this initialization
procedure in the body of the generated package.  This allows the
inclusion of the message in an application by simply including the
specification in a compilation unit, normally the main unit.  The
option also causes the inclusion of a warning suppression pragma
in the specification to allow compilation in a strict compilation
environment.

Optimization of Messages
^^^^^^^^^^^^^^^^^^^^^^^^

When the ``zbmcompile`` loads facilities in sequence which, in
general, distributes the messages associated with various locales.
The ``zbmcompile`` optimize mode, the ``-O`` option, performs
a second pass on the loaded messages gathering messages for each
locale together.  Since applications generally don't change locale
very often, if at all, having all the message strings for a locale
located in the same set of pages can improve performance.

For symmetry reasons, the ``-g`` option is included which
disables optimization.

Locale Selection
^^^^^^^^^^^^^^^^

Occasionally, only a subset of the ``.properties`` files should
be compiled into the generated Ada package.  This selection is supported
using the ``-B``, for base locale, and ``-L`` options.  E.g., to
generate the Moons message package for just the base language, French and
German, the command would be::

   $ zbmcompile -v -B -L fr -L de Moons_Messages moons

These option could be used to generate language packs, possibly via shared
library or dll implementations.

Forced Compilation
^^^^^^^^^^^^^^^^^^

When testing using messages from Java projects, the message files will
frequently be found to contain errors from a \verb|zbmcompile| point of
view.  To force the generation of Ada in the context of input errors, the
``-F`` can be used.  Note, when used, there is no guarantee the resultant
generated Ada code will compile.

Definition of External Initialization Routine
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The possible direction for Ada localization is to allow the loading of
language pack at run-time via shared library or dlls.  This has not been
investigated but in the context of dynamic loading of shared libraries or
dll's, having an initialization name that is well defined makes the
implementation easier.  To support this, the untested functionality of
supplying a linker name for the initialization routine is allowed via the
``-x`` option, e.g.,::

   $ zbmcompile -v -B -x "moons_messages" Moons_Messages moons

Generation of Accessor Packages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The generation of message accessor packages creating routines for each
message defined with parameter lists matching the arguments defined by the
message text is controlled via the two options ``-a`` and ``-G``.

The first style, ``-a``, generates all accessor style packages:

#. ``exceptions``, generate routines to raise exceptions with localized
   message strings (wide strings converted to encoding associated with the
   locale).

#. ``strings``, generate functions returning locale encoded strings for
   the localized messages.

#. ``wstrings``, generate functions returning wide strings for the
   localized messages.

#. ``prints``, generate routines printing the localized messages to
   files as locale encoded strings.

#. ``wprints``, generate routines printing the localized messages to
   files as wide strings (wide files).

The ``-G`` option allows the selection of individual accessor style
packages, e.g., for an application that only uses messages to raise
exceptions and print to wide files, the command line options would be
``-G exceptions`` and ``-G wprints``, i.e., the ``-G`` option
can used multiple time on the same command line.

The packages generated are child packages of the primary package given on
the command line with names based on the facility name, e.g., if the
facility name is "Moons", the generated child packages would be

+----------------+------------------------+
| Style          |  Child Package Name    |
+================+========================+
| ``exceptions`` | ``Moons_Exceptions``   |
+----------------+------------------------+
| ``strings``    | ``Moons_Strings``      |
+----------------+------------------------+
| ``wstrings``   | ``Moons_Wide_Strings`` |
+----------------+------------------------+
| ``prints``     | ``Moons_Prints``       |
+----------------+------------------------+
| ``wprints``    | ``Moons_Wide_Prints``  |
+----------------+------------------------+

Handling non-Ada Message Keys
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When generating accessors, the keys in the various properties files
are assumed to be valid Ada identifiers.  ``zbmcompile`` raises an
error if it encounters non-Ada identifier keys, e.g., keys containing
periods.

The command line option ``-X`` can be used to define how such keys
should be handled with values of ``error`` to generate errors for such
keys and abandon the generation process (this is the default behaviour),
or ``ignore`` to simply ignore such keys and continue generating accessors
for the valid Ada identifier keys.

Output Directory
^^^^^^^^^^^^^^^^

By default, the generated packages are written to the current directory.  To
select a different directory, the ``-o`` option can be used, e.g.,::

   $ zbmcompile -o mesg Moons_Messages moons

would write the packages to the ``mesg`` directory.  The directory must
already exist.

Adjusting the Generated Code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ```zbmcompile`` command has a number of options used to adjust the
generated code.

Comments for Accessors
""""""""""""""""""""""

By default, the generated routines for accessors include the base message
text as a comment.  This allows the display of the text of the messages within
GPS and makes browsing the source easier.  These comments can be suppressed
using the ``-C`` option.  One reason to suppress these comments would be
to minimize recompilations when updating messages.  The ``zbmcompile``
command will only create new source files if the generated contents differs
from the existing files (or the files currently doesn't exist).  With
comments suppressed, updating message strings would only result in the update
to the primary package body requiring, in general, a single recompilation and
re-link of an application.  With comment enabled, the accessor spec files
would be updated resulting in larger recompilations.

Argument Modes
""""""""""""""

The default code generated does not include the ``in`` keyword for in
routine arguments.  Some code bases might have style rules requiring explicit
use of this keyword.  To require the generated code include this keyword, the
``-m`` option can be used.

Positional Elements
"""""""""""""""""""

The generated code includes a number of initialized tables (arrays).  The
default style for such tables is simply to list the entries using implicit
index association.  Again, some code bases might require that explicit
numbering of such code.  This can be enabled using the ``-p`` command
line option.  E.g., instead of

.. code-block:: ada

   Facilities : constant ZT.Constant_String_List (1 .. 7) := (
                   Facility_1'Access,
                   Facility_2'Access,
   ...

the generated code would be

.. code-block:: ada

   Facilities : constant ZT.Constant_String_List (1 .. 7) := (
                   1 => Facility_1'Access,
                   2 => Facility_2'Access,
   ...

Output Line Lengths
"""""""""""""""""""

The generated code keeps within the standard 80 column style for source files.
There are two control parameters which can be used as arguments to the ``-T``
command line option to adjust this for selected items:

#. The accumulated message strings are stored as a single constant string
   initialized using a multi-line string.  The length of the substrings
   written per line can be controlled using the command line ``pool``
   item with an integer value, e.g., ``-T pool 30`` to reduce the size.

#. The base message text written as a comment on accessors is wrapped to
   ensure the 80 column limit is not exceeded.   This results in wrapping
   within words.  To increase the limit for messages, use the ``comment``
   item, e.g., ``-T comment 120``.  Accessor comments include breaks
   for messages with new lines.

Consistency Checks
^^^^^^^^^^^^^^^^^^

Localized messages are cross checked with the base locale messages to ensure
they are consistent, i.e., it is an error for a localized message to refer
to an argument not present in the base message.

There are two options that control the consistency checks performed

#. ``-u`` disable the consistency checks.  This is a rather dangerous
   thing to do and should be avoided.

#. ``-r`` define the base locale.  Normally the base locale messages
   are in the properties file without a locale, i.e., the root locale.
   Some applications might choose to use explicit locale naming for all
   properties files.  The ``-r`` option can be used to designate which
   of the available locales is the base locale.

Selection of Source Locale
^^^^^^^^^^^^^^^^^^^^^^^^^^

The source locale for the base properties file can be specified using the
``-s`` option.  See :ref:`zb-text-sourcelocale` for details on this
functionality.

Generating ASCII Only Sources
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, the ``zbmcompile`` utility generates UTF-8 encoded source files,
in particular, the ``Wide_String`` used to store the compiled messages is a
UTF-8 encoded string in the source file.

It is assumed the compilation environment will be configured to use UTF-8
encoded sources, e.g., the GNAT ``-gnatW8`` compilation option.  The
``zbmcompile`` command line option ``-A`` causes the generated source files
to used string concatenation along with ``Wide_Character'Val`` for non-ASCII
characters in these strings.

Stamp File
^^^^^^^^^^

For Makefile base builds, dependency checking is simplified if a single fixed
named file can be used rather than the set of generated spec and body files
normally created by ``zbmcompile``.  The ``-S`` option allows the
definition of a simple time stamp file which is updated whenever the
``zbmcompile`` command is run.  This file can be used to define built
dependencies in Makefiles.
