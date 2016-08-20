.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Locale Type
-----------
   
The ``Locale_Type`` defines a locale which is used to select localized
messages at run time.  The type basically maps to the standard ISO names
used to identify the language, script and territory for the locale.  Typical
examples of a locale name are

#. ``fr`` for French.  Here only the language abbreviation is
   used.  Here only the language is specified.

#. ``en_US`` for English as spoken in the United Stated, where
   the language and territory are specified.

#. ``zh_Hant`` for Traditional Chinese, where the language and
   script are specified.

#. ``zh_Hans_CN`` for Simplified Chinese as spoken in China
   where language, script and territory are specified.

Language and territory abbreviations must be either 2 or 3 characters in length,
script abbreviations must be 4 characters.  For a list the various language,
script and territory codes, see the source properties files in
``src/text/mesg``.

Locale Encodings
^^^^^^^^^^^^^^^^

The system encoding name is normally appended to the this value separated
by a period, e.g., ``en_US.UTF-8``.  The ZanyBlue library respects this
encoding information and associates a ``Codecs`` object with the locale
to encode and decode from and to ``Wide_String``'s.  The ``Codecs_Type``
assocated with a locale is available via the ``Codecs`` method, e.g.,

.. code-block:: ada

   Current_Codecs : constant Codecs_Type := Current_Locale.Codecs

See :ref:`zb-text-encoding` for more information on the ``Codecs_Type``.

Locale Resolution
^^^^^^^^^^^^^^^^^

When accessing localized data, e.g., a message for a facility or some built-in
localized data, the ZanyBlue library will perform the standard search through
the locales for the data starting with the user supplied locale (normally the
standard, current, locale).

This search applies rules to move from more specific locales to more general
locales walking up a virtual tree of locales until the requested data is found
or the root locale is reached.

The algorithm implemented in the ZanyBlue library is a general locale
parenting algorithm which does the obvious for simple language and territory
locales.  E.g., the parent of the locale ``de_DE`` is ``de``
which, in turn, has as it's parent the root locale.  A similar algorithm is
used for simple language and script locales, e.g., the parent of the locale
``en_Latn`` is ``en``, which, again, has as it's parent the root locale.

The locale parenting algorithm for full language, script and territory
locales will have an ancestor tree of language and script, then language
and territory, then language and finally the root locale.  For example,
the sequence of locales tried for the locale "``fr_Latn_FR``" is

#. ``fr_Latn_FR``

#. ``fr_Latn``

#. ``fr_FR``

#. ``fr``

#. Root Locale

This locale resolution occurs at run-time when a message for a particular
facility is requested.  The locale resolution for the built-in locale
specific data, e.g., day names, time formats, etc., occurs at compile time.
E.g., to access the name associated with Sunday requires only a few table
lookups are at runtime, e.g.,

+---------------------------------------------+----------------+
| Function Call                               | Result         |
+=============================================+================+
| ``Full_Day_Name (Make_Locale ("en"), Sun)`` | ``"Sunday"``   |
+---------------------------------------------+----------------+
| ``Full_Day_Name (Make_Locale ("fr"), Mon)`` | ``"lundi"``    |
+---------------------------------------------+----------------+
| ``Full_Day_Name (Make_Locale ("de"), Tue)`` | ``"Dienstag"`` |
+---------------------------------------------+----------------+

Note: applications would normally just format, via message strings, values,
e.g., ``Time`` values and let the type formatter access the lower level
localized values, in this case when formatting ``Time`` values as dates,
the localized date names might be accessed depending on the format style and
locale.

It should also be noted the values returned for localized data are
``Wide_Strings`` and generally contain non-ASCII characters.  Ad hoc
testing on modern Unix (Linux) systems using X-Windows will display the correct
characters (with the cooperation of the compiler, e.g., the ``-gnatW8``
switch for GNAT).  On Windows, the DOS command will will normally *not*
display such character correctly.  It is possible to enable display of non-ASCII
characters via the selection of a code page for the command window.

Creating Locales
^^^^^^^^^^^^^^^^

Values of ``Locale_Type`` are created via the ``Make_Locale``
function, e.g.,

.. code-block:: ada

   My_Locale : constant Locale_Type := Make_Locale ("en_IE");

.. _zb-text-changelocale:

Changing Default Locale
^^^^^^^^^^^^^^^^^^^^^^^

The ZanyBlue Text routines allow the explicit definition of the locale for a
particular function/procedure call but this normally not needed allowing the
locale to default to the currently defined locale.  The default locale is
taken from the process environment via the variable ``ZB_LANG``, and,
if that is not defined, uses

#. On Unix systems, the value of the environment variable LANG.

#. On Windows systems, the translation of the user's default LCID value
   to a standard locale name (language, script and territory triple).

The default locale used can be adjusted at run time using the
``Set_Locale`` routine, e.g., to explicitly set the locale to Canadian
French, the call would be

.. code-block:: ada

   Set_Locale (Name => "fr_CA");

The Makefiles for the example applications generally include ``run``
targets which run the applications in the default locale.  They also include
rules to run application in other locales by tagging on the locale name to
``run_``, e.g., to run the \texttt{tomcat} example in a Greek locale,
the command would be::

   $ make run_el

Creating Locales
^^^^^^^^^^^^^^^^

The default locale is created on application started and defaults to the locale
associated with the running process.  This is normally the expected locale.
A locale can be explicitly created using the three variations of the
``Make_Locale`` function:

Simple String Locale Name
"""""""""""""""""""""""""

Creating a locale using a simple string contains the locale information of language,
script, territory and encoding (not all are required, e.g.,:

.. code-block:: ada

   en : constant Locale_Type := Make_Locale ("en");
   en_IE : constant Locale_Type := Make_Locale ("en_IE");
   en_Latn_IE : constant Locale_Type := Make_Locale ("en_Latn_IE");
   en_Latn_IE_ISO1 : constant Locale_Type := Make_Locale ("en_Latn_IE.ISO8859-1");

.. todo:: Document other Make_Locale routines

.. _zb-text-sourcelocale:

Message Source Locale
^^^^^^^^^^^^^^^^^^^^^

The current locale is used to locate localized messages in a facility,
e.g., in a French locale, messages defined by the ``_fr``
properties files would be used, if available.  If not available,
then the messages defined by the base properites file will be used.
If the application is developed in an English environment, then the base
properties file will normally contain English messages.  However, it is
not uncommon to have non-English developers choose to use an English
base properties file.

By default, the current locale is also used to format arguments to messages.
This has the biggest impact for dates and times.  For example, the following
message::

    Today=Today is {0,datetime,EEEE}.

will print the day name using the code

.. code-block:: ada

   Print_Today (+Clock);

In an English locale, this will print the message::
    Today is Thursday.

If the same application is run in a French locale and a French localization
of the message is not available for the message, then the base message text
is used, i.e., the English text.  However, if the French locale will be used
to format the date/time argument generating the message,::

    Today is jeudi.

Such mixed language messages should normally be avoid in applications.  It
is better the entire message be in English, even in non-English locales.

To support this functionality, the ZanyBlue Text library associates a locale
with each message.  For localized messages, this is the locale associated with
the localized property file, e.g., messages in the file
``App_fr.properties`` will have the locale ``fr`` associated
with them.  Using the associated message locale is controlled via the enable
and disable source locale routines.  This is enabled by default.  See the
source locale text example.

For the base message file, the default locale associated with the messages
is the root locale.  This should normally be explicitly set using the
``zbmcompile`` ``-s`` option.
