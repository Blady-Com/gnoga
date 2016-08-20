.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.


.. _zb-text-argtypes:

Argument Types
--------------
   
The formatting of messages with arguments is based on "boxing" message
argument data values.  The library provides a set of standard boxed types
corresponding to the standard set of Ada types.  Details on how the formatting
these types is detailed in the following sections.

Boolean Type
^^^^^^^^^^^^

Localized version of the English strings ``true`` and
``false`` are not available via the Unicode CLDR data.  Boolean
values are formatted as unlocalized English values.  Examples, for format
references are (the ``*`` is used to fill for readability):

+--------------------------------+------------------+
| Format                         | Result           |
+================================+==================+
| ``Format ("{0}", +True)``      | ``"true"``       |
+--------------------------------+------------------+
| ``Format ("{0,*>10}", +True)`` | ``"******true"`` |
+--------------------------------+------------------+
| ``Format ("{0,*<10}", +True)`` | ``"true******"`` |
+--------------------------------+------------------+
| ``Format ("{0,*^10}", +True)`` | ``"***true***"`` |
+--------------------------------+------------------+

Other formatting information is ignored, e.g., a formatting style character
associated with numeric types, etc.  The accessor argument type for Booleans is
``Boolean_Category_Type`` which is derived from the
``Enumeration_Category_Type``.

Character Type
^^^^^^^^^^^^^^

The character implementation simply inserts the character value as is
to the formatted output.  The library support both narrow and wide
characters.  Since ``Create`` exist for both, type information
must be supplied if literal values are used.  Field width, fill and
alignment are respected, e.g., for the character variable ``C``
with a value of ``a``:

+--------------------------------+------------------+
| Format                         | Result           |
+================================+==================+
| ``Format ("{0}", +C)``         | ``"a"``          |
+--------------------------------+------------------+
| ``Format ("{0,*>10}", +C)``    | ``"*********a"`` |
+--------------------------------+------------------+
| ``Format ("{0,*<10}", +C)``    | ``"a*********"`` |
+--------------------------------+------------------+
| ``Format ("{0,*^10}", +C)``    | ``"*****a****"`` |
+--------------------------------+------------------+

Other formatting information is ignored, e.g., the format style character, etc.
The accessor argument type for characters is ``Character_Category_Type``
which is derived from the ``String_Category_Type``, i.e., a character
is considered a "degenerate" string.

Duration Type
^^^^^^^^^^^^^
   
The standard implementation of the duration formatting displays the days,
hours, minutes and seconds (seconds as a floating point formatted
to three decimal places).  If the number days is zero, it's formatting is
suppressed.  The digits used are localized (this only impacts
Arabic locales).  Field width, fill and alignment are respected, e.g., for
the Duration variable ``D``

+--------------------------------+------------------------+
| Format                         | Result                 |
+================================+========================+
| ``Format ("{0}", +D)``         | ``"16:48:03.000"``     |
+--------------------------------+------------------------+
| ``Format ("{0,*>16}", +D)``    | ``"****16:48:03.000"`` |
+--------------------------------+------------------------+
| ``Format ("{0,*<16}", +D)``    | ``"16:48:03.000****"`` |
+--------------------------------+------------------------+
| ``Format ("{0,*^16}", +D)``    | ``"**16:48:03.000**"`` |
+--------------------------------+------------------------+

The accessor argument type for Durations is ``Duration_Category_Type``.

Float Types
^^^^^^^^^^^

The ``Float`` and ``Long_Float`` formatting are simply instantiations
of the ``Generic_Floats`` package, see :ref:`zb-text-generic-float`
for more information.  Some examples, details explained in the description of
the generic package:

+--------------------------------+------------------------+
| Format                         | Result                 |
+================================+========================+
| ``Format ("{0,e}", +Pi);``     | ``"3.14159E+00"``      |
+--------------------------------+------------------------+
| ``Format ("{0,f}", +Pi);``     | ``"3.141590"``         |
+--------------------------------+------------------------+
| ``Format ("{0,g}", +Pi);``     | ``"3.141590"``         |
+--------------------------------+------------------------+
| ``Format ("{0,.2f}", +Pi);``   | ``"3.14"``             |
+--------------------------------+------------------------+

The accessor argument type for floats is ``Float_Category_Type``
which is derived from the ``Real_Category_Type``.

Enumeration Types
^^^^^^^^^^^^^^^^^

Formatting of Ada enumeration types is handled via the generic package
``Generic_Enumerations`` which should be instanciated using the enumeration
type.  This allows enumeration values as arguments to messages.  Obviously,
no localization is available for the enumeration values (generated using
``Wide_Image``).  The generic package supplies a ``Create`` function
to create the ZanyBlue "boxed" value along with a renaming for ``+``.

Field width, fill and alignment are respected.

For example, for the definitions

.. code-block:: ada

    type Colour is (Red, Green, Blue);
    package Colour_Arguments is
       new ZanyBlue.Text.Generic_Enumerations (Colour);
    use Colour_Arguments;
    C : Colour := Red;

the example formatting is

+--------------------------------+------------------------+
| Format                         | Result                 |
+================================+========================+
| ``Format ("{0}", +C);``        | ``"RED"``              |
+--------------------------------+------------------------+
| ``Format ("{0,*<10}", +C);``   | ``"*******RED"``       |
+--------------------------------+------------------------+
| ``Format ("{0,*>10}", +C);``   | ``"RED*******"``       |
+--------------------------------+------------------------+
| ``Format ("{0,*^10}", +C);``   | ``"****RED***"``       |
+--------------------------------+------------------------+

The underlying implementation uses the ``Image`` function for the
enumeration value and, as a result, is normally an uppercase value.  The
format width and placement (center, left, right) is respected.
The accessor argument type for enumerations is
``Enumeration_Category_Type``.

If localization of the enumeration image is required, the application will
need to implement this.  One possibility would be to create another facility
that uses the enumeration image as a key and returns the localized value,
e.g., for the ``Colour`` example above::

    RED=Red
    GREEN=Green
    BLUE=Blue

and then use this facility to generate arguments, e.g.,

.. code-block:: ada

    Format ("The colour is {0}",
            +Format ("colours", Colours'Wide_Image (C)));

There is, of course, a lot more work if the application were to support
input of localized enumeration names.

.. todo:: Need to highlight generic instanciation requirement more

Generic ZanyBlue text packages should be instanciated at the library
level to prevent run time accessibility exceptions.

.. _zb-text-generic-float:

Generic Float Types
^^^^^^^^^^^^^^^^^^^
   
The ``Generic_Floats`` package implements formatting for floating point type.
The formatting is based on David M. Gay's [Gay90]_
algorithm and attempts to produce accruate representations of floating
point numbers (see also Guy Steele and Jon White's paper [Steele04]_).

The formatting numeric style is controlled by the formating style characters
(unlike C, the case of the character does not matter):

.. todo:: Check on Swedish localization for exponent floating format

``E``
    The floating point number is formatted using scientific notation, e.g.,
    ``1.23E+10``.  Note in addition to the digits, the decimal point, sign
    characters and the exponent character are localized, e.g., in a Swedish
    locale the exponent characters is displayed as ``times 10 to the power
    of``.

``F``
    The floating point number is formatted as a simple number.  Note, for large
    absolute values of the exponent the formatted value will be a very long
    string of mainly zero characters.

``G``
    This format chooses the shorter of the ``E`` end ``F`` formatting depending
    on the value being formatted.  For this release, the algorithm used is
    relative simplistic.

In additionl to the type characters, the formatting width and precision are
used when formatting floating point numbers:

**width**
    The total field width the formatted value should occupy.  If the formatted
    value is smaller than this width, the result is padded to fill to this
    width (see the alignment characters later).

**precision**
    The precision is the number of digit displayed after the decimal point.

The plus or space character can be used to force either a plus or space
character before the formatted number for positive values, negative numbers
always include a sign character.  The sign characters used are locale
defined.

.. todo:: Move note on decorator usage of '=' to integer section

The alignment and fill characters are used to pad the result to the requested
field width.  In addition to the left, right and center alignment, floating
point (and numeric values in general) also support the numeric alignment
character ``=`` which is simply a shorthand for align right using the
``0`` character for fill (this is the localized ``0`` character).  However,
to pad within base decorators, the ``=`` character must be used, e.g., to
generate ``16#003F#``.

As an example, the following table gives the various formatting options
for the ``Float`` value ``F := 2.9979E+8``:

+--------------------------------+------------------------+
| Format                         | Result                 |
+================================+========================+
| ``Format ("{0,e}", +F);``      | ``"2.99790E+08"``      |
+--------------------------------+------------------------+
| ``Format ("{0,f}", +F);``      | ``"299790000.0"``      |
+--------------------------------+------------------------+
| ``Format ("{0,g}", +F);``      | ``"2.99790E+08"``      |
+--------------------------------+------------------------+
| ``Format ("{0,.2f}", +F);``    | ``"299790000.00"``     |
+--------------------------------+------------------------+
| ``Format ("{0,.2e}", +F);``    | ``"3.00E+08"``         |
+--------------------------------+------------------------+
| ``Format ("{0,*>16e}", +F);``  | ``"*****2.99790E+08"`` |
+--------------------------------+------------------------+
| ``Format ("{0,=16e}", +F);``   | ``"000002.99790E+08"`` |
+--------------------------------+------------------------+

The implementation will use localized strings for infinity and *not a number*
when formatting such values.

The accessor argument type for floats is ``Float_Category_Type``
which is derived from the ``Real_Category_Type``.

Generic ZanyBlue text packages should be instanciated at the library
level to prevent run time accessibility exceptions.

.. _zb-text-generic-integer:

Generic Integer Types
^^^^^^^^^^^^^^^^^^^^^

The ``Generic_Integer`` package implements formatting for
integer types (generic argument type ``range <>``).

The formatting numeric style is controlled by the formatting style characters
which control the base used when formatting, whether the value has a base
decorator, handling of the sign, etc.  The base selector characters are
\begin{itemize}

+-----------------+------------------------------------------------------------+
| Style Character | Description                                                |
+=================+============================================================+
| ``b``           | Format the value in binary (base 2)                        |
+-----------------+------------------------------------------------------------+
| ``o``           | Format the value in octal (base 8)                         |
+-----------------+------------------------------------------------------------+
| ``d``           | Format the value in decimal (base 10), this is the default |
+-----------------+------------------------------------------------------------+
| ``x``           | Format the value in hexadecinmal (base 16), extra digits   |
|                 | are the lower case Latin characters ``a`` to ``f``.  The   |
|                 | CLDR data does not supply localized hexadecimal digits     |
+-----------------+------------------------------------------------------------+
| ``X``           | Format the value in hexadecinmal (base 16), extra digits   |
|                 | are the upper case Latin characters ``A`` to ``F``         |
+-----------------+------------------------------------------------------------+

If the format string includes the base decorator character ``#``
then the non-decimal format include the base, as per Ada syntax in the
formattted result.

The alignment and fill characters are used to pad the result to the requested
field width.  In addition to the left, right and center alignment, integer
(and numeric values in general) also support the numeric alignment
character ``=`` which is simply a shorthand for align right using the
``0`` character for fill (this is the localized ``0`` character).

As an example, the following table gives the various formatting options
for the ``Integer`` value ``I := 42``:

+--------------------------------+------------------------+
| Format                         | Result                 |
+================================+========================+
| ``Format ("{0}", +I);``        | ``"42"``               |
+--------------------------------+------------------------+
| ``Format ("{0,b}", +I);``      | ``"101010"``           |
+--------------------------------+------------------------+
| ``Format ("{0,o}", +I);``      | ``"52"``               |
+--------------------------------+------------------------+
| ``Format ("{0,x}", +I);``      | ``"2a"``               |
+--------------------------------+------------------------+
| ``Format ("{0,X}", +I);``      | ``"2A"``               |
+--------------------------------+------------------------+
| ``Format ("{0,#b}", +I);``     | ``"2#101010#"``        |
+--------------------------------+------------------------+
| ``Format ("{0,#o}", +I);``     | ``"8#52#"``            |
+--------------------------------+------------------------+
| ``Format ("{0,#x}", +I);``     | ``"16#2a#"``           |
+--------------------------------+------------------------+
| ``Format ("{0,#X}", +I);``     | ``"16#2A#"``           |
+--------------------------------+------------------------+
| ``Format ("{0,=#10X}", +I);``  | ``"16#00002A#"``       |
+--------------------------------+------------------------+

The accessor argument type for integers is ``Integer_Category_Type``
which is derived from the ``Number_Category_Type``.

Generic ZanyBlue text packages should be instanciated at the library
level to prevent run time accessibility exceptions.

Generic Modular Types
^^^^^^^^^^^^^^^^^^^^^

The ``Generic_Modulars`` package implements formatting for modular
types (generic type argument ``mod <>``).  This is essentially the same
as the generic integers: the same rules apply, see
:ref:`zb-text-generic-integer`.

The accessor argument type for integers is ``Modular_Category_Type``
which is derived from the ``Integer_Category_Type``.

Generic ZanyBlue text packages should be instanciated at the library
level to prevent run time accessibility exceptions.

Integer Type
^^^^^^^^^^^^

This is simply an instantiation of the ``Generic_Integers`` package
for the standard ``Integer`` type.  See the generic integers description
in section :ref:`zb-text-generic-integer`.

String Types
^^^^^^^^^^^^

The string implementation simply inserts the string value as is
to the formatted output.  The library support both narrow and wide
fixed and unbounded strings.  Since ``Create`` exist for both,
type information must be supplied if literal values are used.  Field width,
fill and alignment are respected, e.g., for the string variable ``S``
with a value of ``abc``:

+--------------------------------+------------------------+
| Format                         | Result                 |
+================================+========================+
| ``Format ("{0}", +S)``         | ``"abc"``              |
+--------------------------------+------------------------+
| ``Format ("{0,*>10}", +S)``    | ``"*******abc"``       |
+--------------------------------+------------------------+
| ``Format ("{0,*<10}", +S)``    | ``"abc*******"``       |
+--------------------------------+------------------------+
| ``Format ("{0,*^10}", +S)``    | ``"****abc***"``       |
+--------------------------------+------------------------+

Other formatting information is ignored, e.g., the type specifiers.  The
accessor argument type for characters is ``String_Category_Type``.

.. _zb-text-datetime:

Date and Time Types
^^^^^^^^^^^^^^^^^^^

The implementation for the formatting of times is the more involved
and support two sub-categories to select either the time or date value
of an ``Ada.Calendar.Time`` value.  This is currently the only argument
type that does not use the standard format string, e.g., you cannot specify
a width, precision, etc.  The root locale formatting is available, however,
using a trailing ``*`` character in the format.

The built-in localization support includes localized formats for dates,
times and date/times.  These localization are implemented in terms of
ASCII date/time format strings, e.g., the occurence of the sequence
``dd`` within the format generated the day of the month in the
output to as two characters (0 padded).  The full set of format strings
is documented in table below.  (Note, some locale can have more than the
simple am, noon, and pm for the day period, see :ref:`zb-utilities-zbinfo`
utility.) Characters not part of a recognized format substring are simply
copied to the output as is.  Sub-strings that should be included as is can
be enclosed in single quotes (this is only needed if the sub-string would
otherwise be interpreted as date/time values.

+------------------+----------------------------------------------------+
| Time-Date Fromat | Description                                        |
+==================+====================================================+
| ``a``            | Day period name, e.g., ``am``, ``noon`` or ``pm``  |
+------------------+----------------------------------------------------+
| ``d``            | Day of the month, 1 .. 31                          |
+------------------+----------------------------------------------------+
| ``dd``           | Day of the month, 01 .. 31                         |
+------------------+----------------------------------------------------+
| ``EEEE``         | Full day of the week name                          |
+------------------+----------------------------------------------------+
| ``EEE``          | Abbreviated day of the week name                   |
+------------------+----------------------------------------------------+
| ``G``            | Era (CE/BCE, only CE is available with Ada)        |
+------------------+----------------------------------------------------+
| ``h``            | Hours, 0 .. 12                                     |
+------------------+----------------------------------------------------+
| ``HH``           | Hours, 00 .. 23                                    |
+------------------+----------------------------------------------------+
| ``H``            | Hours, 0 .. 23                                     |
+------------------+----------------------------------------------------+
| ``mm``           | Minutes, 00 .. 59                                  |
+------------------+----------------------------------------------------+
| ``m``            | Minutes, 0 .. 59                                   |
+------------------+----------------------------------------------------+
| ``MMMM``         | Full month name                                    |
+------------------+----------------------------------------------------+
| ``MMM``          | Abreviated month name                              |
+------------------+----------------------------------------------------+
| ``MM``           | Month number 00 .. 12                              |
+------------------+----------------------------------------------------+
| ``M``            | Month number 0 .. 12                               |
+------------------+----------------------------------------------------+
| ``ss``           | Seconds, 00 .. 59                                  |
+------------------+----------------------------------------------------+
| ``s``            | Seconds, 0 .. 59                                   |
+------------------+----------------------------------------------------+
| ``yyyy``         | Full year, 2012, four digits                       |
+------------------+----------------------------------------------------+
| ``yy``           | Year, e.g., 12                                     |
+------------------+----------------------------------------------------+
| ``y``            | Year, e.g., 2012, minimum number of digits         |
+------------------+----------------------------------------------------+
| ``zzzz``         | Timezone names (not available, GMT offset printed) |
+------------------+----------------------------------------------------+
| ``z``            | GMT timezone offset printed                        |
+------------------+----------------------------------------------------+

The date and time formatting is localized using the information from the
CLDR data and include localized day and month names along with localized
date and time formats.

The day in the week from a day is calculated using the code from [DayInWeek]_.

While a date/time format string can be included as part of the argument
description, it is more normal to use the "pre-package", locale aware,
format styles:

``short``
    The basic information, e.g., a time format would likely not include the
    seconds.  This is the default format.

``medium``
    Additional formatting on ``short``, e.g., a time format would likely
    include the seconds, abbreviated month names in dates, etc.

``long``
    the additional formatting on ``medium``, e.g., full month names, etc.

``full``
    the additional formatting on ``long``, e.g., full day names, etc.

Using direct templates rather than the format styles might not work in all
locales, i.e., generate mixed language results.  E.g., if a template references
the abbreviated month name, this will display as English in an Arabic locale
(abbreviated month names do not appear to be used in Arabic).

The accessor argument type for characters is ``Calendar_Category_Type``,
all three format type strings (``date``, ``time`` and ``datetime`` map to
this category type.

There is a lot of variety with date and times so the examples are more
extensive that other types.  In the following, the date/time being formatted
is 4:48 pm, Blooms Day, June 16, 1904, referred to via the variable
``B``.  The examples below use two locales for demonstration purposes:
``en_US`` and ``fr``.

Short date and times styles: default
""""""""""""""""""""""""""""""""""""

The simplest use is to not specify anything.  The generates an accessor
using an ``Any`` type category, in this case the ``datetime``
is used to format.  A type name is encouraged as the compiler will type
check message arguments.

+---------------------------------+--------------------------------+
| Format                          | Result                         |
+=================================+================================+
| ``Format ("{0}", +B)``          | - en_US: ``"4:48 PM 6/16/04"`` |
|                                 | - fr: ``"16:48 16/06/04"``     |
+---------------------------------+--------------------------------+
| ``Format ("{0,time}", +B)``     | - en_US ``"4:48 PM"``          |
|                                 | - fr: ``"16:48"``              |
+---------------------------------+--------------------------------+
| ``Format ("{0,date}", +B)``     | - en_US: ``"6/16/04"``         |
|                                 | - fr: ``"16/06/04"``           |
+---------------------------------+--------------------------------+
| ``Format ("{0,datetime}", +B)`` | - en_US: ``"4:48 PM 6/16/04"`` |
|                                 | - fr: ``"16:48 16/06/04"``     |
+--------------------------------++--------------------------------+

The formatting when no style is specified is ``short``, e.g.,
``{0,time}"`` is the same as ``"{0,time,short}"``.

Medium date and times styles
""""""""""""""""""""""""""""

The medium style add more formatted values, e.g., using the example
date.  Due to space constraints, the formatting specifier
is given rather than the full call to the ``Format`` function, e.g.,
``Format ("{0,time,long}", +B);`` is written as ``time,long``.

+---------------------+----------------------------------------+
| Format              | Result                                 |
+=====================+========================================+
| ``time,medium``     | - en_US: ``"4:48:03 PM"``              |
|                     | - fr: ``"16:48:03"``                   |
+---------------------+----------------------------------------+
| ``date,medium``     | - en_US: ``"Jun 16, 1904"``            |
|                     | - fr: ``"16 juin 1904"``               |
+---------------------+----------------------------------------+
| ``datetime,medium`` | - en_US: ``"4:48:03 PM Jun 16, 1904"`` |
|                     | - fr: ``"16:48:03 16 juin 1904"``      |
+---------------------+----------------------------------------+

Long date and times styles
""""""""""""""""""""""""""

The long style adds more formatted values on the medium style, e.g., again
using the example date (example executed in the Pacific time zone, 7 hours
earlier than GMT):

+---------------------+-----------------------------------------------+
| Format              | Result                                        |
+=====================+===============================================+
| ``time,long``       | - en_US: ``"4:48:03 PM -0700"``               |
|                     | - fr: ``"16:48:03 -0700"``                    |
+---------------------+-----------------------------------------------+
| ``date,long``       | - en_US: ``"June 16, 1904"``                  |
|                     | - fr: ``"16 juin 1904"``                      |
+---------------------+-----------------------------------------------+
| ``datetime,long``   | - en_US: ``"4:48:03 PM -0700 June 16, 1904"`` |
|                     | - fr: ``"16:48:03 -0700 16 juin 1904"``       |
+---------------------+-----------------------------------------------+

Full date and times styles
""""""""""""""""""""""""""

Finally, the full style uses full day and month names, e.g., using the example
date (example executed in the Pacific time zone, 7 hours earlier than GMT):

+-------------------+---------------------------------------------------------+
| Format            | Result                                                  |
+===================+=========================================================+
| ``time,full``     | - en_US: ``"4:48:03 PM -0700"``                         |
|                   | - fr: ``"16:48:03 -0700"``                              |
+-------------------+---------------------------------------------------------+
| ``date,full``     | - en_US: ``"Thursday, June 16, 1904"``                  |
|                   | - fr: ``"jeudi 16 juin 1904"``                          |
+-------------------+---------------------------------------------------------+
| ``datetime,full`` | - en_US: ``"4:48:03 PM -0700 Thursday, June 16, 1904"`` |
|                   | - fr: ``"16:48:03 -0700 jeudi 16 juin 1904"``           |
+-------------------+---------------------------------------------------------+
