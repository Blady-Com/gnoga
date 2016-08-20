.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Formatting Syntax
-----------------
   
Additional formatting information can be included in the argument reference
via an optional format string after the index value, separated by either a
comma or a colon.  The format syntax is based on the syntax defined for Python
format strings in most cases (see the Argument Types section for details
on types that use additional, non-Python style formatting, e.g, dates and
time).
   
.. _zb-text-format-type:

Specifying Argument Types
^^^^^^^^^^^^^^^^^^^^^^^^^
   
The format string is optionally prefixed by a type name separated by a comma
character for the expected argument without space character (which would be
interpreted as part for the format).  E.g., to indicate a particular argument
is should be an integer, the format would be::

   0002=There are {0,integer} known moons orbiting "{1}".

The type names recognized are given in the following table:

+---------------+------------------------------------------------------------+
| Type Name     | Description                                                |
+===============+============================================================+
| ``any``       | No specific type required                                  |
+---------------+------------------------------------------------------------+
| ``boolean``   | Boolean values required                                    |
+---------------+------------------------------------------------------------+
| ``character`` | Character (wide or narrow) values required                 |
+---------------+------------------------------------------------------------+
| ``date``      | A Calendar type required (formatted as a date)             |
+---------------+------------------------------------------------------------+
| ``datetime``  | A Calendar type required (formatted as a date and time)    |
+---------------+------------------------------------------------------------+
| ``duration``  | Duration type required                                     |
+---------------+------------------------------------------------------------+
| ``enum``      | Enumeration type required                                  |
+---------------+------------------------------------------------------------+
| ``exception`` | Exception type required (e.g., ``when E : others``)        |
+---------------+------------------------------------------------------------+
| ``fixed``     | A fixed point value is required                            |
+---------------+------------------------------------------------------------+
| ``float``     | A floating point value is required                         |
+---------------+------------------------------------------------------------+
| ``integer``   | An integer value is required                               |
+---------------+------------------------------------------------------------+
| ``modular``   | A modular type value is required                           |
+---------------+------------------------------------------------------------+
| ``number``    | A numeric value (integer, real, etc) is required           |
+---------------+------------------------------------------------------------+
| ``real``      | A real value (float or fixed) is required                  |
+---------------+------------------------------------------------------------+
| ``string``    | A string (wide or narrow) is required (characters also ok) |
+---------------+------------------------------------------------------------+
| ``time``      | A Calendar type required (formatted as a time)             |
+---------------+------------------------------------------------------------+

The type information, apart from the date and time related names, do not
impact runtime formatting (the boxed value is formatted according to the
boxed type formatting routine).  The type information does, however, impact
the signature of accessor routine generated.

For example, the message,::

   0002=There are {0,integer} known moons orbiting "{1}".

would generate a format style access with the signature

.. code-block:: ada

   function Format_0002 (
      Argument0   : Integer_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog) return Wide_String;

Here, arugment 0 is required to be an integer type (verified by the compiler)
while argument 1 is unconstrainted with respect to type.

Use of the accessor allows the compiler to verify arguments have the expected
type.

Type names have the expected hierarchy, e.g., a numeric argument type allows
integer, float, fixed, etc, arguments.

General Formatting Syntax
^^^^^^^^^^^^^^^^^^^^^^^^^
   
The formatting information for the various types supported, in general,
follow the Python/C style embedded formatting format for all but the date
and time related formatting (the formatting of these values is described
later in section :ref:`zb-text-datetime`).  E.g., to format an integer with a
field width of 10 characters, the argument reference would be::

   0001=Total number is {0,integer,10}

The syntax for the format information is (using standard BNF notation)::

    [[fill]align][sign][#][0][width][.precision][type][qual]

where

**fill**
    The fill character to use when padding, e.g., if the formatted value
    string is less than the field width additional padding characters
    are added fill out the field.  Where the characters are added is
    defined by the next character, ``align``.  The fill character
    cannot be the ``}`` character.

**align**
    The align character defines the alignment of the formatted value
    within the minimum field width.  The alignment characters recognized are
    
    ``<``
          Value is flushed left, any additional padding characters needed
          are added to the right of the value.

    ``>``
          Value is flushed right, any additional padding characters needed
          are prepend to the left of the value.

    ``=``
          Any padding characters needed are added after the sign character,
          e.g., ``+00010``.  This is used only with numeric value.  For
          non-numeric values, this alignment character is interpreted as
          ``<``.

    ``^``
          The value is centered in the field width with any padding character
          being added before and after the value to center it.

    Note that unless a minimum field width is defined, the field width
    will always be the same size as the data to fill it, so the
    alignment option has no meaning in this case.

**sign**
    The sign character defines how the sign of numeric values are handled.
    This applies only to the sign of the value, the exponent of floating
    point numbers formatted in scientific notation is always preceeded by
    a sign character.  It has three valid values:

    ``+``
          A sign character is always generated.  Positive values are preceeded
          by the locale plus character, negative values by the
          locale minus character.

    ``-``
          A sign character is only generated for negative values where the
          locale minus character is used.  This is the default behaviour if
          no sign character is specified.

    `` `` (a space character)
          A sign character is only generated for negative values where the
          locale minus character is used, positive values are preceeded by a
          space character.

    Note, the sign format character is ignored for non-numeric arguments.

**#**
    The hash character causes integer formatted values formatted as binary,
    octal or hexadecimal to be decorated with the base using standard Ada
    notation.  E.g., formatting the integer 2012 using base 16::

       x  => 7dc
       #x => 16#7dc#

**0**
    If the width field is preceded by a zero (``0``) character, this
    enables zero-padding. This is equivalent to an alignment type of
    `'=`` and a fill character of ``0``.

**width**
    The width is a Latin integer value defining the minimum field width for
    the argument.  Padding, using the fill character, is added if needed to
    meet this minimum width.

**precision**
    The precision is a Latin integer value indicating how many digits
    should be displayed after the decimal point for a floating point
    value.  The precision is not used for non-floating type formatting.

**type**
    The formatting style character gives the expected base to use when
    formatting integer arguments and style when formatting floating point
    arguments.  Style indicators are ignored if the argument is
    not numeric.  The integer formatting style characters supported are

    ``b``
        Binary format. Outputs the number in base 2.

    ``d``
        Decimal Integer. Outputs the number in base 10.

    ``o``
        Octal format. Outputs the number in base 8.

    ``x``
        Hex format. Outputs the number in base 16, using lowercase letters for
        the digits above 9.

    ``X``
        Hex format. Outputs the number in base 16, using uppercase letters for
        the digits above 9.

    **None**
        The same as ``d``.

    The floating point formatting style characters supported are:

    ``E``
        Exponent notation. Prints the number in scientific notation using the
        localized equivalent of the letter ``E`` to indicate the
        exponent.

    ``e``
        Exponent notation. Same as ``E``.  Other formatting systems,
        e.g., C, would use case difference in the format string to change the
        case of the exponent character in the formatted value.  Since localzied
        versions are being used, it is not clear if lowercasing/uppercasing
        such strings is valid.  The two format characters are treated the
        same.

    ``F``
        Fixed point. Displays the number as a fixed-point number.

    ``f``
        Fixed point. Same as ``F``.

    ``G``
        General format. This prints the number as a fixed-point
        number, unless the number is too large, in which case
        it switches to ``E`` exponent notation.

    ``g``
        General format. Same as ``G``.

    **None**
        The same as ``E``.

    Infinity and NaN values are formatted as using localized versions.

**qual**
    The format string can be terminated with a final qualifier character.  For
    the current version, the only valid value for this character is ``*``
    which forces the formatting of the value using the Root locale, i.e.,
    standard Ada Latin formatting.  This impact the formatting of date and time
    values and the formatting of numbers where a localized version might use
    localized digits instead of the Latin ``0123456789``, e.g.,
    Arabic locales.
