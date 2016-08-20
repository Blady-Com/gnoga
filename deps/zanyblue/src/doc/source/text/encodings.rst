.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.


.. _zb-text-encoding:

Encodings and The Codecs Type
-----------------------------

The codecs type captures the information related to encoding and decoding
internal wide Unicode strings to externally useable character sequences,
i.e., narrow strings.  The current implementation initializes the locale
based on the encoding defined by the ``ZB_LANG`` or ``LANG`` environment varaible
encoding definition.  This is normally appended to the locale name separated
by a period.  Typical examples of ``LANG`` values are:

+---------------------+-----------------------------------------------------------+
| LANG                | Description                                               |
+=====================+===========================================================+
| ``en_US.UTF-8``     | English, as used in the United States with UTF-8 encoding |
+---------------------+-----------------------------------------------------------+
| ``fr_FR.ISO8859-1`` | French, as used in France with ISO8859-1 encoding,        |
|                     | Latin-1 encoding for Western European languages           |
+---------------------+-----------------------------------------------------------+

Internally, the encodings are implementated as either direct, algorithmic,
implementation, e.g., the ``UTF-8`` encoding is implemented using
the standard Ada runtime Unicode support packages, or as table driven
lookups on the encoding translation data published on [UnicodeMappings]_.

The encodings support included, by default, in the ZanyBlue library are

+----------------+-----------------------------------------------------------+
| Encoding       | Description                                               |
+================+===========================================================+
| ``ASCII``      | 7-bit ASCII characters only                               |
+----------------+-----------------------------------------------------------+
| ``BIG5``       | Alias for ``CP950``                                       |
+----------------+-----------------------------------------------------------+
| ``CP874``      | ANSI/OEM Thai (ISO 8859-11); Thai (Windows)               |
+----------------+-----------------------------------------------------------+
| ``CP932``      | ANSI/OEM Japanese; Japanese (Shift-JIS)                   |
+----------------+-----------------------------------------------------------+
| ``CP936``      | ANSI/OEM Simplified Chinese (PRC, Singapore)              |
+----------------+-----------------------------------------------------------+
| ``CP949``      | ANSI/OEM Korean (Unified Hangul Code)                     |
+----------------+-----------------------------------------------------------+
| ``CP950``      | ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC) |
+----------------+-----------------------------------------------------------+
| ``CP1250``     | ANSI Central European; Central European (Windows)         |
+----------------+-----------------------------------------------------------+
| ``CP1251``     | ANSI Cyrillic; Cyrillic (Windows)                         |
+----------------+-----------------------------------------------------------+
| ``CP1252``     | ANSI Latin 1; Western European (Windows)                  |
+----------------+-----------------------------------------------------------+
| ``CP1253``     | ANSI Greek; Greek (Windows)                               |
+----------------+-----------------------------------------------------------+
| ``CP1254``     | ANSI Turkish; Turkish (Windows)                           |
+----------------+-----------------------------------------------------------+
| ``CP1255``     | ANSI Hebrew; Hebrew (Windows)                             |
+----------------+-----------------------------------------------------------+
| ``CP1256``     | ANSI Arabic; Arabic (Windows)                             |
+----------------+-----------------------------------------------------------+
| ``CP1257``     | ANSI Baltic; Baltic (Windows)                             |
+----------------+-----------------------------------------------------------+
| ``CP1258``     | ANSI/OEM Vietnamese; Vietnamese (Windows)                 |
+----------------+-----------------------------------------------------------+
| ``CP28591``    | Alias for ``ISO8859-1``                                   |
+----------------+-----------------------------------------------------------+
| ``CP28592``    | Alias for ``ISO8859-2``                                   |
+----------------+-----------------------------------------------------------+
| ``CP28593``    | Alias for ``ISO8859-3``                                   |
+----------------+-----------------------------------------------------------+
| ``CP28594``    | Alias for ``ISO8859-4``                                   |
+----------------+-----------------------------------------------------------+
| ``CP28595``    | Alias for ``ISO8859-5``                                   |
+----------------+-----------------------------------------------------------+
| ``CP28596``    | Alias for ``ISO8859-6``                                   |
+----------------+-----------------------------------------------------------+
| ``CP28597``    | Alias for ``ISO8859-7``                                   |
+----------------+-----------------------------------------------------------+
| ``CP28598``    | Alias for ``ISO8859-8``                                   |
+----------------+-----------------------------------------------------------+
| ``CP28599``    | Alias for ``ISO8859-9``                                   |
+----------------+-----------------------------------------------------------+
| ``CP28603``    | Alias for ``ISO8859-13``                                  |
+----------------+-----------------------------------------------------------+
| ``CP28605``    | Alias for ``ISO8859-15``                                  |
+----------------+-----------------------------------------------------------+
| ``CP65001``    | Alias for ``UTF-8``                                       |
+----------------+-----------------------------------------------------------+
| ``GB2312``     | Alias for ``CP936``                                       |
+----------------+-----------------------------------------------------------+
| ``ISO8859-1``  | ISO 8859-1 Western European                               |
+----------------+-----------------------------------------------------------+
| ``ISO8859-2``  | ISO 8859-2 Central European                               |
+----------------+-----------------------------------------------------------+
| ``ISO8859-3``  | ISO 8859-3 South European                                 |
+----------------+-----------------------------------------------------------+
| ``ISO8859-4``  | ISO 8859-4 Baltic                                         |
+----------------+-----------------------------------------------------------+
| ``ISO8859-5``  | ISO 8859-5 Cyrillic                                       |
+----------------+-----------------------------------------------------------+
| ``ISO8859-6``  | ISO 8859-6 Arabic                                         |
+----------------+-----------------------------------------------------------+
| ``ISO8859-7``  | ISO 8859-7 Greek                                          |
+----------------+-----------------------------------------------------------+
| ``ISO8859-8``  | ISO 8859-8 Hebrew                                         |
+----------------+-----------------------------------------------------------+
| ``ISO8859-9``  | ISO 8859-9 Turkish                                        |
+----------------+-----------------------------------------------------------+
| ``ISO8859-10`` | ISO 8859-10 Nordic languages                              |
+----------------+-----------------------------------------------------------+
| ``ISO8859-11`` | ISO 8859-11 Thai                                          |
+----------------+-----------------------------------------------------------+
| ``ISO8859-13`` | ISO 8859-13 Baltic Rim                                    |
+----------------+-----------------------------------------------------------+
| ``ISO8859-14`` | ISO 8859-14 Celtic                                        |
+----------------+-----------------------------------------------------------+
| ``ISO8859-15`` | ISO 8859-15 Western European (with Euro sign)             |
+----------------+-----------------------------------------------------------+
| ``ISO8859-16`` | ISO 8859-16 South-Eastern European                        |
+----------------+-----------------------------------------------------------+
| ``SHIFT_JIS``  | Alias for ``CP932``                                       |
+----------------+-----------------------------------------------------------+
| ``UTF-8``      | Unicode 8-bit encoding                                    |
+----------------+-----------------------------------------------------------+

.. todo:: Document Codecs Type

The ``Codecs_Type`` defines two functions to perform encoding and
decoding on strings:

.. code-block:: ada

    function Encode (Codecs : Codecs_Type;
                     Value : Wide_String) return String;
    function Decode (Codecs : Codecs_Type;
                     Value : String) return Wide_String;

It is not generally necessary to the encoding functions as the formatting
routines that return ``String``'s encode using the Codecs associated
with the locale.  The decoding routine is not currently used by the library
and is available if applications need to convert encoded input to
``Wide_String``'s.
