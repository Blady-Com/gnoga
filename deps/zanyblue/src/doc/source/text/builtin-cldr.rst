.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Built-in Localizations
----------------------

Localization for dates, times, language names, script names and territory
names are compiled into the ZanyBlue Text library based on the Unicode.org
Common Locale Date Repository (CLDR).  For details on date and time formatting
see the section on ``Ada.Calendar.Time`` argument types later.

The localized name associated with standard language, script and territory
abbreviations are available via the various routines defined in the package
``ZanyBlue.Text.CLDR``.

The library is currently implemented to use the CLDR data to determine the
zero character when printing numbers (integers).  This is normally the
standard ASCII "0", however, some languages, e.g., Arabic, have there own
numeric characters.  In Arabic locale, ``ZanyBlue.Text`` will generate
numeric (integer) output using Arabic numerals.  Whether this is a feature
or an error is unclear at this time.

For this release, the built-in locales are

+-------------+-------------------------+
| Code        | Language                |
+=============+=========================+
| ``ar``      | Arabic                  |
+-------------+-------------------------+
| ``cs``      | Czech                   |
+-------------+-------------------------+
| ``da``      | Danish                  |
+-------------+-------------------------+
| ``de``      | German                  |
+-------------+-------------------------+
| ``el``      | Greek                   |
+-------------+-------------------------+
| ``en``      | English                 |
+-------------+-------------------------+
| ``en_AU``   | English (Australia)     |
+-------------+-------------------------+
| ``en_CA``   | English (Canada)        |
+-------------+-------------------------+
| ``en_IE``   | English (Ireland)       |
+-------------+-------------------------+
| ``en_GB``   | English (Great Britian) |
+-------------+-------------------------+
| ``en_NZ``   | English (New Zealand)   |
+-------------+-------------------------+
| ``en_ZA``   | English (South Africa)  |
+-------------+-------------------------+
| ``es``      | Spanish                 |
+-------------+-------------------------+
| ``fi``      | Finnish                 |
+-------------+-------------------------+
| ``fr``      | French                  |
+-------------+-------------------------+
| ``ga``      | Irish                   |
+-------------+-------------------------+
| ``he``      | Hebrew                  |
+-------------+-------------------------+
| ``hu``      | Hungarian               |
+-------------+-------------------------+
| ``it``      | Italian                 |
+-------------+-------------------------+
| ``ja``      | Japanese                |
+-------------+-------------------------+
| ``ko``      | Korean                  |
+-------------+-------------------------+
| ``nb``      | Norwegian Bokmäl        |
+-------------+-------------------------+
| ``nl``      | Dutch                   |
+-------------+-------------------------+
| ``pl``      | Polish                  |
+-------------+-------------------------+
| ``pt``      | Portuguese              |
+-------------+-------------------------+
| ``ro``      | Romanian                |
+-------------+-------------------------+
| ``ru``      | Russian                 |
+-------------+-------------------------+
| ``sk``      | Slovak                  |
+-------------+-------------------------+
| ``sv``      | Swedish                 |
+-------------+-------------------------+
| ``th``      | Thai                    |
+-------------+-------------------------+
| ``tr``      | Turkish                 |
+-------------+-------------------------+
| ``zh``      | Chinese (Simplified)    |
+-------------+-------------------------+
| ``zh_Hant`` | Chinese (Traditional)   |
+-------------+-------------------------+

Running the example applications, in particular the ``time`` example,
will display localized day, month, etc. names and localized formats.

