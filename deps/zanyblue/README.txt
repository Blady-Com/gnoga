The ZanyBlue.Text Package
-------------------------

ZanyBlue.Text is an Ada 2012 package supporting localization support in Ada
by allowing the externalization of application messages into a properties
file which can be localized into additional languages, similar to Java.
The localized files include the locale string in the file name, e.g., the
file "zbmcompile.properties" is localized to "zbmcompile_fr.properties" in
French.

The locale string is 

* A language abbreviation (2 or 3 alpha-numeric characters), e.g., "en" for
  English, "zh" for Chinese, "cop" for Coptic, etc.
* A script abbreviation (4 alpha-numeric characters), e.g., "Latn" for Latin,
  "Hant" for Traditional Han, "Hebr" for Hebrew, etc.
* A territory abbreviation (2 or 3 alpha-numeric characters), e.g., "US" for
  US, "CN" for China, "142" for Asia, etc.

All components are optional, giving locale strings "" for the base locale,
"en" for English, "zh_Hant" for Traditional Chinese, "zh_TW" for Chinese
in Taiwan (also Traditional Chinese), and "zh_Hant_TW".

The externalized .properties files are compiled to an Ada 2005 package
containing the application strings (including localized strings) to be
compiled into an application.  The selection of locale by ZanyBlue.Text
is based on a locale argument which defaults to the locale string value
of the environment variable "ZB_LANG", or, if it's not defined, "LANG".
This allows applications to automatically switch the display language
(provided the application was localized into the selected locale).
E.g., the example application "moons" defaults to displaying English
but can be changed to using German, French or Spanish, e.g.,  to display
is German:

    $ export ZB_LANG=de
    $ moons

The externalization of messages into a .properties files allows the embedding
of parameters references via index (also similar to Java), e.g., the message
to print the value of a number, the message could be defined as

    00001=The value of the counter is {0} ticks.

The ZanyBlue.Text package supports the "passing" of argument values by
"boxing" the values into ZanyBlue.Text objects.  This particular message
could be printed, with an argument of 1904 as,

    Print_Line ("myapp", "00001", +1904);

The unitary "+" operator is overloaded by the ZanyBlue.Text package to create
the "boxed" object.  See the ZanyBlue documentation for details on the types
supported by the package.

If accessors are used, the generated Ada package will contain a routine
for each key defined, e.g.,

    Print_00001 (+1904);

Argument numbers and, if type information is included in the messages,
argument types can be checked by the compiler.

Website
-------

For full documentation and up-to-date information, visit the web site:

   http://zanyblue.sourceforge.net

Downloading
-----------

The download area contains the bundle:

1) "zanyblue-VERSION-REVISION.tar.gz", the core ZanyBlue source bundle.

Building
--------

The ZanyBlue packages have been build primarily on Unix systems using
GNAT GPL 2016 (or on Windows using GNAT GPL 2016).  To build the library
and message compiler (zbmcompile) only the compiler is needed.

The build is GNU make based.  To build on Unix, assuming your PATH is
setup correctly:

   $ zanyblue-VERSION
   $ cd src
   $ make

To run the regression tests, Ahven is needed and is expected to be be installed
on the build system, use the "check" build target in the src directory:

    $ make check

All the examples, except the "gtk" example, do not require additional packages
beyond the ZanyBlue library.  The "gtk" example requires the GtkAda package
from AdaCore.

Contact
-------

For additional information contact Michael Rohan <mrohan@zanyblue.com>
