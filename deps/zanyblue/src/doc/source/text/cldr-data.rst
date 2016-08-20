.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Unicode CLDR Data
-----------------

The Unicode.org CLDR data used to define the locale specific information
such as the date and time formats also includes localized names for languages,
scripts and territories.  This localized information is included in the
ZanyBlue Text library via the ``ZanyBlue.Text.CLDR`` package and can
be used to translate abbreviations, e.g, ``en`` to localized
named, e.g., the function call,

.. code-block:: ada

   Language_Name ("en")

returns ``English`` in an English locale and ``anglais`` in a French locale.
There localized names for scripts and territories are available via the functions
``Script_Name`` and ``Territory_Name`` functions.

All functions take an optional ``Unknown`` parameter giving the result
returned for unknown names (defaulting to the empty string) and a final
locale parameter.

