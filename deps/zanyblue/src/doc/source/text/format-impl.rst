.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

ZanyBlue Formatting Implementation
----------------------------------

The primary formatting method is the ``Format`` set of functions
which format a message given a facility name, a key within that
facility and a set of arguments (either as an ``Argument_List``
or as individual "boxed" arguments).  The final two arguments for
both this set of ``Format`` functions or the ``Print`` and
``Print_Line`` procedures (explained later) is the locale
and the catalog.  All the functions and procedures defined in
this section are defined in the package ``ZanyBlue.Text.Formatting``
which is generally the only ZanyBlue package needed by applications.

The locale defaults to the current locale and generally need not be
specified.  A possible example of where a locale would need to be
specified would be a client/server application where the client sends
a locale name defining their preferred locale for messages.

The use of the catalog argument is even rarer and allows messages
to be defined/loaded into separate catalogs.  The ZanyBlue library
maintains a global common catalog which is used as the default for
all functions and procedures that take catalog arguments.

Format Functions
^^^^^^^^^^^^^^^^

The specification of the ``Format`` function is

.. code-block:: ada

   function Format (Facility  : Wide_String;
                    Key       : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog)
      return Wide_String;

along with the in-line boxed argument version:

.. code-block:: ada

   function Format (Facility  : Wide_String;
                    Key       : Wide_String;
                    Argument0 : Argument_Type'Class := Null_Argument;
                    Argument1 : Argument_Type'Class := Null_Argument;
                    Argument2 : Argument_Type'Class := Null_Argument;
                    Argument3 : Argument_Type'Class := Null_Argument;
                    Argument4 : Argument_Type'Class := Null_Argument;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog)
      return Wide_String;


Print Procedures
^^^^^^^^^^^^^^^^

Corresponding to the formatting functions, a set of ``Print`` and
``Print_Line`` procedures are available which print to the formatted
message to the standard output file or the given file argument.  These
procedures have versions that take both an argument list, i.e.,

.. code-block:: ada

   procedure Print (Facility  : Wide_String;
                    Key       : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog);

and the in-line "boxed" arguments, e.g.,

.. code-block:: ada

   procedure Print (Facility  : Wide_String;
                    Key       : Wide_String;
                    Argument0 : Argument_Type'Class := Null_Argument;
                    Argument1 : Argument_Type'Class := Null_Argument;
                    Argument2 : Argument_Type'Class := Null_Argument;
                    Argument3 : Argument_Type'Class := Null_Argument;
                    Argument4 : Argument_Type'Class := Null_Argument;
                    Locale    : Locale_Type := Current_Locale;
                    Catalog   : Catalog_Type := Standard_Catalog);

There are two options available when printing output to the ``Current_Output``:
use the ``Text_IO.Current_Output`` or ``Wide_Text_IO.Current_Output``.  The
default uses the ``Wide_Text_IO`` version which defers any encoding to the
Ada run-time (e.g., the GNAT ``-gnatW8`` command line option, etc).  To use
the ``Text_IO`` version, the ``Formatting`` routing ``Disable_Wide_IO``
should be used.  This allows the ZanyBlue library to honour the encoding
specified via the environment and is recommended, e.g.,

.. code-block:: ada

   Disable_Wide_IO;
   Print_Line ("xmpl", "0001", +My_Var);

The routine ``Enable_Wide_IO`` is available to re-enable the use of the
``Wide_Text_IO`` destination.

The signature for the ``Print_Line`` versions are similar.  Both the
``Print`` and ``Print_Line`` procedure sets have corresponding
versions that take a first argument giving the destination file, e.g.,

.. code-block:: ada

   procedure Print (Destination : Ada.Wide_Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Arguments   : Argument_List;
                    Locale      : Locale_Type := Current_Locale;
                    Catalog     : Catalog_Type := Standard_Catalog);

Plain Formatting Versions
^^^^^^^^^^^^^^^^^^^^^^^^^

Both the ``Format`` functions and ``Print`` procedure have versions
that take a message format instead and arguments (either as an argument
list or as in-line "boxed" arguments), e.g.,

.. code-block:: ada

   function Format (Text      : Wide_String;
                    Arguments : Argument_List;
                    Locale    : Locale_Type := Current_Locale)
      return Wide_String;

The locale argument is still required in this context as arguments are
still formatted within the context of a locale.

Usage of these functions and procedures do not externalize the message
text and, as such, do little to help internationalize applications.

Localized Exceptions
^^^^^^^^^^^^^^^^^^^^

Ada allows exceptions to be raised with a message string, e.g.,

.. code-block:: ada

   raise My_Exception with "Something is wrong here";

.. todo:: Is localized exception encoded using UTF-8 or selected locale?

The ZanyBlue library includes ``Raise_Exception`` procedures with
signatures paralleling the ``Format`` methods.  The procedures
raise the identified exception with a localized formatted messages.
Since the Ada standard defines exception message to be a ``String``,
the formatted ``Wide_String`` is converted to a ``String`` by
UTF-8 encoding the ``Wide_String``.  The specification of the
argument list version of this procedure is

.. code-block:: ada

   procedure Raise_Exception (E         : Ada.Exceptions.Exception_Id;
                              Facility  : Wide_String;
                              Key       : Wide_String;
                              Arguments : Argument_List;
                              Locale    : Locale_Type := Current_Locale;
                              Catalog   : Catalog_Type := Standard_Catalog);

The conversion of ``Wide_String`` to an UTF-8 encoded ``String`` uses
the GNAT specific Unicode functions.

Missing Arguments and Exceptions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Format strings refer to arguments by index, e.g.::

    moons=There are {0} moons orbiting "{1}".

expects two "boxed" arguments.  If supplied with less than expected, e.g.,

.. code-block:: ada

    Print_Line ("myapp", "moons", +10);

where the planet name is not supplied, is, by default, considered an error
and the exception ``No_Such_Argument_Error`` is raised.  This behavior
can be adjusted by calling the catalogs routine ``Disable_Exceptions``.
When exceptions are disabled, missing arguments are replaced in the formatted
string with the format information enclosed in vertical bars rather than
braces.

The ``Disable_Exceptions`` has an inverse routine ``Enable_Exceptions``
which re-enables exceptions.  This is either on the default standard catalog
or a user supplied argument catalog.  The status of exceptions for a catalog
can be queried using the function ``Exceptions_Enabled``.
