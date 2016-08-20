.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Message Filtering
-----------------
   
Frequently an application has different output modes, e.g., debug, verbose,
normal, quiet, etc.  If the various ``Print`` routines are used to
generate messages for the application, these messages can be filtered using
the tagged type ``Message_Filter_Type`` which is used by the ZanyBlue
library to determine if a message should be really be printed.

The ``Message_Filter_Type`` defines the method

.. code-block:: ada

   function Is_Filtered (Filter   : Message_Filter_Type;
                         Facility : Wide_String;
                         Key : Wide_String) return Boolean;

An application convention can be used on the message keys to define, e.g.,
verbose message filtering.  The example application
``examples/text/filtering`` uses the convention:

* Verbose messages begin with the letter ``V``

* Informational messages begin with the letter ``I``

* Warning messages begin with the letter ``W``

* Error messages begin with the letter ``E``

The declaration of a filtering type for this configuration would be

.. code-block:: ada

   type My_Filter_Type is new Message_Filter_Type with
      record
         Verbose : Boolean := False;
      end record;

   function Is_Filtered (Filter   : My_Filter_Type;
                         Facility : Wide_String;
                         Key : Wide_String) return Boolean;

with the simple implementation for this example being

.. code-block:: ada

   function Is_Filtered (Filter   : My_Filter_Type;
                         Facility : Wide_String;
                         Key : Wide_String) return Boolean is
   begin
      return Key (Key'First) = 'V' and not Filter.Verbose;
   end Is_Filtered;

To enable the filtering, the filter must be registered with the ZanyBlue
library via the ``Set_Filter`` routine.  This routine takes an access to
``Message_Filter_Type'Class`` object.  E.g., for the example filtering
application, the filter is installed using:

.. code-block:: ada

   use ZanyBlue.Text.Formatting;
   ...
   App_Filter : aliased My_Filter_Type;
   ...
   Set_Filter (App_Filter'Access);

There is a cost associated with filtered messages:

* The various ZanyBlue routines are called.

* Any arguments will be "boxed" and appended to an argument list.

* The filtering code is called for all messages.

This cost, however, does not included the cost associated with formatting
the message as the filtering occurs before any message formatting happens.
Normally, it is this formatting cost that is the highest for general
messages.
