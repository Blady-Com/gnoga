.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Stub Implementations
--------------------

Some part of the ZanyBlue text library includes stub implementations.  This
section documents them.

Generic_Fixed
^^^^^^^^^^^^^
   
The ``Generic_Fixed`` package implemented formatting for the fixed
floating point type.  This is a stub implementation in this version of
the ZanyBlue library and simply dispatches to the underlying
Ada ``Wide_Image`` routine to format the value.

Generic ZanyBlue text packages should be instanciated at the library
level to prevent run time accessibility exceptions.
