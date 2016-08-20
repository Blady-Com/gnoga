.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

.. _zb-text:

The Text Package
================

The intent of the ``Text`` package is to allow the separate
definition of the text messages displayed by an application into a
``.properties`` file.  Messages are reference in the source code
by a key value (a simple string or accessor function returning the text
associated with a particular key) within a group of related messages -- a
facility.  The message strings include Java style message argument
place holders: argument number (zero-based) along with optional type specific
formatting information enclosed in chain brackets.  Allowing formatting
of message arguments in the context of a localized message.

The divorce of the message text from the source code allows the easy
localization of application messages.  The source language ``.properties``
file is delivered to translation vendors who then return localized
``.properties`` files.  E.g., for an application ``myapp``, with
application messages defined in the file::

    myapp.properties

the localized French and Japanese files would be::

    myapp_fr.properties
    myapp_ja.properties

The ``Text`` package supports message text searching among a set of
localized ``.properties`` files with fallback to available messages.
For example, if the above application, ``myapp``, is run in a Canadian
French environment (``ZB_LANG=fr_CA`` on Unix), localized display of each
message is implemented by trying to locate the message in the files, in order::

    myapp_fr_CA.properties
    myapp_fr.properties
    myapp.properties

If no French localized message is available, i.e., the properties file which
does not include a language code is used, the base properties file.  Frequently
this is English (the final file shown above).
   
Contents:

.. toctree::
   :maxdepth: 2

   messages
   lowlevel
   accessor-xmpl
   msg-format
   format-syntax
   argtypes
   filtering
   stubs
   format-impl
   locale-type
   encodings
   builtin-cldr
   cldr-data
   pseudo
   metrics
   zbmcompile
