--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Format_Errors; use ZanyBlue.Text.Format_Errors;

function ZanyBlue.Text.Format_Message
  (Message        : String;
   Arguments      : ZanyBlue.Text.Arguments.Argument_List;
   Mapping        : ZanyBlue.Text.Pseudo.Pseudo_Map_Access;
   Locale         : ZanyBlue.Text.Locales.Locale_Type;
   Raise_Errors   : Boolean                         := True;
   Mark_Messages  : Boolean                         := True;
   Mark_Arguments : Boolean                         := True;
   Error_Handler  : access Error_Handler_Type'Class :=
     Standard_Error_Handler'Access)
   return String;
--  Function to format a message text string containing references to
--  arguments, e.g., "{0}", "{1,10}", with the argument values based on
--  the argument list, Arguments.  The Mapping argument, if not null,
--  defines a mapping of source characters to a pseudo translation of
--  the character (as a simple character).  The Locale argument is not
--  use directly by the Format_Message function but is passed on to
--  the formatting functions for individual argument references.  When
--  a reference to an argument the does not exist in the Argument list
--  the exception No_Such_Argument_Error is raised unless Raise_Errors
--  is False.
