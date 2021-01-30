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

--  @usage joinpaths path ...
--  @summary return a list of path concatenated into a single path
--  @start-doc
--  The "joinpaths" function takes one or more path components and combines
--  them into a single path, e.g.,::
--
--      ZBTest> set libdir $(joinpaths $project_dir lib)
--

with Ada.Strings.Wide_Unbounded;
with ZanyBlue.Wide_Directories;

separate (ZBTest.Functions)
function Joinpaths_Function
  (State : access State_Type;
   Args  : List_Type)
   return String
is
   pragma Unreferenced (State);

   use Ada.Strings.Wide_Unbounded;
   use ZanyBlue.Wide_Directories;

   Buffer : Unbounded_Wide_String;

begin
   if Length (Args) < 2 then
      raise Function_Usage_Error;
   end if;
   Append (Buffer, Value (Args, 2));
   for I in 3 .. Length (Args) loop
      Set_Unbounded_Wide_String
        (Buffer, Wide_Compose (To_Wide_String (Buffer), Value (Args, I)));
   end loop;
   return To_Wide_String (Buffer);
end Joinpaths_Function;
