--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
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

--
--  This is a simple wrapper package around the standard Ada.Command_Line
--  package with Wide_String arguments and functions.  The underlying
--  Strings from Ada.Command_Line are simply interpreted as UTF-8 encoded
--  strings and are decoded to Wide_Strings.  This, obviously, does not
--  support UTF-8 encoded Wide_Wide_Strings.
--

with Ada.Command_Line;
with ZanyBlue.OS;

package body ZanyBlue.Wide_Command_Line is

   use ZanyBlue.OS;
   use Ada.Command_Line;

   -------------------
   -- Wide_Argument --
   -------------------

   function Wide_Argument (Number : Positive) return Wide_String is
   begin
      return From_UTF8 (Argument (Number));
   end Wide_Argument;

   -------------------------
   -- Wide_Argument_Count --
   -------------------------

   function Wide_Argument_Count return Natural is
   begin
      return Argument_Count;
   end Wide_Argument_Count;

   -----------------------
   -- Wide_Command_Name --
   -----------------------

   function Wide_Command_Name return Wide_String is
   begin
      return From_UTF8 (Command_Name);
   end Wide_Command_Name;

end ZanyBlue.Wide_Command_Line;
