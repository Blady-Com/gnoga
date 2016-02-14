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

with Ada.Strings.Wide_Fixed;

separate (ZBTest.Commands)
procedure Which_Command (State : in out State_Type;
                         Args  : List_Type) is

   use Ada.Strings.Wide_Fixed;

   procedure Display_Path (State      : State_Type;
                           Name       : Wide_String;
                           Executable : Boolean);
   --  Search for a path name and display the result.

   ------------------
   -- Display_Path --
   ------------------

   procedure Display_Path (State      : State_Type;
                           Name       : Wide_String;
                           Executable : Boolean) is
   begin
      if Executable then
         Print_00033 (+State.Locate_Executable (Name));
      else
         Print_00033 (+State.Locate_File (Name));
      end if;
   exception
   when File_Not_Found =>
      Print_10010 (+Name);
   end Display_Path;

   Executable : Boolean := False;

begin
   for I in 2 .. Length (Args) loop
      if Value (Args, I) = "-e" then
         Executable := True;
      elsif Value (Args, I) = "-f" then
         Executable := False;
      elsif Head (Value (Args, I), 1) = "-" then
         raise Command_Usage_Error;
      else
         Display_Path (State, Value (Args, I), Executable);
      end if;
   end loop;
end Which_Command;
