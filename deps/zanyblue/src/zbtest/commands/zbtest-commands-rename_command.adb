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

with ZanyBlue.Wide_Directories;

separate (ZBTest.Commands)
procedure Rename_Command (State : in out State_Type;
                          Args  : List_Type) is

   pragma Unreferenced (State);

   use ZanyBlue.Wide_Directories;

   procedure Rename_File (Old_Name : Wide_String;
                          New_Name : Wide_String);
   --  Rename the file.

   -----------------
   -- Rename_File --
   -----------------

   procedure Rename_File (Old_Name : Wide_String;
                          New_Name : Wide_String) is
   begin
      Wide_Rename (Old_Name, New_Name);
      Print_00012 (+Old_Name, +New_Name);
   exception
   when E : Ada.Wide_Text_IO.Name_Error | Ada.Wide_Text_IO.Use_Error =>
      Print_10034 (+Old_Name, +New_Name, +E);
   end Rename_File;

begin
   if Length (Args) = 3 then
      Rename_File (Value (Args, 2), Value (Args, 3));
   else
      raise Command_Usage_Error;
   end if;
exception
when E : ZanyBlue.Wide_Directories.Use_Error =>
   Print_10019 (+Value (Args, 2), +Value (Args, 3), +E);
end Rename_Command;
