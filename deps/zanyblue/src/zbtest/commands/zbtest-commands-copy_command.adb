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

with Ada.Strings.Wide_Fixed;
with ZanyBlue.Wide_Directories;

separate (ZBTest.Commands)
procedure Copy_Command (State : in out State_Type;
                        Args  : List_Type) is

   use Ada.Strings.Wide_Fixed;
   use ZanyBlue.Wide_Directories;

   procedure Copy_Directory (State            : in out State_Type;
                             Source_Name      : Wide_String;
                             Destination_Name : Wide_String);
   --  Recursively copy a directory.

   procedure Copy_File (State            : in out State_Type;
                        Source_Name      : Wide_String;
                        Destination_Name : Wide_String);
   --  Copy a simple file.

   --------------------
   -- Copy_Directory --
   --------------------

   procedure Copy_Directory (State            : in out State_Type;
                             Source_Name      : Wide_String;
                             Destination_Name : Wide_String) is
      Source_Path : constant Wide_String :=
                       State.Locate_Directory (Source_Name);
   begin
      Wide_Copy_Tree (Source_Path, Destination_Name);
      Print_00019 (+Source_Name, +Destination_Name);
      State.Add_Undo_Action (Format ("delete -r {0}", +Destination_Name));
   end Copy_Directory;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File (State            : in out State_Type;
                        Source_Name      : Wide_String;
                        Destination_Name : Wide_String) is
      Source_Path : constant Wide_String := State.Locate_File (Source_Name);
   begin
      Wide_Copy_File (Source_Path, Destination_Name);
      Print_00011 (+Source_Name, +Destination_Name);
      State.Add_Undo_Action (Format ("delete {0}", +Destination_Name));
   end Copy_File;

   Source_Index      : Natural := 0;
   Destination_Index : Natural := 0;
   Recursive         : Boolean := False;

begin
   for I in 2 .. Length (Args) loop
      if Value (Args, I) = "-r" then
         Recursive := True;
      elsif Head (Value (Args, I), 1) = "-" then
         raise Command_Usage_Error;
      elsif Source_Index = 0 then
         Source_Index := I;
      elsif Destination_Index = 0 then
         Destination_Index := I;
      else
         raise Command_Usage_Error;
      end if;
   end loop;
   if Source_Index = 0 then
      raise Command_Usage_Error;
   end if;
   if Destination_Index = 0 then
      Destination_Index := Source_Index;
   end if;
   if Recursive then
      Copy_Directory (State, Value (Args, Source_Index),
                             Value (Args, Destination_Index));
   else
      Copy_File (State, Value (Args, Source_Index),
                        Value (Args, Destination_Index));
   end if;
exception
when File_Not_Found =>
   Print_10018 (+Value (Args, Source_Index));
end Copy_Command;
