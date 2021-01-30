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

--  @usage delete [ -r ] name
--  @summary delete a file in the test area
--  @start-doc
--  Delete a file from the test area.  With the "-r" option, a directory
--  tree is deleted.
--

with Ada.Strings.Wide_Fixed;
with ZanyBlue.Wide_Directories;

separate (ZBTest.Commands)
procedure Delete_Command
  (State : in out State_Type;
   Args  :        List_Type)
is

   use Ada.Strings.Wide_Fixed;
   use ZanyBlue.Wide_Directories;

   procedure Delete_Directory
     (State : in out State_Type;
      Name  :        String);
   --  Delete a directory (recursively).

   procedure Delete_File
     (State : in out State_Type;
      Name  :        String);
   --  Delete a file.

   ----------------------
   -- Delete_Directory --
   ----------------------

   procedure Delete_Directory
     (State : in out State_Type;
      Name  :        String)
   is
      pragma Unreferenced (State);
   begin
      Wide_Delete_Tree (Name);
      Print_00020 (+Name);
   end Delete_Directory;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File
     (State : in out State_Type;
      Name  :        String)
   is
      pragma Unreferenced (State);
   begin
      Wide_Delete_File (Name);
      Print_00017 (+Name);
   end Delete_File;

   Target_Index : Natural := 0;
   Recursive    : Boolean := False;

begin
   for I in 2 .. Length (Args) loop
      if Value (Args, I) = "-r" then
         Recursive := True;
      elsif Head (Value (Args, I), 1) = "-" then
         raise Command_Usage_Error;
      elsif Target_Index = 0 then
         Target_Index := I;
      else
         raise Command_Usage_Error;
      end if;
   end loop;
   if Target_Index = 0 then
      raise Command_Usage_Error;
   end if;
   if Recursive then
      Delete_Directory (State, Value (Args, Target_Index));
   else
      Delete_File (State, Value (Args, Target_Index));
   end if;
exception
   when E : ZanyBlue.Wide_Directories.Name_Error =>
      Print_10026 (+Value (Args, Target_Index), +E);
end Delete_Command;
