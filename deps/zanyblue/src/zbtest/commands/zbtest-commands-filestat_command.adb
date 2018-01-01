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

--  @usage filestat name log-file
--  @summary write status of a test area file to a log file
--  @start-doc
--  Write status of a test area file to a log file.  This is frequently
--  used to verify the existence/non-existence of a file or directory in the
--  test area.  The information written to the file stat log file, for ordinary
--  files is:
--
--  * The file name
--  * The file type ("ORDINARY_FILE")
--  * The file size in bytes
--  * The time stamp associated with the file
--
--  For directories and other non-ordinary files only the file name and type is
--  writeen to the log file.
--
--  If the target file for the filestat command does not exist in the test area
--  the generated log file contains message::
--
--      The file "NAME" does not exist
--
--  Examples::
--
--      ZBTest> filestat a
--      ZBTest> filestat expected.txt expected01.log
--      Generated status report on the file "expected.txt" (non-existent)
--          to "expected01.log"
--      ...
--      ZBTest> filestat expected.txt expected02.log
--      Generated status report on the file "expected.txt" to "expected02.log"
--

with Ada.Directories;
with Ada.Strings.Wide_Fixed;
with ZanyBlue.Wide_Directories;
with ZBTest.Commands.File_Size_Arguments;
with ZBTest.Commands.File_Type_Arguments;

separate (ZBTest.Commands)
procedure Filestat_Command (State : in out State_Type;
                            Args  : List_Type) is

   use Ada.Strings.Wide_Fixed;
   use ZanyBlue.Wide_Directories;
   use ZBTest.Commands.File_Size_Arguments;
   use ZBTest.Commands.File_Type_Arguments;

   procedure File_Stat (State     : in out State_Type;
                        File_Name : Wide_String;
                        Log_Name  : Wide_String);
   --  Generate the status report on a file.

   ---------------
   -- File_Stat --
   ---------------

   procedure File_Stat (State     : in out State_Type;
                        File_Name : Wide_String;
                        Log_Name  : Wide_String) is
      pragma Unreferenced (State);
      use type Ada.Directories.File_Kind;
      Log_File : File_Type;
   begin
      Wide_Create (Log_File, Log_Name);
      if Wide_Exists (File_Name) then
         Print_00025 (+File_Name, Log_File);
         Print_00026 (+Wide_Kind (File_Name), Log_File);
         if Wide_Kind (File_Name) = Ada.Directories.Ordinary_File then
            Print_00027 (+Wide_Size (File_Name), Log_File);
            Print_00028 (+Wide_Modification_Time (File_Name), Log_File);
         end if;
         Print_00022 (+File_Name, +Log_Name);
      else
         Print_00024 (+File_Name, Log_File);
         Print_00023 (+File_Name, +Log_Name);
      end if;
      Close (Log_File);
   end File_Stat;

   Source_Index : Natural := 0;
   Log_Index    : Natural := 0;

begin
   for I in 2 .. Length (Args) loop
      if Head (Value (Args, I), 1) = "-" then
         raise Command_Usage_Error;
      elsif Source_Index = 0 then
         Source_Index := I;
      elsif Log_Index = 0 then
         Log_Index := I;
      else
         raise Command_Usage_Error;
      end if;
   end loop;
   if Source_Index = 0 or else Log_Index = 0 then
      raise Command_Usage_Error;
   end if;
   File_Stat (State, Value (Args, Source_Index), Value (Args, Log_Index));
end Filestat_Command;
