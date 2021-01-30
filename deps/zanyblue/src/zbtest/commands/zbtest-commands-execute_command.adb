--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2017, Michael Rohan <mrohan@zanyblue.com>
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

--  @usage execute [ -f | -s | -o output ] command [ command-arg ... ]
--  @summary execute a system command
--  @start-doc
--  Execute a command given the command name and an optional list
--  of command arguments.  The command to execute is located by
--
--  #. Searching from the current test directory up the directory tree
--     for an executable with the command name in a bin directory, i.e.,
--     if executing a command built in the current source tree.
--
--  #. Or, searching the value of the "path" list parameter (initialized to
--     the value of the "PATH" environment variable).
--
--  The search attempts to find the first file with an extension defined by
--  the "exes" list parameter that matches the command name.  For example,
--  on Windows, the directories listed on the "path" parameter are searched
--  for files with extensions "bat", "cmd", "com" or "exe".
--
--  Once an executable is found, a new process is spawned to execute it.
--  If the "-o" option is given, then the command output (standard output
--  and standard error) is sent to the named file.
--
--  The "-s" option defines a command that is expected to execute successfully.
--  This is the default and the "-s" option is normally not given.
--
--  The "-f" option defines a command that is expected fail (non-zero exit
--  status).
--
--  An execute failure file is generated if a command does not exit with the
--  expected status.
--
--  Examples
--
--  #. Execute the "ls" command, the output is sent to the "screen"::
--
--      ZBTest> execute ls
--
--  #. Execute the "ls" command, the output is sent to the the file "ls.out"
--     in the test area::
--
--      ZBTest> execute -o ls.out ls
--
--  #. Execute the "ls" command, the output is sent to the the file "ls.out"
--     in the test area.  The command is expected to exit with a failure
--     status in this case, i.e., the file "nosuchfile" is not expected to
--     exist::
--
--      ZBTest> execute -f -o ls.out ls nosuchfile
--
--  The "execute" command is normally used with the "-o" option generating
--  a log file for comparision with a reference log file via the ZBTest
--  function "nextlog".
--

with GNAT.OS_Lib;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;

separate (ZBTest.Commands)
procedure Execute_Command
  (State : in out State_Type;
   Args  :        List_Type)
is

   use Ada.Strings.Wide_Fixed;

   procedure Execute_Command
     (State          : in out State_Type;
      Command        :        String;
      Expect_Failure :        Boolean;
      Args           :        List_Type;
      Args_Index     :        Positive;
      N_Args         :        Natural;
      Output_Name    :        String);
   --  Execute the command, given the full path to the command.

   procedure Register_Execute_Failure
     (State          : in out State_Type;
      Command_Line   :        String;
      Expect_Failure :        Boolean;
      Success        :        Boolean);
   --  Register a command failure.

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (State          : in out State_Type;
      Command        :        String;
      Expect_Failure :        Boolean;
      Args           :        List_Type;
      Args_Index     :        Positive;
      N_Args         :        Natural;
      Output_Name    :        String)
   is
      use Ada.Strings.Wide_Unbounded;
      Command_Line : Unbounded_Wide_String;
      Arguments    : GNAT.OS_Lib.Argument_List (1 .. N_Args);
      Success      : Boolean;
      Return_Code  : Integer;
   begin
      Print_00029 (+Command);
      for I in Args_Index .. Args_Index + N_Args - 1 loop
         Arguments (I - Args_Index + 1) :=
           new String'(Wide_To_UTF8 (Value (Args, I)));
         Append (Command_Line, Value (Args, I));
         Append (Command_Line, " ");
      end loop;
      if Output_Name'Length > 0 then
         GNAT.OS_Lib.Spawn
           (Wide_To_UTF8 (Command), Arguments, Wide_To_UTF8 (Output_Name),
            Success, Return_Code);
      else
         Return_Code := GNAT.OS_Lib.Spawn (Wide_To_UTF8 (Command), Arguments);
      end if;
      if not (Expect_Failure xor Return_Code = 0) then
         Register_Execute_Failure
           (State, To_Wide_String (Command_Line), Expect_Failure, Success);
      end if;
      for I in Arguments'Range loop
         GNAT.OS_Lib.Free (Arguments (I));
      end loop;
   end Execute_Command;

   ------------------------------
   -- Register_Execute_Failure --
   ------------------------------

   procedure Register_Execute_Failure
     (State          : in out State_Type;
      Command_Line   :        String;
      Expect_Failure :        Boolean;
      Success        :        Boolean)
   is
      Test_Name : constant String :=
        Format
          ("{0}-exec{1}", +State.Full_Test_Name,
           +State.Get_Integer ("_execfail"));
      File : File_Type;
   begin
      Wide_Create (File, Test_Name);
      Print_00045 (File);
      Print_00046 (+Command_Line, File);
      if Expect_Failure then
         Print_00034 (File);
      else
         Print_00035 (File);
      end if;
      if Success then
         Print_00036 (File);
      else
         Print_00037 (File);
      end if;
      Close (File);
      State.Increment ("_execfail", Deep => False);
      State.Register_Failure (Test_Name);
   end Register_Execute_Failure;

   Expect_Failure : Boolean  := False;
   Command_Index  : Natural  := 0;
   Output_Index   : Natural  := 0;
   Index          : Positive := 2;

begin
   while Index <= Length (Args) and then Command_Index = 0 loop
      if Value (Args, Index) = "-f" then
         Expect_Failure := True;
      elsif Value (Args, Index) = "-s" then
         Expect_Failure := False;
      elsif Value (Args, Index) = "-o" then
         if Index <= Length (Args) then
            Index        := Index + 1;
            Output_Index := Index;
         else
            raise Command_Usage_Error;
         end if;
      elsif Head (Value (Args, Index), 1) = "-" then
         raise Command_Usage_Error;
      else
         Command_Index := Index;
      end if;
      Index := Index + 1;
   end loop;
   if Command_Index = 0 then
      raise Command_Usage_Error;
   end if;
   if Output_Index /= 0 then
      Execute_Command
        (State, State.Locate_Executable (Value (Args, Command_Index)),
         Expect_Failure, Args, Command_Index + 1,
         Length (Args) - Command_Index, Value (Args, Output_Index));
   else
      Execute_Command
        (State, State.Locate_Executable (Value (Args, Command_Index)),
         Expect_Failure, Args, Command_Index + 1,
         Length (Args) - Command_Index, "");
   end if;
exception
   when File_Not_Found =>
      Print_10036 (+Value (Args, Command_Index));
end Execute_Command;
