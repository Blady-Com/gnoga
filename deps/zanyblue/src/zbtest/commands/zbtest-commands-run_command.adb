--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2018, Michael Rohan <mrohan@zanyblue.com>
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

--  @usage run script
--  @summary run another ZBTest script
--  @start-doc
--  Run a script given the script name, i.e., without the "zbt" extension.
--  The script should either be in the current directory or in a sub-directory
--  with the same name as the script, e.g., "run xmpl", will try the files::
--
--      xmpl.zbt
--      xmpl/xmpl.zbt
--
--  Once located, the ZBTest commands in the script are executed within the
--  context of a new implicit scope.  Modifications to the environment made
--  by the script are discarded (via undo actions) when the script completes.
--

with ZanyBlue.Wide_Directories;

separate (ZBTest.Commands)
procedure Run_Command
  (State : in out State_Type;
   Args  :        List_Type)
is

   use ZanyBlue.Wide_Directories;

   procedure Execute_Script
     (State  : in out State_Type;
      Script :        String);
   --  Execute the script: open the script file and execute each line.

   procedure Load_Init_Scripts
     (State      : in out State_Type;
      Script_Dir :        String);
   --  Load any zbtest initialization scripts found in the directory
   --  tree, if not already loaded.

   function Locate_Script
     (Path : String;
      Name : String)
      return String;
   --  Locate the script to execute.

   procedure Register_Run_Failure
     (State       : in out State_Type;
      Script_Name :        String);
   --  Register the failure to run a test script as a test failure.

   procedure Run
     (State : in out State_Type;
      Name  :        String);
   --  Run the script.

   procedure Wrap_Up
     (State     : in out State_Type;
      Test_Name :        String);
   --  Wrap-up the test, close any open scopes and print summary

   --------------------
   -- Execute_Script --
   --------------------

   procedure Execute_Script
     (State  : in out State_Type;
      Script :        String)
   is
      File : File_Type;
   begin
      State.Set_String ("_source", Script);
      State.Set_Integer ("_lineno", 0);
      State.Set_String ("_testname", Wide_Base_Name (Script));
      State.Set_String ("_fulltestname", Full_Test_Name (State));
      Wide_Open (File, In_File, Script);
      State.Read_Eval_Loop (File, False);
      Close (File);
   exception
      when E : others =>
         if Is_Open (File) then
            Close (File);
         end if;
         Print_10040 (+E, +Script);
   end Execute_Script;

   -----------------------
   -- Load_Init_Scripts --
   -----------------------

   procedure Load_Init_Scripts
     (State      : in out State_Type;
      Script_Dir :        String)
   is
      Init_Script : constant String :=
        Wide_Compose (Script_Dir, ZBTest_Init_Name, ZBTest_Extension);
      Init_Param : constant String := "_init::" & Init_Script;
   begin
      begin
         --  Load the scripts in our parent directories first.
         Load_Init_Scripts (State, Wide_Containing_Directory (Script_Dir));
      exception
         when ZanyBlue.Wide_Directories.Use_Error =>
         --  At the root directory, just continue
            null;
         when ZanyBlue.Wide_Directories.Name_Error =>
         --  Something strange about he name, just return.
            null;
      end;
      if State.Is_Defined (Init_Param) then
         --  Initialization script already loaded.  They are only loaded
         --  once per scope.
         return;
      end if;
      State.Set_Boolean (Init_Param, True);
      if not Wide_Exists (Init_Script) then
         --  Initialization script does not exist at this directory level.
         return;
      end if;
      Print_00047 (+Init_Script);
      Execute_Script (State, Init_Script);
   end Load_Init_Scripts;

   -------------------
   -- Locate_Script --
   -------------------

   function Locate_Script
     (Path : String;
      Name : String)
      return String
   is

      Test_1 : constant String :=
        Wide_Compose ("", Name, ZBTest_Extension);
      Test_2 : constant String :=
        Wide_Compose (Name, Name, ZBTest_Extension);
      Test_3 : constant String :=
        Wide_Compose (Path, Name, ZBTest_Extension);
      Test_4 : constant String :=
        Wide_Compose (Wide_Compose (Path, Name), Name, ZBTest_Extension);
   begin
      if Wide_Is_File (Name) then
         return Name;
      elsif Wide_Is_File (Test_1) then
         return Test_1;
      elsif Wide_Is_File (Test_2) then
         return Test_2;
      elsif Wide_Is_File (Test_3) then
         return Test_3;
      elsif Wide_Is_File (Test_4) then
         return Test_4;
      else
         raise File_Not_Found;
      end if;
   end Locate_Script;

   --------------------------
   -- Register_Run_Failure --
   --------------------------

   procedure Register_Run_Failure
     (State       : in out State_Type;
      Script_Name :        String)
   is
      Test_Name : constant String :=
        Format ("{0}.{1}-run", +State.Full_Test_Name, +Script_Name);
      File : File_Type;
   begin
      Wide_Create (File, Test_Name);
      Print_10037 (+Script_Name, File);
      Close (File);
      State.Register_Failure (Test_Name);
   end Register_Run_Failure;

   ----------
   -- Run --
   ----------

   procedure Run
     (State : in out State_Type;
      Name  :        String)
   is
      Cur_Path   : constant String := State.Get_String ("_curpath");
      Script     : constant String := Locate_Script (Cur_Path, Name);
      Script_Dir : constant String := Wide_Containing_Directory (Script);
   begin
      Print_00013 (+Script);
      State.New_Scope;
      State.Initialize_Scope (Script_Dir, Implicit_Scope => True);
      Load_Init_Scripts (State, Script_Dir);
      Execute_Script (State, Script);
      Wrap_Up (State, Full_Test_Name (State));
   end Run;

   -------------
   -- Wrap_Up --
   -------------

   procedure Wrap_Up
     (State     : in out State_Type;
      Test_Name :        String)
   is
      N_Fail : Natural;
      N_OK   : Natural;
   begin
      --  Close any user created scopes ...
      while not State.Is_Defined ("_implicit_scope", False) loop
         State.End_Scope;
      end loop;
      State.End_Scope (N_OK, N_Fail);
      Print_00018 (+Test_Name, +N_Fail, +N_OK);
   end Wrap_Up;

begin
   if Length (Args) = 2 then
      Run (State, Value (Args, 2));
   else
      raise Command_Usage_Error;
   end if;
exception
   when File_Not_Found =>
      Register_Run_Failure (State, Value (Args, 2));
      Print_10020 (+Value (Args, 2));
end Run_Command;
