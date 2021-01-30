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

with System;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Wide_Text_IO;
with Ada.Characters.Handling;
with ZBTest.Input_Parser;
with ZBTest.Commands;
with ZBTest.Functions;
with ZanyBlue.OS;
with ZanyBlue.Wide_Directories;
with ZanyBlue.Text.Formatting;
with ZBTest_Messages.ZBTest_Prints;
with ZBTest_Messages.ZBTest_Wide_Strings;

pragma Unreferenced (ZBTest.Functions);

package body ZBTest.States is

   use Ada.Characters.Handling;
   use ZBTest.Commands;
   use ZBTest.Functions;
   use ZBTest.Input_Parser;
   use ZanyBlue.OS;
   use ZanyBlue.Text;
   use ZanyBlue.Wide_Directories;
   use ZanyBlue.Text.Formatting;
   use ZBTest_Messages.ZBTest_Prints;

   type Search_Type is (Search_File, Search_Executable, Search_Directory);

   procedure Add_Tree_To_Path
     (State : in out State_Type;
      Here  :        String);
   --  Add the directory tree rooted in the test area directory to the
   --  executeable search path.

   procedure Expand_Functions
     (State  : in out State_Type;
      Buffer : in out Unbounded_Wide_String);
   --  Expand references to function, e.g., $(which -e zbmcompile)

   procedure Expand_References
     (State  :        State_Type;
      Buffer : in out Unbounded_Wide_String);
   --  Exopand reference to parameters, e.g., $_testname

   function Read_Test_Failure
     (File_Name : String)
      return String;
   --  Read and return the text contained in a fail file

   procedure Remove_Test_Area (Test_Area : String);
   --  Remove pre-existing test area prior to running a test.

   procedure Rename_Test_Marker
     (Test_Name   : String;
      Status_Name : String);
   --  Rename the marker file for a test to either the test ok or
   --  fail status name.  This could fail if the file already exists.

   function Search
     (State       : State_Type;
      Name        : String;
      Paths_Name  : String;
      Exts_Name   : String;
      Target_Type : Search_Type)
      return String;
   --  Search for files or directories on the search path parameter.  The
   --  file located can have any of the exts extensions.

   procedure Set_Last_Test (State : in out State_Type);
   --  Set the time stamp associate with the end of the current test, the
   --  "_last_test" parameter.

   procedure Write_XML (State : in out State_Type);
   --  Write the summary XML for an individual test (current scope).

   ----------------------
   -- Add_Tree_To_Path --
   ----------------------

   procedure Add_Tree_To_Path
     (State : in out State_Type;
      Here  :        String)
   is

      Bin_Path : constant String := Wide_Compose (Here, "bin");

   begin
      if Wide_Is_Directory (Bin_Path) then
         State.Append ("path", Bin_Path);
      end if;
      State.Append ("searchpath", Here);
      Add_Tree_To_Path (State, Wide_Containing_Directory (Here));
   exception
      when Ada.Text_IO.Use_Error =>
         null;
   end Add_Tree_To_Path;

   ---------------------
   -- Add_Undo_Action --
   ---------------------

   procedure Add_Undo_Action
     (State  : in out State_Type;
      Action :        String)
   is
   begin
      State.Prepend ("_undo", Action);
   end Add_Undo_Action;

   ------------
   -- Append --
   ------------

   procedure Append
     (State : in out State_Type;
      Name  :        String;
      Value :        String)
   is
   begin
      State.Parameters.Append (Name, Value);
   end Append;

   -------------------------------
   -- Define_Initial_Parameters --
   -------------------------------

   procedure Define_Initial_Parameters (State : in out State_Type) is
      Here : constant String := Wide_Current_Directory;
   begin
      State.New_Scope;
      State.Initialize_Scope (Here);
      State.Set_Boolean ("_terminate", False);
      State.Set_String ("prompt", "ZBTest> ");
      State.Set_String ("_testname", "zbtest");
      State.Set_String ("_testarea", "test-area");
      State.Set_String ("_curlog", "zbtest.log");
      State.Set_String ("_fulltestname", "zbtest");
      State.Set_Boolean ("_implicit_scope", True);
      case OS_Name is
         when Unix =>
            State.Set_String ("_pathsep", ":");
            State.Set_String ("_platform", "unix");
            State.Append ("exes", "");
            State.Append ("exes", "sh");
         when Windows =>
            State.Set_String ("_pathsep", ";");
            State.Set_String ("_platform", "windows");
            State.Append ("exes", "bat");
            State.Append ("exes", "cmd");
            State.Append ("exes", "com");
            State.Append ("exes", "exe");
      end case;
      State.Set_Integer ("_word_size", System.Word_Size);
      Add_Tree_To_Path (State, Here);
      State.Set_Boolean ("_xml_p", False);
      State.Execute_Line ("getenv -l PATH path");
   end Define_Initial_Parameters;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (State      : State_Type;
      File_Name  : String;
      All_Scopes : Boolean)
   is
      File : File_Type;
   begin
      Create (File, Mode => Out_File, Name => Wide_To_UTF8 (File_Name));
      State.Dump (File, All_Scopes);
      Close (File);
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (State       : State_Type;
      Destination : File_Type;
      All_Scopes  : Boolean)
   is
   begin
      State.Parameters.Dump (Destination, All_Scopes);
   end Dump;

   ---------------
   -- End_Scope --
   ---------------

   procedure End_Scope (State : in out State_Type) is
      N_OK   : Natural;
      N_Fail : Natural;
   begin
      End_Scope (State, N_OK, N_Fail);
   end End_Scope;

   ---------------
   -- End_Scope --
   ---------------

   procedure End_Scope
     (State  : in out State_Type;
      N_OK   :    out Natural;
      N_Fail :    out Natural)
   is
      Undo_Actions : constant List_Type :=
        State.Get_List ("_undo", Deep => False);
   begin
      for I in 1 .. Length (Undo_Actions) loop
         Print_00032 (+Value (Undo_Actions, I));
         State.Execute_Line (Value (Undo_Actions, I));
      end loop;
      N_OK   := State.Get_Integer ("_n_ok_cumulative");
      N_Fail := State.Get_Integer ("_n_fail_cumulative");
      if State.Get_Boolean ("_xml_p") then
         Write_XML (State);
      end if;
      State.Parameters.End_Scope;
   end End_Scope;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (State : in out State_Type;
      Args  :        List_Type)
   is
      Implementation : Command_Type;
   begin
      if Length (Args) > 0 then
         Implementation := Find (Value (Args, 1));
         Implementation (State, Args);
      end if;
   exception
      when Command_Usage_Error =>
         Print_Command_Usage (Value (Args, 1));
      when Unknown_Command_Error =>
         Print_10009 (+Value (Args, 1));
   end Execute_Command;

   ------------------
   -- Execute_Line --
   ------------------

   procedure Execute_Line
     (State      : in out State_Type;
      Input_Line :        String)
   is
      Buffer : Unbounded_Wide_String;
   begin
      Append (Buffer, Input_Line);
      Expand (State, Buffer);
      if To_Wide_String (Buffer) /= Input_Line then
         Print_00044
           (+Full_Test_Name (State), +State.Get_Integer ("_lineno"), +Buffer);
      end if;
      State.Execute_Command (Parse_Words (To_Wide_String (Buffer)));
   exception
      when Unterminated_String =>
         Print_00043;
   end Execute_Line;

   ------------------
   -- Execute_Line --
   ------------------

   procedure Execute_Line
     (State       : in out State_Type;
      Input_Line  :        String;
      Interactive :        Boolean)
   is
   begin
      if not Interactive then
         Print_00014
           (+Full_Test_Name (State), +State.Get_Integer ("_lineno"),
            +Input_Line);
      end if;
      Execute_Line (State, Input_Line);
   end Execute_Line;

   ------------
   -- Expand --
   ------------

   procedure Expand
     (State  : in out State_Type;
      Buffer : in out Unbounded_Wide_String)
   is
   begin
      Expand_References (State, Buffer);
      Expand_Functions (State, Buffer);
   end Expand;

   ----------------------
   -- Expand_Functions --
   ----------------------

   procedure Expand_Functions
     (State  : in out State_Type;
      Buffer : in out Unbounded_Wide_String)
   is

      function Result
        (Function_Call : String)
         return String;

      ------------
      -- Result --
      ------------

      function Result
        (Function_Call : String)
         return String
      is
         Args : constant List_Type := Parse_Words (Function_Call);
         Impl : Function_Type;
      begin
         if Length (Args) = 0 then
            Print_10031;
            return "";
         end if;
         Impl := Find (Value (Args, 1));
         return Impl (State'Access, Args);
      exception
         when Unknown_Function_Error =>
            Print_10032 (+Value (Args, 1));
            return "";
         when Function_Usage_Error =>
            Print_Function_Usage (Value (Args, 1));
            return "";
      end Result;

      Start          : Positive := 1;
      Next           : Natural;
      Function_Start : Natural;
      Function_End   : Natural;
   begin
      loop
         Function_Start := Index (Buffer, "$(", Start);
         exit when Function_Start = 0;
         Function_End := Index (Buffer, ")", Function_Start);
         Next         := Index (Buffer, "$(", Function_Start + 2);
         if Next > 0 and then Next < Function_End then
            Start := Function_Start + 2;
         else
            Replace_Slice
              (Buffer, Function_Start, Function_End,
               Result (Slice (Buffer, Function_Start + 2, Function_End - 1)));
            Start := 1;
         end if;
      end loop;
   end Expand_Functions;

   -----------------------
   -- Expand_References --
   -----------------------

   procedure Expand_References
     (State  :        State_Type;
      Buffer : in out Unbounded_Wide_String)
   is

      procedure Replace_Reference
        (Buffer : in out Unbounded_Wide_String;
         Start  :        Positive;
         Last   :        Positive);

      procedure Replace_Reference
        (Buffer : in out Unbounded_Wide_String;
         Start  :        Positive;
         Last   :        Positive)
      is
         Parameter : constant String := Slice (Buffer, Start + 1, Last);
      begin
         if State.Is_Defined (Parameter) then
            Replace_Slice (Buffer, Start, Last, State.Get_String (Parameter));
         else
            Replace_Slice (Buffer, Start, Last, "");
            Print_10030 (+Parameter);
         end if;
      end Replace_Reference;

      Last      : Positive;
      Start     : Positive := 1;
      Next_Ch   : Character;
      Ch        : Character;
      In_String : Boolean  := False;
      I         : Positive := 1;

   begin
      while I < Length (Buffer) loop
         Start   := I;
         Ch      := To_Character (Element (Buffer, I));
         I       := I + 1;
         Next_Ch := To_Character (Element (Buffer, I));
         if not In_String then
            In_String := Ch = ''';
            if Ch = '$'
              and then
              (Next_Ch = '_'
               or else (Is_Alphanumeric (Next_Ch) and not Is_Digit (Next_Ch)))
            then
               Last := I;
               while Next_Ch = '_' or else Is_Alphanumeric (Next_Ch) loop
                  Last := Last + 1;
                  if Last <= Length (Buffer) then
                     Next_Ch := To_Character (Element (Buffer, Last));
                  else
                     Next_Ch := '$';
                  end if;
               end loop;
               Last := Last - 1;
               Replace_Reference (Buffer, Start, Last);
            end if;
         else
            In_String := Ch /= ''';
         end if;
      end loop;
   end Expand_References;

   --------------------
   -- Full_Test_Name --
   --------------------

   function Full_Test_Name
     (State : State_Type)
      return String
   is
      Test_Names : constant List_Type := State.Get_List ("_testname");
      Buffer     : Unbounded_Wide_String;
   begin
      for I in reverse 1 .. Length (Test_Names) loop
         Append (Buffer, Value (Test_Names, I));
         if I /= 1 then
            Append (Buffer, ".");
         end if;
      end loop;
      return To_Wide_String (Buffer);
   end Full_Test_Name;

   ---------
   -- Get --
   ---------

   function Get
     (State : State_Type;
      Name  : String)
      return Value_Type'Class
   is
   begin
      return State.Parameters.Get (Name);
   end Get;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (State : State_Type;
      Name  : String)
      return Boolean
   is
   begin
      return State.Parameters.Get_Boolean (Name);
   end Get_Boolean;

   -------------------
   -- Get_Character --
   -------------------

   function Get_Character
     (State : State_Type;
      Name  : String)
      return Unicode_Character
   is
      Value : constant String := State.Get_String (Name);
   begin
      if Value'Length > 0 then
         return Value (Value'First);
      else
         return ' ';
      end if;
   end Get_Character;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float
     (State : State_Type;
      Name  : String)
      return Float
   is
   begin
      return State.Parameters.Get_Float (Name);
   end Get_Float;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer
     (State : State_Type;
      Name  : String)
      return Integer
   is
   begin
      return State.Parameters.Get_Integer (Name);
   end Get_Integer;

   --------------
   -- Get_List --
   --------------

   function Get_List
     (State : State_Type;
      Name  : String;
      Deep  : Boolean := True)
      return List_Type
   is
   begin
      return State.Parameters.Get_List (Name, Deep);
   end Get_List;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (State : State_Type;
      Name  : String)
      return String
   is
   begin
      return State.Parameters.Get_String (Name);
   end Get_String;

   --------------
   -- Get_Time --
   --------------

   function Get_Time
     (State : State_Type;
      Name  : String)
      return Ada.Calendar.Time
   is
   begin
      return State.Parameters.Get_Time (Name);
   end Get_Time;

   ---------------
   -- Increment --
   ---------------

   procedure Increment
     (State     : in out State_Type;
      Name      :        String;
      By_Amount :        Integer := 1;
      Deep      :        Boolean := True)
   is
   begin
      State.Parameters.Increment (Name, By_Amount, Deep);
   end Increment;

   ----------------------
   -- Initialize_Scope --
   ----------------------

   procedure Initialize_Scope
     (State          : in out State_Type;
      Script_Dir     :        String;
      Implicit_Scope :        Boolean := False)
   is
   begin
      State.Set_Integer ("_n_fail_cumulative", 0);
      State.Set_Integer ("_n_ok_cumulative", 0);
      State.Set_Integer ("_n_fail", 0);
      State.Set_Integer ("_n_ok", 0);
      State.Set_Integer ("_n_tests", 0);
      State.Set_Integer ("_n_tests", 0);
      State.Set_Integer ("_lognum", 0);
      State.Set_Integer ("_lineno", 0);
      State.Set_Integer ("_execfail", 1);
      State.Set_String ("_curpath", Script_Dir);
      State.Set_String ("searchpath", Script_Dir);
      State.Set_Time ("_last_test", State.Get_Time ("_start_time"));
      State.Set_Boolean ("_implicit_scope", Implicit_Scope);
   end Initialize_Scope;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined
     (State     : State_Type;
      Name      : String;
      Any_Scope : Boolean := True)
      return Boolean
   is
   begin
      return State.Parameters.Is_Defined (Name, Any_Scope);
   end Is_Defined;

   ----------------------
   -- Locate_Directory --
   ----------------------

   function Locate_Directory
     (State : State_Type;
      Name  : String)
      return String
   is
   begin
      return Search (State, Name, "searchpath", "", Search_Directory);
   end Locate_Directory;

   -----------------------
   -- Locate_Executable --
   -----------------------

   function Locate_Executable
     (State : State_Type;
      Name  : String)
      return String
   is
   begin
      return Search (State, Name, "path", "exes", Search_Executable);
   end Locate_Executable;

   -----------------
   -- Locate_File --
   -----------------

   function Locate_File
     (State : State_Type;
      Name  : String)
      return String
   is
   begin
      return Search (State, Name, "searchpath", "", Search_File);
   end Locate_File;

   ---------------
   -- New_Scope --
   ---------------

   procedure New_Scope (State : in out State_Type) is
   begin
      State.Parameters.New_Scope;
      State.Set_Time ("_start_time", Ada.Calendar.Clock);
   end New_Scope;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (State : in out State_Type;
      Name  :        String;
      Value :        String)
   is
   begin
      State.Parameters.Prepend (Name, Value);
   end Prepend;

   --------------------
   -- Read_Eval_Loop --
   --------------------

   procedure Read_Eval_Loop
     (State       : in out State_Type;
      Input       :        File_Type;
      Interactive :        Boolean)
   is
   begin
      while not State.Get_Boolean ("_terminate") loop
         if Interactive then
            Print_00033 (+State.Get_String ("prompt"), With_NL => False);
         end if;
         State.Increment ("_lineno", Deep => False);
         State.Execute_Line (Wide_From_UTF8 (Get_Line (Input)), Interactive);
      end loop;
   exception
      when End_Error =>
         return;
   end Read_Eval_Loop;

   -----------------------
   -- Read_Test_Failure --
   -----------------------

   function Read_Test_Failure
     (File_Name : String)
      return String
   is

      use Ada.Directories;

      File_Name_S : constant String  := Wide_To_UTF8 (File_Name);
      File_Size   : constant Natural := Natural (Size (File_Name_S));
      subtype Element_Type is String (1 .. File_Size);
      package File_IO is new Ada.Direct_IO (Element_Type);
      Input  : File_IO.File_Type;
      Result : Element_Type;
   begin
      File_IO.Open (Input, File_IO.In_File, File_Name_S);
      File_IO.Read (Input, Result);
      File_IO.Close (Input);
      return Result;
   end Read_Test_Failure;

   ----------------------
   -- Register_Failure --
   ----------------------

   procedure Register_Failure
     (State     : in out State_Type;
      Test_Name :        String)
   is

      use ZBTest_Messages.ZBTest_Wide_Strings;

      Cur_Name  : constant String := State.Get_String ("_fulltestname");
      Cur_Log   : constant String := State.Get_String ("_curlog");
      Fail_Name : constant String :=
        Wide_Compose (Name => Test_Name, Extension => "fail");
   begin
      Set_Last_Test (State);
      Rename_Test_Marker (Test_Name, Fail_Name);
      State.Increment ("_n_fail_cumulative", Deep => True);
      State.Increment ("_n_fail", Deep => False);
      State.Increment ("_n_tests", Deep => False);
      State.Prepend
        ("_xml_results",
         Format_01004 (+Cur_Name, +Cur_Log, +Read_Test_Failure (Fail_Name)));
      Print_10024 (+Test_Name);
   end Register_Failure;

   ----------------------
   -- Register_Success --
   ----------------------

   procedure Register_Success
     (State     : in out State_Type;
      Test_Name :        String)
   is

      use ZBTest_Messages.ZBTest_Wide_Strings;

      Cur_Name : constant String := State.Get_String ("_fulltestname");
      Cur_Log  : constant String := State.Get_String ("_curlog");
      OK_Name  : constant String :=
        Wide_Compose (Name => Test_Name, Extension => "ok");
   begin
      Set_Last_Test (State);
      Rename_Test_Marker (Test_Name, OK_Name);
      State.Increment ("_n_ok_cumulative", Deep => True);
      State.Increment ("_n_ok", Deep => False);
      State.Increment ("_n_tests", Deep => False);
      State.Prepend ("_xml_results", Format_01003 (+Cur_Name, +Cur_Log));
      Print_10023 (+Test_Name);
   end Register_Success;

   ----------------------
   -- Remove_Test_Area --
   ----------------------

   procedure Remove_Test_Area (Test_Area : String) is
   begin
      Print_10011 (+Test_Area);
      Wide_Delete_Tree (Test_Area);
   exception
      when E : Ada.Wide_Text_IO.Use_Error =>
         Print_10013 (+Test_Area, +E);
         raise Invalid_Environment;
   end Remove_Test_Area;

   ------------------------
   -- Rename_Test_Marker --
   ------------------------

   procedure Rename_Test_Marker
     (Test_Name   : String;
      Status_Name : String)
   is
   begin
      Wide_Rename (Test_Name, Status_Name);
   exception
      when E : Ada.Wide_Text_IO.Use_Error =>
         Print_10038 (+Status_Name, +E);
   end Rename_Test_Marker;

   ------------
   -- Search --
   ------------

   function Search
     (State       : State_Type;
      Name        : String;
      Paths_Name  : String;
      Exts_Name   : String;
      Target_Type : Search_Type)
      return String
   is
      Search_Path : constant List_Type := State.Get_List (Paths_Name);
      Extensions  : List_Type          := State.Get_List (Exts_Name);
   begin
      if Length (Extensions) = 0 then
         Append (Extensions, "");
      end if;
      for Path in 1 .. Length (Search_Path) loop
         for Extension in 1 .. Length (Extensions) loop
            declare
               Trail_Path : constant String :=
                 Wide_Compose
                   (Value (Search_Path, Path), Name,
                    Value (Extensions, Extension));
            begin
               case Target_Type is
                  when Search_File =>
                     if Wide_Is_File (Trail_Path) then
                        return Trail_Path;
                     end if;
                  when Search_Executable =>
                     if Wide_Is_Executable_File (Trail_Path) then
                        return Trail_Path;
                     end if;
                  when Search_Directory =>
                     if Wide_Is_Directory (Trail_Path) then
                        return Trail_Path;
                     end if;
               end case;
            end;
         end loop;
      end loop;
      raise File_Not_Found;
   end Search;

   ---------
   -- Set --
   ---------

   procedure Set
     (State : in out State_Type;
      Name  :        String;
      Value :        Value_Type'Class)
   is
   begin
      State.Parameters.Set (Name, Value);
   end Set;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean
     (State : in out State_Type;
      Name  :        String;
      Value :        Boolean)
   is
   begin
      State.Parameters.Set_Boolean (Name, Value);
   end Set_Boolean;

   ---------------
   -- Set_Float --
   ---------------

   procedure Set_Float
     (State : in out State_Type;
      Name  :        String;
      Value :        Float)
   is
   begin
      State.Parameters.Set_Float (Name, Value);
   end Set_Float;

   -----------------
   -- Set_Integer --
   -----------------

   procedure Set_Integer
     (State : in out State_Type;
      Name  :        String;
      Value :        Integer)
   is
   begin
      State.Parameters.Set_Integer (Name, Value);
   end Set_Integer;

   procedure Set_Last_Test (State : in out State_Type) is
      use Ada.Calendar;
      Now : constant Time := Clock;
   begin
      State.Set_Time ("_last_test", Now);
   end Set_Last_Test;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (State : in out State_Type;
      Name  :        String;
      Value :        String)
   is
   begin
      State.Parameters.Set_String (Name, Value);
   end Set_String;

   --------------
   -- Set_Time --
   --------------

   procedure Set_Time
     (State : in out State_Type;
      Name  :        String;
      Value :        Ada.Calendar.Time)
   is
   begin
      State.Parameters.Set_Time (Name, Value);
   end Set_Time;

   ---------------------
   -- Setup_Test_Area --
   ---------------------

   procedure Setup_Test_Area (State : in out State_Type) is
      Test_Area : constant String :=
        Wide_Full_Name (State.Get_String ("_testarea"));
   begin
      State.Prepend ("path", Test_Area);
      if Wide_Exists (Test_Area) then
         if Wide_Is_Directory (Test_Area) then
            Remove_Test_Area (Test_Area);
         else
            Print_10012 (+Test_Area);
            raise Invalid_Environment;
         end if;
      end if;
      Print_00009 (+Test_Area);
      Wide_Create_Directory (Test_Area);
      Wide_Set_Directory (Test_Area);
      State.Set_String ("_testarea", Test_Area);
   exception
      when E : Ada.Wide_Text_IO.Use_Error =>
         Print_10014 (+Test_Area, +E);
         raise Invalid_Environment;
   end Setup_Test_Area;

   ---------------
   -- Write_XML --
   ---------------

   procedure Write_XML (State : in out State_Type) is separate;

   ----------------------
   -- Write_XML_Report --
   ----------------------

   procedure Write_XML_Report (State : State_Type) is
   begin
      null;
   end Write_XML_Report;

end ZBTest.States;
