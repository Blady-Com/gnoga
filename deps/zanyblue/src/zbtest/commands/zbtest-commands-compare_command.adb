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

--  @usage compare log-file [ ref-log-file ]
--  @summary compare a log file with a reference log
--  @start-doc
--  The compare command compares (with regular expression matching) a
--  generated log file with a reference log file.  If the files match, a
--  .ok file is created and the number of OK tests is incremented, otherwise
--  a .fail file is created and the number of FAIL tests is incremented.
--
--  The reference log file is found by searching the searchpath parameter
--  and is normally in the same directory as the .zbt test script.
--

with Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded;
with ZanyBlue.Wide_Directories;
with ZBTest_Messages.ZBTest_Wide_Prints;
with GNAT.Regexp;

separate (ZBTest.Commands)
procedure Compare_Command
  (State : in out State_Type;
   Args  :        List_Type)
is

   use Ada.Strings.Wide_Unbounded;
   use ZanyBlue.Wide_Directories;

   procedure Compare
     (State    : in out State_Type;
      Log_Name :        String;
      Ref_Name :        String);
   --  Compare two files.  This should really try to do a context diff.

   procedure Match_Lines
     (Status_File : in out Ada.Wide_Text_IO.File_Type;
      Ref_Line    :        String;
      Gen_Line    :        String;
      Matched     :    out Boolean);
   --  Match two lines from two files being compared.  If the lines don't
   --  match exactly as strings, try using the reference line as a regex and
   --  try regex matching.

   function Regex_Match
     (Gen_Line : String;
      Ref_Line : String)
      return Boolean;
   --  Match two lines by considering the reference line to be a regular
   --  expression.

   function Status_File_Name
     (State    : State_Type;
      Log_Name : String)
      return String;
   --  Name of the comparsion log file.

   -------------
   -- Compare --
   -------------

   procedure Compare
     (State    : in out State_Type;
      Log_Name :        String;
      Ref_Name :        String)
   is

      type File_Names is (Status_File, Ref_File, Log_File);
      type File_List is array (File_Names) of Ada.Wide_Text_IO.File_Type;

      Open_Failed : exception;

      procedure Open_File
        (Status : in out Ada.Wide_Text_IO.File_Type;
         File   : in out Ada.Wide_Text_IO.File_Type;
         Name   :        String);
      --  Open a file.  An error message is printed if the open fails and
      --  the exception Open_Failed is raised.

      procedure Wrap_Up
        (Files       : in out File_List;
         Status_Name :        String;
         Fail        :        Boolean);
      --  Wrap up the comparsion by closing any open files and registering
      --  the comparsion result: failure or success.

      ---------------
      -- Open_File --
      ---------------

      procedure Open_File
        (Status : in out Ada.Wide_Text_IO.File_Type;
         File   : in out Ada.Wide_Text_IO.File_Type;
         Name   :        String)
      is
      begin
         Wide_Open (File, Ada.Wide_Text_IO.In_File, Name);
      exception
         when others =>
            ZBTest_Messages.ZBTest_Wide_Prints.Print_10022 (+Name, Status);
            Print_10022 (+Name);
            raise Open_Failed;
      end Open_File;

      -------------
      -- Wrap_Up --
      -------------

      procedure Wrap_Up
        (Files       : in out File_List;
         Status_Name :        String;
         Fail        :        Boolean)
      is
      begin
         for I in Files'Range loop
            if Ada.Wide_Text_IO.Is_Open (Files (I)) then
               Ada.Wide_Text_IO.Close (Files (I));
            end if;
         end loop;
         if Fail then
            State.Register_Failure (Status_Name);
         else
            State.Register_Success (Status_Name);
         end if;
      end Wrap_Up;

      Status_Name : constant String := Status_File_Name (State, Log_Name);
      Files       : File_List;
      Matched     : Boolean              := True;
      Fail        : Boolean              := False;

   begin
      State.Set_String ("_curlog", Log_Name);
      Wide_Create (Files (Status_File), Status_Name);
      Open_File
        (Files (Status_File), Files (Ref_File), State.Locate_File (Ref_Name));
      Open_File (Files (Status_File), Files (Log_File), Log_Name);
      while not Ada.Wide_Text_IO.End_Of_File (Files (Ref_File)) and
        not Ada.Wide_Text_IO.End_Of_File (Files (Log_File))
      loop
         Match_Lines
           (Files (Status_File), Ada.Wide_Text_IO.Get_Line (Files (Ref_File)),
            Ada.Wide_Text_IO.Get_Line (Files (Log_File)), Matched);
         Fail := Fail or else not Matched;
      end loop;
      while not Ada.Wide_Text_IO.End_Of_File (Files (Ref_File)) loop
         Fail := True;
         Ada.Wide_Text_IO.Put_Line
           (Files (Status_File),
            "-" & Ada.Wide_Text_IO.Get_Line (Files (Ref_File)));
      end loop;
      while not Ada.Wide_Text_IO.End_Of_File (Files (Log_File)) loop
         Fail := True;
         Ada.Wide_Text_IO.Put_Line
           (Files (Status_File),
            "+" & Ada.Wide_Text_IO.Get_Line (Files (Log_File)));
      end loop;
      Wrap_Up (Files, Status_Name, Fail);
   exception
      when Open_Failed =>
         Wrap_Up (Files, Status_Name, True);
      when File_Not_Found =>
         ZBTest_Messages.ZBTest_Wide_Prints.Print_10028
           (+Ref_Name, Files (Status_File));
         Print_10028 (+Ref_Name);
         Wrap_Up (Files, Status_Name, True);
      when E : others =>
         Wrap_Up (Files, Status_Name, True);
         Print_10027 (+Status_Name, +E);
   end Compare;

   -----------------
   -- Match_Lines --
   -----------------

   procedure Match_Lines
     (Status_File : in out Ada.Wide_Text_IO.File_Type;
      Ref_Line    :        String;
      Gen_Line    :        String;
      Matched     :    out Boolean)
   is
   begin
      Matched :=
        Ref_Line = Gen_Line
        or else Regex_Match (Wide_To_UTF8 (Gen_Line), Wide_To_UTF8 (Ref_Line));
      if not Matched then
         Ada.Wide_Text_IO.Put (Status_File, ">");
         Ada.Wide_Text_IO.Put_Line (Status_File, Ref_Line);
         Ada.Wide_Text_IO.Put (Status_File, "<");
         Ada.Wide_Text_IO.Put_Line (Status_File, Gen_Line);
      end if;
   end Match_Lines;

   -----------------
   -- Regex_Match --
   -----------------

   function Regex_Match
     (Gen_Line : String;
      Ref_Line : String)
      return Boolean
   is
      use GNAT.Regexp;
   begin
      return Match (Gen_Line, Compile (Ref_Line));
   exception
      when Error_In_Regexp =>
         return False;
      when E : others =>
         Print_10025 (+E);
         return False;
   end Regex_Match;

   ----------------------
   -- Status_File_Name --
   ----------------------

   function Status_File_Name
     (State    : State_Type;
      Log_Name : String)
      return String
   is
      Test_Name : constant String := State.Full_Test_Name;
      Base_Name : constant String := Wide_Base_Name (Log_Name);
      Buffer    : Unbounded_Wide_String;
   begin
      Append (Buffer, Test_Name);
      Append (Buffer, ".");
      Append (Buffer, Base_Name);
      return To_Wide_String (Buffer);
   end Status_File_Name;

begin
   case Length (Args) is
      when 2 =>
         Compare (State, Value (Args, 2), Value (Args, 2));
      when 3 =>
         Compare (State, Value (Args, 2), Value (Args, 3));
      when others =>
         raise Command_Usage_Error;
   end case;
end Compare_Command;
