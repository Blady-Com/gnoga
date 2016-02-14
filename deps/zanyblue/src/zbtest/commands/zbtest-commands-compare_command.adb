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

with Ada.Strings.Wide_Unbounded;
with ZanyBlue.Wide_Directories;
with ZanyBlue.Wide_Regexp;

separate (ZBTest.Commands)
procedure Compare_Command (State : in out State_Type;
                           Args  : List_Type) is

   use Ada.Strings.Wide_Unbounded;
   use ZanyBlue.Wide_Directories;

   procedure Compare (State    : in out State_Type;
                      Log_Name : Wide_String;
                      Ref_Name : Wide_String);
   --  Compare two files.  This should really try to do a context diff.

   procedure Match_Lines (Status_File : in out File_Type;
                          Ref_Line    : Wide_String;
                          Gen_Line    : Wide_String;
                          Matched     : out Boolean);
   --  Match two lines from two files being compared.  If the lines don't
   --  match exactly as strings, try using the reference line as a regex and
   --  try regex matching.

   function Regex_Match (Gen_Line : Wide_String;
                         Ref_Line : Wide_String) return Boolean;
   --  Match two lines by considering the reference line to be a regular
   --  expression.

   function Status_File_Name (State    : State_Type;
                              Log_Name : Wide_String) return Wide_String;
   --  Name of the comparsion log file.

   -------------
   -- Compare --
   -------------

   procedure Compare (State    : in out State_Type;
                      Log_Name : Wide_String;
                      Ref_Name : Wide_String) is

      type File_Names is (Status_File, Ref_File, Log_File);
      type File_List is array (File_Names) of File_Type;

      Open_Failed : exception;

      procedure Open_File (Status : in out File_Type;
                           File   : in out File_Type;
                           Name   : Wide_String);
      --  Open a file.  An error message is printed if the open fails and
      --  the exception Open_Failed is raised.

      procedure Wrap_Up (Files       : in out File_List;
                         Status_Name : Wide_String;
                         Fail        : Boolean);
      --  Wrap up the comparsion by closing any open files and registering
      --  the comparsion result: failure or success.

      ---------------
      -- Open_File --
      ---------------

      procedure Open_File (Status : in out File_Type;
                           File   : in out File_Type;
                           Name   : Wide_String) is
      begin
         Wide_Open (File, In_File, Name);
      exception
      when others =>
         Print_10022 (+Name, Status);
         Print_10022 (+Name);
         raise Open_Failed;
      end Open_File;

      -------------
      -- Wrap_Up --
      -------------

      procedure Wrap_Up (Files       : in out File_List;
                         Status_Name : Wide_String;
                         Fail        : Boolean) is
      begin
         for I in Files'Range loop
            if Is_Open (Files (I)) then
               Close (Files (I));
            end if;
         end loop;
         if Fail then
            State.Register_Failure (Status_Name);
         else
            State.Register_Success (Status_Name);
         end if;
      end Wrap_Up;

      Status_Name : constant Wide_String := Status_File_Name (State, Log_Name);
      Files       : File_List;
      Matched     : Boolean := True;
      Fail        : Boolean := False;

   begin
      Wide_Create (Files (Status_File), Status_Name);
      Open_File (Files (Status_File), Files (Ref_File),
                 State.Locate_File (Ref_Name));
      Open_File (Files (Status_File), Files (Log_File), Log_Name);
      while not End_Of_File (Files (Ref_File))
        and not End_Of_File (Files (Log_File))
      loop
         Match_Lines (Files (Status_File), Get_Line (Files (Ref_File)),
                      Get_Line (Files (Log_File)), Matched);
         Fail := Fail or else not Matched;
      end loop;
      while not End_Of_File (Files (Ref_File)) loop
         Fail := True;
         Put_Line (Files (Status_File), "-" & Get_Line (Files (Ref_File)));
      end loop;
      while not End_Of_File (Files (Log_File)) loop
         Fail := True;
         Put_Line (Files (Status_File), "+" & Get_Line (Files (Log_File)));
      end loop;
      Wrap_Up (Files, Status_Name, Fail);
   exception
   when Open_Failed =>
      Wrap_Up (Files, Status_Name, True);
   when File_Not_Found =>
      Print_10028 (+Ref_Name);
      Print_10028 (+Ref_Name, Files (Status_File));
      Wrap_Up (Files, Status_Name, True);
   when E : others =>
      Wrap_Up (Files, Status_Name, True);
      Print_10027 (+Status_Name, +E);
   end Compare;

   -----------------
   -- Match_Lines --
   -----------------

   procedure Match_Lines (Status_File : in out File_Type;
                          Ref_Line    : Wide_String;
                          Gen_Line    : Wide_String;
                          Matched     : out Boolean) is
   begin
      Matched := Ref_Line = Gen_Line or else Regex_Match (Gen_Line, Ref_Line);
      if not Matched then
         Put (Status_File, ">");
         Put_Line (Status_File, Ref_Line);
         Put (Status_File, "<");
         Put_Line (Status_File, Gen_Line);
      end if;
   end Match_Lines;

   -----------------
   -- Regex_Match --
   -----------------

   function Regex_Match (Gen_Line : Wide_String;
                         Ref_Line : Wide_String) return Boolean is
      use ZanyBlue.Wide_Regexp;
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

   function Status_File_Name (State    : State_Type;
                              Log_Name : Wide_String) return Wide_String is
      Test_Name : constant Wide_String := State.Full_Test_Name;
      Base_Name : constant Wide_String := Wide_Base_Name (Log_Name);
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
