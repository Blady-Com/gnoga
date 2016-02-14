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

with Ada.Calendar;
with Ada.Wide_Text_IO;
with AUnit.Test_Cases;
with AUnit.Reporter;

package ZanyBlue.Test is

   use AUnit.Test_Cases;

   procedure Add_Routine (Test    : in out Test_Case'Class;
                          Routine : Test_Routine;
                          Name    : String);

   --  Following are useful during testing when we are not interested
   --  in a string value but need to generate it, e.g., when we expect
   --  an exception should be raise.
   procedure Discard (Value      : String);
   procedure Discard (Wide_Value : Wide_String);
   procedure Discard (Value      : Integer);
   procedure Discard (Value      : Float);
   procedure Discard (Value      : Ada.Calendar.Time);
   procedure Discard (Value      : Boolean);

   procedure Check_Value (Test      : in out AUnit.Test_Cases.Test_Case'Class;
                          Generated : Wide_String;
                          Expected  : Wide_String;
                          Message   : Wide_String := "Failure");

   procedure WAssert (Test      : in out AUnit.Test_Cases.Test_Case'Class;
                      Condition : Boolean;
                      Message   : Wide_String);

   procedure Check_Log_File (Test    : in out AUnit.Test_Cases.Test_Case'Class;
                             Test_Area : Wide_String;
                             Test_Name : Wide_String;
                             Message   : Wide_String);
   function Compare_Files (Name_A, Name_B : Wide_String) return Boolean;
   function Compare_Log_File (Test_Area : Wide_String;
                              Test_Name : Wide_String) return Boolean;
   procedure Create_Log_File (File      : in out Ada.Wide_Text_IO.File_Type;
                              Test_Area : Wide_String;
                              Test_Name : Wide_String);
   function Test_Src_Directory (Test_Area : Wide_String) return Wide_String;
   function Test_RefLog_Name (Test_Area : Wide_String;
                              Test_Name : Wide_String) return Wide_String;
   function Test_In_Name (Test_Area : Wide_String;
                          Test_Name : Wide_String) return Wide_String;
   function Test_Log_Name (Test_Area : Wide_String;
                           Test_Name : Wide_String) return Wide_String;
   procedure Set_Output (File : Ada.Wide_Text_IO.File_Type)
      renames Ada.Wide_Text_IO.Set_Output;
   procedure Set_Output (Output    : in out Ada.Wide_Text_IO.File_Type;
                         Test_Area : Wide_String;
                         Test_Name : Wide_String);
   procedure Restore_Output (Output : in out Ada.Wide_Text_IO.File_Type);
   procedure Restore_Output;

   Usage_Error : exception;

   function Use_XML return Boolean;
   function Top_Directory return Wide_String;
   function Reporter_Implementation return AUnit.Reporter.Reporter'Class;

end ZanyBlue.Test;
