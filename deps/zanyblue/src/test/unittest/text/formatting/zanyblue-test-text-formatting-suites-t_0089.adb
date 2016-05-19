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

with ZanyBlue.Text.Catalogs;

separate (ZanyBlue.Test.Text.Formatting.Suites)
procedure T_0089 (T : in out Test_Case'Class) is

   use ZanyBlue.Text.Catalogs;

   Test_Name : constant Wide_String := "t_0089";
   Locale    : constant Locale_Type := Make_Locale ("");
   Facility  : constant Wide_String := "fac1";
   Key       : constant Wide_String := "key1";
   Message   : constant Wide_String := "Message: #1={0}, #2={1}";
   Catalog   : constant Catalog_Type := Create;

   procedure With_Exceptions (Name : Wide_String);
   procedure Without_Exception (Name : Wide_String);

   procedure With_Exceptions (Name : Wide_String) is
      Output    : File_Type;
   begin
      Enable_Exceptions (Catalog);
      Create_Log_File (Output, Test_Area, Name);
      Print (Output, Facility, Key, +10,
             Locale => Locale, Catalog => Catalog);
      Close (Output);
      WAssert (T, False, "Exception not raised");
   exception
   when No_Such_Argument_Error =>
      Close (Output);
      WAssert (T, True, "Expected exception raised");
   end With_Exceptions;

   procedure Without_Exception (Name : Wide_String) is
      Output    : File_Type;
   begin
      Disable_Exceptions (Catalog);
      Create_Log_File (Output, Test_Area, Name);
      Print (Output, Facility, Key, +10,
             Locale => Locale, Catalog => Catalog);
      Close (Output);
   end Without_Exception;

begin
   Add (Catalog, Facility, Key, Message, Locale);
   With_Exceptions (Test_Name & "a");
   Check_Log_File (T, Test_Area, Test_Name & "a",
           "Should be empty from exception");
   Without_Exception (Test_Name & "b");
   Check_Log_File (T, Test_Area, Test_Name & "b",
           "Exception output failure");
end T_0089;
