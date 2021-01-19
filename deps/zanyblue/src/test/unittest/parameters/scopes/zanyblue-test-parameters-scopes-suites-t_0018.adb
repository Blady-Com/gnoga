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

with Ada.Calendar;
with Ada.Wide_Text_IO;
with ZanyBlue.OS;

separate (ZanyBlue.Test.Parameters.Scopes.Suites)
procedure T_0018 (T : in out Test_Case'Class) is

   use Ada.Calendar;
   use Ada.Wide_Text_IO;
   use ZanyBlue.OS;

   Test_Name  : constant String := "t_0018";
   Filename   : constant String := Test_Log_Name (Test_Area, Test_Name);
   T1         : constant Time := Time_Of (2011, 10, 31, Duration (60483));
   T2         : constant Time := Time_Of (2011, 11, 2, Duration (60483));
   Output     : File_Type;
   Scope : Parameter_Stack_Type;

begin
   Scope.New_Scope;
   Scope.Set_Boolean ("b", True);
   Scope.Set_Float ("f", 0.0);
   Scope.Set_Integer ("i", 10);
   Scope.Append ("l", "a");
   Scope.Append ("l", "b");
   Scope.Append ("l", "c");
   Scope.Set_Time ("t", T1);
   Scope.Set_String ("s", "abc");
   Scope.New_Scope;
   Scope.Set_Boolean ("b", False);
   Scope.Set_Float ("f", 1.0);
   Scope.Set_Integer ("i", -10);
   Scope.Append ("l", "x");
   Scope.Append ("l", "y");
   Scope.Append ("l", "z");
   Scope.Set_Time ("t", T2);
   Scope.Set_String ("s", "xyz");
   Wide_Create (Output, Filename);
   Scope.Dump (Output, False);
   Close (Output);
   Check_Log_File (T, Test_Area, Test_Name,
           "Dump local scope value");
end T_0018;
