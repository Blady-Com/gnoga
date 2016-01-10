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
with ZanyBlue.Text.Catalogs;

separate (ZanyBlue.Test.Utils.Suites)
procedure T_0002 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Catalogs;

   Test_Name  : constant Wide_String := "t_0002";
   Locale     : constant Locale_Type := Make_Locale ("");
   Facility   : constant Wide_String := "fac1";
   Catalog    : constant Catalog_Type := Create;
   Start_Time : Ada.Calendar.Time;
   Output     : File_Type;

begin
   Add (Catalog, Facility, "00001",
        "App, V{0}.{1}.{2} {3} (r{4}) at {5}",
        Locale);
   Add (Catalog, Facility, "00002",
        "Copyright notice: {0}",
        Locale);
   Add (Catalog, Facility, "00003",
        "Goodbye {0}, {1}",
        Locale);
   Set_Output (Output, Test_Area, Test_Name);
   Start_Time := Banner (Facility, Catalog => Catalog);
   Trailer (Facility, Start_Time, Catalog => Catalog);
   Restore_Output (Output);
   Check_Log_File (R, Test_Area, Test_Name,
                   "Banner should have been printed");
end T_0002;
