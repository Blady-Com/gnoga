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

separate (ZanyBlue.Test.Text.Formatting.Suites)
procedure T_0099 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Calendar;
   use ZanyBlue.Text.Catalogs;

   Catalog   : constant Catalog_Type := Create;
   en        : constant Locale_Type := Make_Locale ("en");
   fr        : constant Locale_Type := Make_Locale ("fr");
   T         : constant Time := Time_Of (2012, 1, 16, Duration (60483));

begin
   Add (Catalog, "myfac", "mykey", "Today is {0,EEEE}", Root_Locale, en);
   Enable_Source_Locales (Catalog);
   Check_Value (R, Format ("myfac", "mykey", +T,
                           Catalog => Catalog, Locale => fr),
                   "Today is Monday",
                   "Expected Today is Monday");
   Disable_Source_Locales (Catalog);
   Check_Value (R, Format ("myfac", "mykey", +T,
                           Catalog => Catalog, Locale => fr),
                   "Today is lundi",
                   "Expected Today is lundi");
end T_0099;
