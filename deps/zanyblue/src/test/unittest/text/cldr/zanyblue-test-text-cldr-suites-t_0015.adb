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

separate (ZanyBlue.Test.Text.CLDR.Suites)
procedure T_0015 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L_Name : constant Wide_String := "en_US";
   L : constant Locale_Type := Make_Locale (L_Name);

begin
   Check_Value (R, Full_Locale_Name (Make_Locale (""), L),
                "Root",
                L_Name & " full locale name for base");
   Check_Value (R, Full_Locale_Name (Make_Locale ("en"), L),
                "English",
                L_Name & " full locale name for en");
   Check_Value (R, Full_Locale_Name (Make_Locale ("en_US"), L),
                "English (United States)",
                L_Name & " full locale name for en_US");
   Check_Value (R, Full_Locale_Name (Make_Locale ("fr"), L),
                "French",
                L_Name & " full locale name for fr");
   Check_Value (R, Full_Locale_Name (Make_Locale ("fr_CA"), L),
                "French (Canada)",
                L_Name & " full locale name for fr_CA");
end T_0015;