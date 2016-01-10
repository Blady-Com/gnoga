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
procedure T_0046 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   no : constant Locale_Type := Make_Locale ("no");
   nb : constant Locale_Type := Make_Locale ("nb");

   procedure Check_Territory (Abbreviation : Wide_String);

   procedure Check_Territory (Abbreviation : Wide_String) is
   begin
      Check_Value (R, Territory_Name (Abbreviation, Locale => no),
                      Territory_Name (Abbreviation, Locale => nb),
                      "no/nb territory names");
   end Check_Territory;

begin
   if not Is_Locale_Defined ("no", "", "") then
      WAssert (R, True, "NO localization not included");
      return;
   end if;
   if not Is_Locale_Defined ("nb", "", "") then
      WAssert (R, True, "NB localization not included");
      return;
   end if;
   Check_Territory ("001");
   Check_Territory ("AC");
   Check_Territory ("BB");
   Check_Territory ("CA");
   Check_Territory ("DE");
   Check_Territory ("EA");
   Check_Territory ("FI");
   Check_Territory ("GA");
   Check_Territory ("HK");
   Check_Territory ("IC");
   Check_Territory ("JE");
   Check_Territory ("KE");
   Check_Territory ("LA");
   Check_Territory ("MA");
   Check_Territory ("NA");
   Check_Territory ("OM");
   Check_Territory ("PA");
   Check_Territory ("QA");
   Check_Territory ("RE");
   Check_Territory ("SA");
   Check_Territory ("TA");
   Check_Territory ("UA");
   Check_Territory ("VA");
   Check_Territory ("WF");
   Check_Territory ("YE");
   Check_Territory ("ZA");
end T_0046;
