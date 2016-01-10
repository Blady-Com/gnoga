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
procedure T_0044 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   no : constant Locale_Type := Make_Locale ("no");
   nb : constant Locale_Type := Make_Locale ("nb");

   procedure Check_Language (Abbreviation : Wide_String);

   procedure Check_Language (Abbreviation : Wide_String) is
   begin
      Check_Value (R, Language_Name (Abbreviation, Locale => no),
                      Language_Name (Abbreviation, Locale => nb),
                      "no/nb language names");
   end Check_Language;

begin
   if not Is_Locale_Defined ("no", "", "") then
      WAssert (R, True, "NO localization not included");
      return;
   end if;
   if not Is_Locale_Defined ("nb", "", "") then
      WAssert (R, True, "NB localization not included");
      return;
   end if;
   Check_Language ("aa");
   Check_Language ("ba");
   Check_Language ("ca");
   Check_Language ("da");
   Check_Language ("ee");
   Check_Language ("fa");
   Check_Language ("ga");
   Check_Language ("ha");
   Check_Language ("ia");
   Check_Language ("ja");
   Check_Language ("ka");
   Check_Language ("ky");
   Check_Language ("mad");
   Check_Language ("na");
   Check_Language ("oc");
   Check_Language ("pa");
   Check_Language ("qu");
   Check_Language ("raj");
   Check_Language ("sa");
   Check_Language ("ta");
   Check_Language ("udm");
   Check_Language ("vai");
   Check_Language ("wa");
   Check_Language ("xal");
   Check_Language ("yao");
   Check_Language ("za");
end T_0044;
