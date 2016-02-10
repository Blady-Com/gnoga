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

separate (ZanyBlue.Test.Text.Locales.Suites)
procedure T_0031 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   procedure Check_Locale (Value  : Wide_String;
                           Name   : Wide_String;
                           LTag   : Wide_String;
                           LName  : Wide_String);

   procedure Check_Locale (Value  : Wide_String;
                           Name   : Wide_String;
                           LTag   : Wide_String;
                           LName  : Wide_String) is
      Locale : constant Locale_Type := Make_Locale (Value);
   begin
      Check_Value (R, Locale_Name (Locale), Name,
                      "Unexpected name value");
      Check_Value (R, Traits_Tag (Locale), LTag,
                      "Unexpected traits tag value");
      Check_Value (R, Traits_Name (Locale), LName,
                      "Unexpected traits name value");
   end Check_Locale;

begin
   Check_Locale ("en",         "en",         "EN        ", "en");
   Check_Locale ("fr_FR",      "fr_FR",      "FR        ", "fr");
   Check_Locale ("zh_Hant",    "zh_Hant",    "ZH HANT   ", "zh_Hant");
   Check_Locale ("en-us",      "en_US",      "EN        ", "en");
   Check_Locale ("zh-hant-tw", "zh_Hant_TW", "ZH HANT   ", "zh_Hant");
end T_0031;