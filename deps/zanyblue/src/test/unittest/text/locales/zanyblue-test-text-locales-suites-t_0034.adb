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
procedure T_0034 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   Locale : Locale_Type := Make_Locale ("en");

begin
   Check_Value (R, Locale_Name (Locale), "en",
                   "Expected locale name 'en'");
   Check_Value (R, Traits_Tag (Locale), "EN        ",
                   "Unexpected traits tag 'EN'");
   Check_Value (R, Traits_Name (Locale), "en",
                   "Unexpected traits name 'en'");
   Check_Value (R, Short_Day_Name (Locale, Mon), "Mon",
                "en short day name, Mon");
   Check_Value (R, Short_Day_Name (Locale, Tue), "Tue",
                "en short day name, Tue");
   Check_Value (R, Short_Day_Name (Locale, Wed), "Wed",
                "en short day name, Wed");
   Check_Value (R, Short_Day_Name (Locale, Thu), "Thu",
                "en short day name, Thu");
   Check_Value (R, Short_Day_Name (Locale, Fri), "Fri",
                "en short day name, Fri");
   Check_Value (R, Short_Day_Name (Locale, Sat), "Sat",
                "en short day name, Sat");
   Check_Value (R, Short_Day_Name (Locale, Sun), "Sun",
                "en short day name, Sun");
   Set_Traits (Locale, Wide_Name => "fr");
   Check_Value (R, Locale_Name (Locale), "en",
                   "Expected locale name 'en'");
   Check_Value (R, Traits_Tag (Locale), "FR        ",
                   "Unexpected traits tag 'FR'");
   Check_Value (R, Traits_Name (Locale), "fr",
                   "Unexpected traits name 'fr'");
   Check_Value (R, Short_Day_Name (Locale, Mon), "lun.",
                "fr short day name, Mon");
   Check_Value (R, Short_Day_Name (Locale, Tue), "mar.",
                "fr short day name, Tue");
   Check_Value (R, Short_Day_Name (Locale, Wed), "mer.",
                "fr short day name, Wed");
   Check_Value (R, Short_Day_Name (Locale, Thu), "jeu.",
                "fr short day name, Thu");
   Check_Value (R, Short_Day_Name (Locale, Fri), "ven.",
                "fr short day name, Fri");
   Check_Value (R, Short_Day_Name (Locale, Sat), "sam.",
                "fr short day name, Sat");
   Check_Value (R, Short_Day_Name (Locale, Sun), "dim.",
                "fr short day name, Sun");
end T_0034;
