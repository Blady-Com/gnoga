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

separate (ZanyBlue.Test.Text.CLDR.Suites)
procedure T_0040 (T : in out Test_Case'Class) is

   ar : constant Locale_Type := Make_Locale ("ar");

   procedure Check_Territory (Abbreviation : String;
                              Value        : String);

   procedure Check_Territory (Abbreviation : String;
                              Value        : String) is
   begin
      Check_Value (T, Territory_Name (Abbreviation, Locale => ar), Value,
                      "Unexpected name for " & Abbreviation);
   end Check_Territory;

begin
   if not Is_Locale_Defined ("ar", "", "") then
      WAssert (T, True, "AR localization not included");
      return;
   end if;
   Check_Territory ("001", "العالم");
   Check_Territory ("AC", "جزيرة أسينشيون");
   Check_Territory ("BA", "البوسنة والهرسك");
   Check_Territory ("CA", "كندا");
   Check_Territory ("DE", "ألمانيا");
   Check_Territory ("EA", "سيوتا وميليلا");
   Check_Territory ("FI", "فنلندا");
   Check_Territory ("GA", "الجابون");
   Check_Territory ("HK", "هونغ كونغ الصينية");
   Check_Territory ("IC", "جزر الكناري");
   Check_Territory ("JE", "جيرسي");
   Check_Territory ("KE", "كينيا");
   Check_Territory ("LA", "لاوس");
   Check_Territory ("MA", "المغرب");
   Check_Territory ("NA", "ناميبيا");
   Check_Territory ("OM", "عُمان");
   Check_Territory ("PA", "بنما");
   Check_Territory ("QA", "قطر");
   Check_Territory ("RE", "روينيون");
   Check_Territory ("SA", "المملكة العربية السعودية");
   Check_Territory ("TA", "تريستان دي كونها");
   Check_Territory ("UA", "أوكرانيا");
   Check_Territory ("VA", "الفاتيكان");
   Check_Territory ("WF", "جزر والس وفوتونا");
   Check_Territory ("YE", "اليمن");
   Check_Territory ("ZA", "جنوب أفريقيا");
end T_0040;
