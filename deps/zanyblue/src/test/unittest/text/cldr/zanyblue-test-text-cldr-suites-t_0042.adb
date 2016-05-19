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
procedure T_0042 (T : in out Test_Case'Class) is

   ja : constant Locale_Type := Make_Locale ("ja");

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String);

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String) is
   begin
      Check_Value (T, Territory_Name (Abbreviation, Locale => ja), Value,
                      "Expected: " & Value);
   end Check_Territory;

begin
   if not Is_Locale_Defined ("ja", "", "") then
      WAssert (T, True, "JA localization not included");
      return;
   end if;
   Check_Territory ("001", "世界");
   Check_Territory ("AC", "アセンション島");
   Check_Territory ("BA", "ボスニア・ヘルツェゴビナ");
   Check_Territory ("CA", "カナダ");
   Check_Territory ("DE", "ドイツ");
   Check_Territory ("EA", "セウタ・メリリャ");
   Check_Territory ("FI", "フィンランド");
   Check_Territory ("GA", "ガボン");
   Check_Territory ("HK", "中華人民共和国香港特別行政区");
   Check_Territory ("IC", "カナリア諸島");
   Check_Territory ("JE", "ジャージー");
   Check_Territory ("KE", "ケニア");
   Check_Territory ("LA", "ラオス");
   Check_Territory ("MA", "モロッコ");
   Check_Territory ("NA", "ナミビア");
   Check_Territory ("OM", "オマーン");
   Check_Territory ("PA", "パナマ");
   Check_Territory ("QA", "カタール");
   Check_Territory ("RE", "レユニオン島");
   Check_Territory ("SA", "サウジアラビア");
   Check_Territory ("TA", "トリスタン・ダ・クーニャ");
   Check_Territory ("UA", "ウクライナ");
   Check_Territory ("VA", "バチカン市国");
   Check_Territory ("WF", "ウォリス・フツナ");
   Check_Territory ("YE", "イエメン");
   Check_Territory ("ZA", "南アフリカ");
end T_0042;
