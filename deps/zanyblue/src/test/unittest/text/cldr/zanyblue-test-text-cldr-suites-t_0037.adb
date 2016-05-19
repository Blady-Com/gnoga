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
procedure T_0037 (T : in out Test_Case'Class) is

   ja : constant Locale_Type := Make_Locale ("ja");

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String);

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String) is
   begin
      Check_Value (T, Script_Name (Abbreviation, Locale => ja), Value,
                      "Unexpected script name for " & Abbreviation);
   end Check_Script;

begin
   if not Is_Locale_Defined ("ja", "", "") then
      WAssert (T, True, "JA localization not included");
      return;
   end if;
   Check_Script ("Arab", "アラビア文字");
   Check_Script ("Bali", "バリ文字");
   Check_Script ("Cans", "統合カナダ先住民音節文字");
   Check_Script ("Deva", "デーバナーガリー文字");
   Check_Script ("Egyd", "エジプト民衆文字");
   Check_Script ("Geok", "グルジア文字(フツリ)");
   Check_Script ("Hang", "ハングル");
   Check_Script ("Inds", "インダス文字");
   Check_Script ("Java", "ジャワ文字");
   Check_Script ("Kali", "カヤー文字");
   Check_Script ("Laoo", "ラオ文字");
   Check_Script ("Mand", "マンダ文字");
   Check_Script ("Nkoo", "ンコ文字");
   Check_Script ("Ogam", "オガム文字");
   Check_Script ("Perm", "古ぺルム文字");
   Check_Script ("Roro", "ロンゴロンゴ文字");
   Check_Script ("Sara", "サラティ文字");
   Check_Script ("Tagb", "タグバンワ文字");
   Check_Script ("Ugar", "ウガリット文字");
   Check_Script ("Vaii", "ヴァイ文字");
   Check_Script ("Xpeo", "古代ペルシア文字");
   Check_Script ("Yiii", "イ文字");
   Check_Script ("Zxxx", "非表記");
end T_0037;
