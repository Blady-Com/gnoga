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
procedure T_0038 (T : in out Test_Case'Class) is

   zh : constant Locale_Type := Make_Locale ("zh");

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String);

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String) is
   begin
      Check_Value (T, Script_Name (Abbreviation, Locale => zh), Value,
                      "Unexpected name for " & Abbreviation);
   end Check_Script;

begin
   if not Is_Locale_Defined ("zh", "", "") then
      WAssert (T, True, "ZH localization not included");
      return;
   end if;
   Check_Script ("Arab", "阿拉伯文");
   Check_Script ("Bali", "巴厘文");
   Check_Script ("Cans", "加拿大土著统一音节");
   Check_Script ("Deva", "天城文");
   Check_Script ("Egyd", "后期埃及文");
   Check_Script ("Geok", "格鲁吉亚文（教堂体）");
   Check_Script ("Hang", "韩文字");
   Check_Script ("Inds", "古希腊哈拉潘");
   Check_Script ("Java", "爪哇文");
   Check_Script ("Kali", "克耶李文字");
   Check_Script ("Lana", "兰拿文");
   Check_Script ("Mand", "阿拉米文");
   Check_Script ("Nkoo", "西非书面文字（N’Ko）");
   Check_Script ("Ogam", "欧甘文");
   Check_Script ("Perm", "古彼尔姆文");
   Check_Script ("Rjng", "拉让文");
   Check_Script ("Sara", "沙拉堤文");
   Check_Script ("Tagb", "塔格班瓦文");
   Check_Script ("Ugar", "乌加里特文");
   Check_Script ("Vaii", "瓦依文");
   Check_Script ("Xpeo", "古波斯文");
   Check_Script ("Yiii", "彝文");
   Check_Script ("Zxxx", "非书面文字");
end T_0038;
