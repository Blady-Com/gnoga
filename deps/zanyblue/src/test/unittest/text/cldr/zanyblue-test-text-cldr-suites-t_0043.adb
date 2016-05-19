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
procedure T_0043 (T : in out Test_Case'Class) is

   zh : constant Locale_Type := Make_Locale ("zh");

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String);

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String) is
   begin
      Check_Value (T, Territory_Name (Abbreviation, Locale => zh), Value,
                      "Mis-match for " & Abbreviation);
   end Check_Territory;

begin
   if not Is_Locale_Defined ("zh", "", "") then
      WAssert (T, True, "ZH localization not included");
      return;
   end if;
   Check_Territory ("001", "世界");
   Check_Territory ("AC", "阿森松岛");
   Check_Territory ("BA", "波斯尼亚和黑塞哥维那");
   Check_Territory ("CA", "加拿大");
   Check_Territory ("DE", "德国");
   Check_Territory ("EA", "休达及梅利利亚");
   Check_Territory ("FI", "芬兰");
   Check_Territory ("GA", "加蓬");
   Check_Territory ("HK", "中国香港特别行政区");
   Check_Territory ("IC", "加纳利群岛");
   Check_Territory ("JE", "泽西岛");
   Check_Territory ("KE", "肯尼亚");
   Check_Territory ("LA", "老挝");
   Check_Territory ("MA", "摩洛哥");
   Check_Territory ("NA", "纳米比亚");
   Check_Territory ("OM", "阿曼");
   Check_Territory ("PA", "巴拿马");
   Check_Territory ("QA", "卡塔尔");
   Check_Territory ("RE", "留尼汪");
   Check_Territory ("SA", "沙特阿拉伯");
   Check_Territory ("TA", "特里斯坦-达库尼亚群岛");
   Check_Territory ("UA", "乌克兰");
   Check_Territory ("VA", "梵蒂冈");
   Check_Territory ("WF", "瓦利斯和富图纳");
   Check_Territory ("YE", "也门");
   Check_Territory ("ZA", "南非");
end T_0043;
