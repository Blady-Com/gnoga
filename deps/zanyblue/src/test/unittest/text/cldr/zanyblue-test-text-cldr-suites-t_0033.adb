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
procedure T_0033 (T : in out Test_Case'Class) is

   zh : constant Locale_Type := Make_Locale ("zh");

   procedure Check_Language (Abbreviation : String;
                             Value        : String);

   procedure Check_Language (Abbreviation : String;
                             Value        : String) is
   begin
      Check_Value (T, Language_Name (Abbreviation, Locale => zh), Value,
                      "Mis-match for " & Abbreviation);
   end Check_Language;

begin
   if not Is_Locale_Defined ("zh", "", "") then
      WAssert (T, True, "ZH localization not included");
      return;
   end if;
   Check_Language ("aa", "阿法文");
   Check_Language ("ba", "巴什客尔文");
   Check_Language ("ca", "加泰罗尼亚文");
   Check_Language ("da", "丹麦文");
   Check_Language ("el", "希腊文");
   Check_Language ("fa", "波斯文");
   Check_Language ("ga", "爱尔兰文");
   Check_Language ("ha", "豪萨文");
   Check_Language ("ia", "国际文字");
   Check_Language ("ja", "日文");
   Check_Language ("ka", "格鲁吉亚文");
   Check_Language ("la", "拉丁文");
   Check_Language ("mad", "马都拉文");
   Check_Language ("na", "瑙鲁文");
   Check_Language ("oc", "奥克西唐文");
   Check_Language ("pa", "旁遮普文");
   Check_Language ("qu", "盖丘亚文");
   Check_Language ("raj", "拉贾斯坦文");
   Check_Language ("sa", "梵文");
   Check_Language ("ta", "泰米尔文");
   Check_Language ("udm", "乌德穆尔特文");
   Check_Language ("vai", "瓦伊文");
   Check_Language ("wa", "瓦隆文");
   Check_Language ("xal", "卡尔梅克文");
   Check_Language ("yao", "瑶族文");
   Check_Language ("za", "壮文");
end T_0033;
