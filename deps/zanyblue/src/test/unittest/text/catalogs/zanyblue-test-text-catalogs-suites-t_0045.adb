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

separate (ZanyBlue.Test.Text.Catalogs.Suites)
procedure T_0045 (T : in out Test_Case'Class) is

   Facility     : constant Wide_String := "xstrings";
   Dir_Name     : constant Wide_String := Test_Src_Directory (Test_Area);
   Catalog      : Catalog_Type;
   N_Locales    : Natural;
   N_Messages   : Natural;

   procedure Check (Locale           : Wide_String;
                    Value            : Wide_String;
                    Expect_Exception : Boolean := False);

   procedure Check (Locale           : Wide_String;
                    Value            : Wide_String;
                    Expect_Exception : Boolean := False) is
      L : constant Locale_Type := Make_Locale (Locale);
   begin
      Check_Value (T, Get_Text (Catalog, Facility, "reboot.title", L), Value);
      WAssert (T, not Expect_Exception,
               "Expected No_Such_Message_Error exception");
   exception
   when No_Such_Message_Error =>
      WAssert (T, Expect_Exception,
               "Expected No_Such_Message_Error exception");
   end Check;

begin
   Catalog := Create;
   Load_Facility (Catalog, Facility, N_Locales, N_Messages, Dir_Name);
   WAssert (T, N_Locales = 9, "Expected 9 locales");
   WAssert (T, N_Messages = 189,
             "Expected 189 messages: " & Natural'Wide_Image (N_Messages));
   Check ("de", "Neustart");
   Check ("de_DE", "Neustart");
   Check ("es", "Reiniciar");
   Check ("es_ES", "Reiniciar");
   Check ("fr", "Redémarrer");
   Check ("fr_FR", "Redémarrer");
   Check ("it", "Riavvia");
   Check ("it_IT", "Riavvia");
   Check ("ja", "再起動");
   Check ("ja_JP", "再起動");
   Check ("ko", "다시 시작");
   Check ("ko_KR", "다시 시작");
   Check ("sv", "Starta om");
   Check ("sv_SE", "Starta om");
   Check ("zh", "*", True);
   Check ("zh_CN", "重新启动");
   Check ("zh_TW", "重新啟動");
   Check ("", "*", True);
end T_0045;
