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
procedure T_0035 (T : in out Test_Case'Class) is

   ar : constant Locale_Type := Make_Locale ("ar");

   procedure Check_Script (Abbreviation : String;
                           Value        : String);

   procedure Check_Script (Abbreviation : String;
                           Value        : String) is
   begin
      Check_Value (T, Script_Name (Abbreviation, Locale => ar), Value,
                      "Unexpected script name for " & Abbreviation);
   end Check_Script;

begin
   if not Is_Locale_Defined ("ar", "", "") then
      WAssert (T, True, "AR localization not included");
      return;
   end if;
   Check_Script ("Arab", "العربية");
   Check_Script ("Bali", "البالية");
   Check_Script ("Cans", "مقاطع كندية أصلية موحدة");
   Check_Script ("Deva", "الديفاناجاري");
   Check_Script ("Egyd", "الديموطيقية");
   Check_Script ("Geok", "الأبجدية الجورجية - أسومتافرلي و نسخري");
   Check_Script ("Hang", "الهانغول");
   Check_Script ("Inds", "اندس - هارابان");
   Check_Script ("Java", "الجاوية");
   Check_Script ("Kali", "الكياه لى");
   Check_Script ("Lana", "الانا");
   Check_Script ("Mand", "المانداينية");
   Check_Script ("Nkoo", "أنكو");
   Check_Script ("Ogam", "الأوجهام");
   Check_Script ("Perm", "البيرميكية القديمة");
   Check_Script ("Roro", "رنجورنجو");
   Check_Script ("Sara", "الساراتي");
   Check_Script ("Tagb", "التاجبانوا");
   Check_Script ("Ugar", "الأجاريتيكية");
   Check_Script ("Vaii", "الفاي");
   Check_Script ("Xpeo", "الفارسية القديمة");
   Check_Script ("Yiii", "اليي");
   Check_Script ("Zxxx", "غير مكتوب");
end T_0035;
