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
procedure T_0045 (T : in out Test_Case'Class) is

   no : constant Locale_Type := Make_Locale ("no");
   nb : constant Locale_Type := Make_Locale ("nb");

   procedure Check_Script (Abbreviation : String);

   procedure Check_Script (Abbreviation : String) is
      no_Value : constant String := Script_Name (Abbreviation,
                                                      Locale => no);
      nb_Value : constant String := Script_Name (Abbreviation,
                                                      Locale => nb);
   begin
      Check_Value (T, no_Value, nb_Value, "no/nb script names");
   end Check_Script;

begin
   if not Is_Locale_Defined ("no", "", "") then
      WAssert (T, True, "NO localization not included");
      return;
   end if;
   if not Is_Locale_Defined ("nb", "", "") then
      WAssert (T, True, "NB localization not included");
      return;
   end if;
   Check_Script ("Arab");
   Check_Script ("Bali");
   Check_Script ("Cakm");
   Check_Script ("Deva");
   Check_Script ("Egyd");
   Check_Script ("Geok");
   Check_Script ("Hang");
   Check_Script ("Inds");
   Check_Script ("Java");
   Check_Script ("Kana");
   Check_Script ("Lana");
   Check_Script ("Mand");
   Check_Script ("Nkoo");
   Check_Script ("Ogam");
   Check_Script ("Perm");
   Check_Script ("Rjng");
   Check_Script ("Samr");
   Check_Script ("Tagb");
   Check_Script ("Ugar");
   Check_Script ("Vaii");
   Check_Script ("Xpeo");
   Check_Script ("Yiii");
   Check_Script ("Zmth");
end T_0045;
