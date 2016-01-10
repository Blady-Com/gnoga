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

separate (ZanyBlue.Test.Text.CLDR.Suites)
procedure T_0034 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   fr : constant Locale_Type := Make_Locale ("fr");

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String);

   procedure Check_Script (Abbreviation : Wide_String;
                           Value        : Wide_String) is
   begin
      Check_Value (R, Script_Name (Abbreviation, Locale => fr), Value,
                      "Expected: " & Value);
   end Check_Script;

begin
   if not Is_Locale_Defined ("fr", "", "") then
      WAssert (R, True, "FR localization not included");
      return;
   end if;
   Check_Script ("Arab", "arabe");
   Check_Script ("Bali", "balinais");
   Check_Script ("Cakm", "chakma");
   Check_Script ("Deva", "dévanâgarî");
   Check_Script ("Egyd", "démotique égyptien");
   Check_Script ("Geok", "géorgien khoutsouri");
   Check_Script ("Hang", "hangûl");
   Check_Script ("Inds", "indus");
   Check_Script ("Java", "javanais");
   Check_Script ("Kana", "katakana");
   Check_Script ("Lana", "lanna");
   Check_Script ("Mand", "mandéen");
   Check_Script ("Nkoo", "n’ko");
   Check_Script ("Ogam", "ogam");
   Check_Script ("Perm", "ancien permien");
   Check_Script ("Rjng", "rejang");
   Check_Script ("Samr", "samaritain");
   Check_Script ("Tagb", "tagbanoua");
   Check_Script ("Ugar", "ougaritique");
   Check_Script ("Vaii", "vaï");
   Check_Script ("Xpeo", "cunéiforme persépolitain");
   Check_Script ("Yiii", "yi");
   Check_Script ("Zmth", "notation mathématique");
end T_0034;
