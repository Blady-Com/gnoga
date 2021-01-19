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
procedure T_0039 (T : in out Test_Case'Class) is

   fr : constant Locale_Type := Make_Locale ("fr");

   procedure Check_Territory (Abbreviation : String;
                              Value        : String);

   procedure Check_Territory (Abbreviation : String;
                              Value        : String) is
   begin
      Check_Value (T, Territory_Name (Abbreviation, Locale => fr), Value,
                      "Mis-match for " & Abbreviation);
   end Check_Territory;

begin
   if not Is_Locale_Defined ("fr", "", "") then
      WAssert (T, True, "FR localization not included");
      return;
   end if;
   Check_Territory ("001", "Monde");
   Check_Territory ("AC", "Île de l’Ascension");
   Check_Territory ("BB", "Barbade");
   Check_Territory ("CA", "Canada");
   Check_Territory ("DE", "Allemagne");
   Check_Territory ("EA", "Ceuta et Melilla");
   Check_Territory ("FI", "Finlande");
   Check_Territory ("GA", "Gabon");
   Check_Territory ("HK", "R.A.S. chinoise de Hong Kong");
   Check_Territory ("IC", "Îles Canaries");
   Check_Territory ("JE", "Jersey");
   Check_Territory ("KE", "Kenya");
   Check_Territory ("LA", "Laos");
   Check_Territory ("MA", "Maroc");
   Check_Territory ("NA", "Namibie");
   Check_Territory ("OM", "Oman");
   Check_Territory ("PA", "Panama");
   Check_Territory ("QA", "Qatar");
   Check_Territory ("RE", "La Réunion");
   Check_Territory ("SA", "Arabie saoudite");
   Check_Territory ("TA", "Tristan da Cunha");
   Check_Territory ("UA", "Ukraine");
   Check_Territory ("VA", "État de la Cité du Vatican");
   Check_Territory ("WF", "Wallis-et-Futuna");
   Check_Territory ("YE", "Yémen");
   Check_Territory ("ZA", "Afrique du Sud");
end T_0039;
