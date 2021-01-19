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
procedure T_0029 (T : in out Test_Case'Class) is

   fr : constant Locale_Type := Make_Locale ("fr");

   procedure Check_Language (Abbreviation : String;
                             Value        : String);

   procedure Check_Language (Abbreviation : String;
                             Value        : String) is
   begin
      Check_Value (T, Language_Name (Abbreviation, Locale => fr), Value,
                      "Mis-match for " & Abbreviation);
   end Check_Language;

begin
   if not Is_Locale_Defined ("fr", "", "") then
      WAssert (T, True, "FR localization not included");
      return;
   end if;
   Check_Language ("aa", "afar");
   Check_Language ("ba", "bachkir");
   Check_Language ("ca", "catalan");
   Check_Language ("da", "danois");
   Check_Language ("ee", "éwé");
   Check_Language ("fa", "persan");
   Check_Language ("ga", "irlandais");
   Check_Language ("ha", "haoussa");
   Check_Language ("ia", "interlingua");
   Check_Language ("ja", "japonais");
   Check_Language ("ka", "géorgien");
   Check_Language ("ky", "kirghize");
   Check_Language ("mad", "madurais");
   Check_Language ("na", "nauruan");
   Check_Language ("oc", "occitan");
   Check_Language ("pa", "pendjabi");
   Check_Language ("qu", "quechua");
   Check_Language ("raj", "rajasthani");
   Check_Language ("sa", "sanskrit");
   Check_Language ("ta", "tamoul");
   Check_Language ("udm", "oudmourte");
   Check_Language ("vai", "vaï");
   Check_Language ("wa", "wallon");
   Check_Language ("xal", "kalmouk");
   Check_Language ("yao", "yao");
   Check_Language ("za", "zhuang");
end T_0029;
