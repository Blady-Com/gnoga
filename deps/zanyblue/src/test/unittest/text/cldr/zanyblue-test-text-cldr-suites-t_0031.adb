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
procedure T_0031 (T : in out Test_Case'Class) is

   he : constant Locale_Type := Make_Locale ("he");

   procedure Check_Language (Abbreviation : String;
                             Value        : String);

   procedure Check_Language (Abbreviation : String;
                             Value        : String) is
   begin
      Check_Value (T, Language_Name (Abbreviation, Locale => he), Value,
                      "Mis-match for " & Abbreviation);
   end Check_Language;

begin
   if not Is_Locale_Defined ("he", "", "") then
      WAssert (T, True, "HE localization not included");
      return;
   end if;
   Check_Language ("aa", "אפארית");
   Check_Language ("ba", "בשקירית");
   Check_Language ("ca", "קטלאנית");
   Check_Language ("da", "דנית");
   Check_Language ("egy", "מצרית עתיקה");
   Check_Language ("fa", "פרסית");
   Check_Language ("ga", "אירית");
   Check_Language ("ha", "האוסה");
   Check_Language ("ia", "‏אינטרלינגואה");
   Check_Language ("ja", "יפנית");
   Check_Language ("ka", "גאורגית");
   Check_Language ("la", "לטינית");
   Check_Language ("mag", "מאגאהית");
   Check_Language ("na", "נאורית");
   Check_Language ("oc", "אוקסיטנית");
   Check_Language ("pa", "פנג׳אבית");
   Check_Language ("raj", "ראג׳סטן");
   Check_Language ("sa", "סנסקריט");
   Check_Language ("ta", "טמילית");
   Check_Language ("ug", "אויגהור");
   Check_Language ("ve", "וונדה");
   Check_Language ("xh", "קסוסה");
   Check_Language ("yap", "יאפזית");
   Check_Language ("zh", "סינית");
end T_0031;
