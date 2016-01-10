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
procedure T_0041 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   he : constant Locale_Type := Make_Locale ("he");

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String);

   procedure Check_Territory (Abbreviation : Wide_String;
                              Value        : Wide_String) is
   begin
      Check_Value (R, Territory_Name (Abbreviation, Locale => he), Value,
                      "Expected: " & Value);
   end Check_Territory;

begin
   if not Is_Locale_Defined ("he", "", "") then
      WAssert (R, True, "HE localization not included");
      return;
   end if;
   Check_Territory ("001", "העולם");
   Check_Territory ("AC", "האי אסנשן");
   Check_Territory ("BA", "בוסניה והרצגובינה");
   Check_Territory ("CA", "קנדה");
   Check_Territory ("DE", "גרמניה");
   Check_Territory ("EA", "סאוטה ומלייה");
   Check_Territory ("FI", "פינלנד");
   Check_Territory ("GA", "גאבון");
   Check_Territory ("HK", "הונג קונג - מחוז מנהלי מיוחד של סין");
   Check_Territory ("IC", "האיים הקנריים");
   Check_Territory ("JE", "ג׳רסי");
   Check_Territory ("KE", "קניה");
   Check_Territory ("LA", "לאוס");
   Check_Territory ("MA", "מרוקו");
   Check_Territory ("NA", "נמיביה");
   Check_Territory ("OM", "עומאן");
   Check_Territory ("PA", "פנמה");
   Check_Territory ("QA", "קטאר");
   Check_Territory ("RE", "ראוניון");
   Check_Territory ("SA", "ערב הסעודית");
   Check_Territory ("TA", "טריסטן דה קונה");
   Check_Territory ("UA", "אוקראינה");
   Check_Territory ("VA", "הוותיקן");
   Check_Territory ("WF", "איי ווליס ופוטונה");
   Check_Territory ("YE", "תימן");
   Check_Territory ("ZA", "דרום אפריקה");
end T_0041;
