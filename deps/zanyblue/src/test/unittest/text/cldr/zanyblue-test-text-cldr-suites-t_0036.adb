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
procedure T_0036 (T : in out Test_Case'Class) is

   he : constant Locale_Type := Make_Locale ("he");

   procedure Check_Script (Abbreviation : String;
                           Value        : String);

   procedure Check_Script (Abbreviation : String;
                           Value        : String) is
   begin
      Check_Value (T, Script_Name (Abbreviation, Locale => he), Value,
                      "Expected: " & Value);
   end Check_Script;

begin
   if not Is_Locale_Defined ("he", "", "") then
      WAssert (T, True, "HE localization not included");
      return;
   end if;
   Check_Script ("Arab", "????????");
   Check_Script ("Bali", "??????????????");
   Check_Script ("Cher", "??????????????");
   Check_Script ("Deva", "????????????????");
   Check_Script ("Egyp", "?????? ??????????????");
   Check_Script ("Geor", "????????????");
   Check_Script ("Hang", "????????????");
   Check_Script ("Inds", "????????????");
   Check_Script ("Jpan", "????????");
   Check_Script ("Knda", "??????????");
   Check_Script ("Laoo", "????????");
   Check_Script ("Mong", "??????????????");
   Check_Script ("Orya", "????????????");
   Check_Script ("Phnx", "????????????");
   Check_Script ("Runr", "????????");
   Check_Script ("Sinh", "????????????");
   Check_Script ("Taml", "????????");
   Check_Script ("Ugar", "??????????????");
   Check_Script ("Xpeo", "???????? ????????");
   Check_Script ("Zxxx", "???? ????????");
end T_0036;
