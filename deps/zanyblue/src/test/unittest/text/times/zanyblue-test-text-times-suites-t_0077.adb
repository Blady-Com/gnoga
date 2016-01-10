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

with Ada.Calendar;
with Ada.Calendar.Time_Zones;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;

separate (ZanyBlue.Test.Text.Times.Suites)
procedure T_0077 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Calendar;
   use Ada.Calendar.Time_Zones;
   use ZanyBlue.Text.Times;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;

   procedure Check (Format : Wide_String;
                    Value  : Wide_String);

   Locale    : constant Locale_Type := Make_Locale ("zh_CN");
   V1        : constant Time := Time_Of (1904, 6, 16, Duration (60483));
   Arg1      : constant Time_Argument_Type := Create (V1, 8 * 60);
   Args      : Argument_List;

   procedure Check (Format : Wide_String;
                    Value  : Wide_String) is

   begin
      Check_Value (R, Args.Format (0, "", Format, Locale, False), Value,
                   "Format """ & Format & """ style");
   end Check;

begin
   Args.Append (Arg1);
   Check ("",        "04/6/16 下午4:48");
   Check ("*",       "6/16/04 4:48 PM");
   Check ("full",    "1904年6月16日星期四 +0800下午4:48:03");
   Check ("full*",   "Thursday, June 16, 1904 4:48:03 PM +0800");
   Check ("long",    "1904年6月16日 +0800下午4:48:03");
   Check ("long*",   "June 16, 1904 4:48:03 PM +0800");
   Check ("medium",  "1904年6月16日 下午4:48:03");
   Check ("medium*", "Jun 16, 1904 4:48:03 PM");
   Check ("short",   "04/6/16 下午4:48");
   Check ("short*",  "6/16/04 4:48 PM");
end T_0077;
