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

with Ada.Calendar;
with ZanyBlue.Text.Locales;

separate (ZanyBlue.Test.Text.Times.Suites)
procedure T_0071 (T : in out Test_Case'Class) is

   use Ada.Calendar;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Times;

   Locale    : constant Locale_Type := Make_Locale ("fr");

   procedure Check (Value : Wide_String;
                    T_Val : Time);

   procedure Check (Value : Wide_String;
                    T_Val : Time) is
      Arg1      : constant Time_Argument_Type := Create (T_Val);
   begin
      Check_Value (T, Arg1.Format ("time", "short", Locale), Value,
                   """time"" format");
   end Check;

begin
   Check ("00:00", Time_Of (1904, 6, 16, Duration (0)));
   Check ("00:30", Time_Of (1904, 6, 16, Duration (1800)));
   Check ("01:00", Time_Of (1904, 6, 16, Duration (3600)));
   Check ("01:30", Time_Of (1904, 6, 16, Duration (5400)));
   Check ("02:00", Time_Of (1904, 6, 16, Duration (7200)));
   Check ("02:30", Time_Of (1904, 6, 16, Duration (9000)));
   Check ("03:00", Time_Of (1904, 6, 16, Duration (10800)));
   Check ("03:30", Time_Of (1904, 6, 16, Duration (12600)));
   Check ("04:00", Time_Of (1904, 6, 16, Duration (14400)));
   Check ("04:30", Time_Of (1904, 6, 16, Duration (16200)));
   Check ("05:00", Time_Of (1904, 6, 16, Duration (18000)));
   Check ("05:30", Time_Of (1904, 6, 16, Duration (19800)));
   Check ("06:00", Time_Of (1904, 6, 16, Duration (21600)));
   Check ("06:30", Time_Of (1904, 6, 16, Duration (23400)));
   Check ("07:00", Time_Of (1904, 6, 16, Duration (25200)));
   Check ("07:30", Time_Of (1904, 6, 16, Duration (27000)));
   Check ("08:00", Time_Of (1904, 6, 16, Duration (28800)));
   Check ("08:30", Time_Of (1904, 6, 16, Duration (30600)));
   Check ("09:00", Time_Of (1904, 6, 16, Duration (32400)));
   Check ("09:30", Time_Of (1904, 6, 16, Duration (34200)));
   Check ("10:00", Time_Of (1904, 6, 16, Duration (36000)));
   Check ("10:30", Time_Of (1904, 6, 16, Duration (37800)));
   Check ("11:00", Time_Of (1904, 6, 16, Duration (39600)));
   Check ("11:30", Time_Of (1904, 6, 16, Duration (41400)));
   Check ("12:00", Time_Of (1904, 6, 16, Duration (43200)));
   Check ("12:30", Time_Of (1904, 6, 16, Duration (45000)));
   Check ("13:00", Time_Of (1904, 6, 16, Duration (46800)));
   Check ("13:30", Time_Of (1904, 6, 16, Duration (48600)));
   Check ("14:00", Time_Of (1904, 6, 16, Duration (50400)));
   Check ("14:30", Time_Of (1904, 6, 16, Duration (52200)));
   Check ("15:00", Time_Of (1904, 6, 16, Duration (54000)));
   Check ("15:30", Time_Of (1904, 6, 16, Duration (55800)));
   Check ("16:00", Time_Of (1904, 6, 16, Duration (57600)));
   Check ("16:30", Time_Of (1904, 6, 16, Duration (59400)));
   Check ("17:00", Time_Of (1904, 6, 16, Duration (61200)));
   Check ("17:30", Time_Of (1904, 6, 16, Duration (63000)));
   Check ("18:00", Time_Of (1904, 6, 16, Duration (64800)));
   Check ("18:30", Time_Of (1904, 6, 16, Duration (66600)));
   Check ("19:00", Time_Of (1904, 6, 16, Duration (68400)));
   Check ("19:30", Time_Of (1904, 6, 16, Duration (70200)));
   Check ("20:00", Time_Of (1904, 6, 16, Duration (72000)));
   Check ("20:30", Time_Of (1904, 6, 16, Duration (73800)));
   Check ("21:00", Time_Of (1904, 6, 16, Duration (75600)));
   Check ("21:30", Time_Of (1904, 6, 16, Duration (77400)));
   Check ("22:00", Time_Of (1904, 6, 16, Duration (79200)));
   Check ("22:30", Time_Of (1904, 6, 16, Duration (81000)));
   Check ("23:00", Time_Of (1904, 6, 16, Duration (82800)));
   Check ("23:30", Time_Of (1904, 6, 16, Duration (84600)));
end T_0071;
