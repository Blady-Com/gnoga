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
with ZanyBlue.Text.Locales;

separate (ZanyBlue.Test.Text.Times.Suites)
procedure T_0074 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Calendar;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Times;

   Locale    : constant Locale_Type := Make_Locale ("ar");

   procedure Check (Value : Wide_String;
                    T     : Time);

   procedure Check (Value : Wide_String;
                    T     : Time) is
      Arg1      : constant Time_Argument_Type := Create (T);
   begin
      Check_Value (R, Arg1.Format ("time", "short", Locale), Value,
                   """time"" format");
   end Check;

begin
   Check ("٠:٠٠ ص", Time_Of (1904, 6, 16, Duration (0)));
   Check ("٠:٣٠ ص", Time_Of (1904, 6, 16, Duration (1800)));
   Check ("١:٠٠ ص", Time_Of (1904, 6, 16, Duration (3600)));
   Check ("١:٣٠ ص", Time_Of (1904, 6, 16, Duration (5400)));
   Check ("٢:٠٠ ص", Time_Of (1904, 6, 16, Duration (7200)));
   Check ("٢:٣٠ ص", Time_Of (1904, 6, 16, Duration (9000)));
   Check ("٣:٠٠ ص", Time_Of (1904, 6, 16, Duration (10800)));
   Check ("٣:٣٠ ص", Time_Of (1904, 6, 16, Duration (12600)));
   Check ("٤:٠٠ ص", Time_Of (1904, 6, 16, Duration (14400)));
   Check ("٤:٣٠ ص", Time_Of (1904, 6, 16, Duration (16200)));
   Check ("٥:٠٠ ص", Time_Of (1904, 6, 16, Duration (18000)));
   Check ("٥:٣٠ ص", Time_Of (1904, 6, 16, Duration (19800)));
   Check ("٦:٠٠ ص", Time_Of (1904, 6, 16, Duration (21600)));
   Check ("٦:٣٠ ص", Time_Of (1904, 6, 16, Duration (23400)));
   Check ("٧:٠٠ ص", Time_Of (1904, 6, 16, Duration (25200)));
   Check ("٧:٣٠ ص", Time_Of (1904, 6, 16, Duration (27000)));
   Check ("٨:٠٠ ص", Time_Of (1904, 6, 16, Duration (28800)));
   Check ("٨:٣٠ ص", Time_Of (1904, 6, 16, Duration (30600)));
   Check ("٩:٠٠ ص", Time_Of (1904, 6, 16, Duration (32400)));
   Check ("٩:٣٠ ص", Time_Of (1904, 6, 16, Duration (34200)));
   Check ("١٠:٠٠ ص", Time_Of (1904, 6, 16, Duration (36000)));
   Check ("١٠:٣٠ ص", Time_Of (1904, 6, 16, Duration (37800)));
   Check ("١١:٠٠ ص", Time_Of (1904, 6, 16, Duration (39600)));
   Check ("١١:٣٠ ص", Time_Of (1904, 6, 16, Duration (41400)));
   Check ("١٢:٠٠ م", Time_Of (1904, 6, 16, Duration (43200)));
   Check ("١٢:٣٠ م", Time_Of (1904, 6, 16, Duration (45000)));
   Check ("١:٠٠ م", Time_Of (1904, 6, 16, Duration (46800)));
   Check ("١:٣٠ م", Time_Of (1904, 6, 16, Duration (48600)));
   Check ("٢:٠٠ م", Time_Of (1904, 6, 16, Duration (50400)));
   Check ("٢:٣٠ م", Time_Of (1904, 6, 16, Duration (52200)));
   Check ("٣:٠٠ م", Time_Of (1904, 6, 16, Duration (54000)));
   Check ("٣:٣٠ م", Time_Of (1904, 6, 16, Duration (55800)));
   Check ("٤:٠٠ م", Time_Of (1904, 6, 16, Duration (57600)));
   Check ("٤:٣٠ م", Time_Of (1904, 6, 16, Duration (59400)));
   Check ("٥:٠٠ م", Time_Of (1904, 6, 16, Duration (61200)));
   Check ("٥:٣٠ م", Time_Of (1904, 6, 16, Duration (63000)));
   Check ("٦:٠٠ م", Time_Of (1904, 6, 16, Duration (64800)));
   Check ("٦:٣٠ م", Time_Of (1904, 6, 16, Duration (66600)));
   Check ("٧:٠٠ م", Time_Of (1904, 6, 16, Duration (68400)));
   Check ("٧:٣٠ م", Time_Of (1904, 6, 16, Duration (70200)));
   Check ("٨:٠٠ م", Time_Of (1904, 6, 16, Duration (72000)));
   Check ("٨:٣٠ م", Time_Of (1904, 6, 16, Duration (73800)));
   Check ("٩:٠٠ م", Time_Of (1904, 6, 16, Duration (75600)));
   Check ("٩:٣٠ م", Time_Of (1904, 6, 16, Duration (77400)));
   Check ("١٠:٠٠ م", Time_Of (1904, 6, 16, Duration (79200)));
   Check ("١٠:٣٠ م", Time_Of (1904, 6, 16, Duration (81000)));
   Check ("١١:٠٠ م", Time_Of (1904, 6, 16, Duration (82800)));
   Check ("١١:٣٠ م", Time_Of (1904, 6, 16, Duration (84600)));
end T_0074;
