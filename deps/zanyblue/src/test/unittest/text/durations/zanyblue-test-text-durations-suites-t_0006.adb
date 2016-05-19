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

with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;

separate (ZanyBlue.Test.Text.Durations.Suites)
procedure T_0006 (T : in out Test_Case'Class) is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Durations;
   use ZanyBlue.Text.Arguments;

   V1        : constant Duration := Duration (0);
   V2        : constant Duration := Duration (60 + 1);
   V3        : constant Duration := Duration (3600 + 60 + 1);
   V4        : constant Duration := Duration (3600 * 25 + 60 + 1);
   List      : Argument_List;

begin
   Append (List, +V1);
   Append (List, +V2);
   Append (List, +V3);
   Append (List, +V4);
   Check_Value (T, List.Format (0, "", "", Root_Locale, False),
                "0:00:00.000");
   Check_Value (T, List.Format (1, "", "", Root_Locale, False),
                "0:01:01.000");
   Check_Value (T, List.Format (2, "", "", Root_Locale, False),
                "1:01:01.000");
   Check_Value (T, List.Format (3, "", "", Root_Locale, False),
                "1 1:01:01.000");
end T_0006;
