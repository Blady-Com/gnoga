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

separate (ZanyBlue.Test.Text.Floats.Suites)
procedure T_0006 (T : in out Test_Case'Class) is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;

   Locale   : constant Locale_Type := Make_Locale ("ar");
   V1       : constant Float := 55.0;
   V2       : constant Float := 66.1;
   V3       : constant Float := -55.0;
   V4       : constant Float := -66.1;
   List      : Argument_List;

begin
   Append (List, +V1);
   Append (List, +V2);
   Append (List, +V3);
   Append (List, +V4);
   Check_Value (T, List.Format (0, "", "", Locale, False), "٥.٥٠٠٠٠E‎+٠١");
   Check_Value (T, List.Format (1, "", "", Locale, False), "٦.٦١٠٠٠E‎+٠١");
   Check_Value (T, List.Format (2, "", "", Locale, False), "‎-٥.٥٠٠٠٠E‎+٠١");
   Check_Value (T, List.Format (3, "", "", Locale, False), "‎-٦.٦١٠٠٠E‎+٠١");
end T_0006;
