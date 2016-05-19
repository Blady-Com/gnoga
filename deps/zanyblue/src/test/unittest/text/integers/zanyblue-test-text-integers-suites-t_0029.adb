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

separate (ZanyBlue.Test.Text.Integers.Suites)
procedure T_0029 (T : in out Test_Case'Class) is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Integers;
   use ZanyBlue.Text.Arguments;

   Locale   : constant Locale_Type := Make_Locale ("ar");
   Arg0     : constant Integer_Argument_Type := Create (0);
   Arg1     : constant Integer_Argument_Type := Create (2009);
   Arg2     : constant Integer_Argument_Type := Create (-2009);
   Arg3     : constant Integer_Argument_Type := Create (2 ** 31 - 1);
   Arg4     : constant Integer_Argument_Type := Create (-(2 ** 31 - 1));
   Args     : Argument_List;

begin
   Args.Append (Arg0);
   Args.Append (Arg1);
   Args.Append (Arg2);
   Args.Append (Arg3);
   Args.Append (Arg4);
   Check_Value (T, Args.Format (0, "", "integer,b", Locale, False),
                   "٠");
   Check_Value (T, Args.Format (1, "", "integer,b", Locale, False),
                   "١١١١١٠١١٠٠١");
   Check_Value (T, Args.Format (2, "", "integer,b", Locale, False),
                   "‎-١١١١١٠١١٠٠١");
   Check_Value (T, Args.Format (3, "", "integer,b", Locale, False),
                   "١١١١١١١١١١١١١١١١١١١١١١١١١١١١١١١");
   Check_Value (T, Args.Format (4, "", "integer,b", Locale, False),
                   "‎-١١١١١١١١١١١١١١١١١١١١١١١١١١١١١١١");
   Check_Value (T, Args.Format (0, "", "integer,b*", Locale, False),
                   "0");
   Check_Value (T, Args.Format (1, "", "integer,b*", Locale, False),
                   "11111011001");
   Check_Value (T, Args.Format (2, "", "integer,b*", Locale, False),
                   "-11111011001");
   Check_Value (T, Args.Format (3, "", "integer,b*", Locale, False),
                   "1111111111111111111111111111111");
   Check_Value (T, Args.Format (4, "", "integer,b*", Locale, False),
                   "-1111111111111111111111111111111");
end T_0029;
