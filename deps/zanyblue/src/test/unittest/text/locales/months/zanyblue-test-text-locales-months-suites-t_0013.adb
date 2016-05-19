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

separate (ZanyBlue.Test.Text.Locales.Months.Suites)
procedure T_0013 (T : in out Test_Case'Class) is

   L : constant Locale_Type := Make_Locale ("xx");

begin
   Check_Value (T, Short_Month_Name (L, Jan), "Jan",
                "Fallback short month name, Jan");
   Check_Value (T, Short_Month_Name (L, Feb), "Feb",
                "Fallback short month name, Feb");
   Check_Value (T, Short_Month_Name (L, Mar), "Mar",
                "Fallback short month name, Mar");
   Check_Value (T, Short_Month_Name (L, Apr), "Apr",
                "Fallback short month name, Apr");
   Check_Value (T, Short_Month_Name (L, May), "May",
                "Fallback short month name, May");
   Check_Value (T, Short_Month_Name (L, Jun), "Jun",
                "Fallback short month name, Jun");
   Check_Value (T, Short_Month_Name (L, Jul), "Jul",
                "Fallback short month name, Jul");
   Check_Value (T, Short_Month_Name (L, Aug), "Aug",
                "Fallback short month name, Aug");
   Check_Value (T, Short_Month_Name (L, Sep), "Sep",
                "Fallback short month name, Sep");
   Check_Value (T, Short_Month_Name (L, Oct), "Oct",
                "Fallback short month name, Oct");
   Check_Value (T, Short_Month_Name (L, Nov), "Nov",
                "Fallback short month name, Nov");
   Check_Value (T, Short_Month_Name (L, Dec), "Dec",
                "Fallback short month name, Dec");
end T_0013;
