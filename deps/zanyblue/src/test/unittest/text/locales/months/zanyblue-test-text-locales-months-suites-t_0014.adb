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

separate (ZanyBlue.Test.Text.Locales.Months.Suites)
procedure T_0014 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L : constant Locale_Type := Make_Locale ("xx");

begin
   Check_Value (R, Full_Month_Name (L, Jan), "January",
                "Fallback full month name, Jan");
   Check_Value (R, Full_Month_Name (L, Feb), "February",
                "Fallback full month name, Feb");
   Check_Value (R, Full_Month_Name (L, Mar), "March",
                "Fallback full month name, Mar");
   Check_Value (R, Full_Month_Name (L, Apr), "April",
                "Fallback full month name, Apr");
   Check_Value (R, Full_Month_Name (L, May), "May",
                "Fallback full month name, May");
   Check_Value (R, Full_Month_Name (L, Jun), "June",
                "Fallback full month name, Jun");
   Check_Value (R, Full_Month_Name (L, Jul), "July",
                "Fallback full month name, Jul");
   Check_Value (R, Full_Month_Name (L, Aug), "August",
                "Fallback full month name, Aug");
   Check_Value (R, Full_Month_Name (L, Sep), "September",
                "Fallback full month name, Sep");
   Check_Value (R, Full_Month_Name (L, Oct), "October",
                "Fallback full month name, Oct");
   Check_Value (R, Full_Month_Name (L, Nov), "November",
                "Fallback full month name, Nov");
   Check_Value (R, Full_Month_Name (L, Dec), "December",
                "Fallback full month name, Dec");
end T_0014;
