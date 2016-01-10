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
procedure T_0011 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L : constant Locale_Type := Make_Locale ("zh");

begin
   Check_Value (R, Full_Month_Name (L, Jan), "一月",
                "zh (Hans) full month name, Jan");
   Check_Value (R, Full_Month_Name (L, Feb), "二月",
                "zh (Hans) full month name, Feb");
   Check_Value (R, Full_Month_Name (L, Mar), "三月",
                "zh (Hans) full month name, Mar");
   Check_Value (R, Full_Month_Name (L, Apr), "四月",
                "zh (Hans) full month name, Apr");
   Check_Value (R, Full_Month_Name (L, May), "五月",
                "zh (Hans) full month name, May");
   Check_Value (R, Full_Month_Name (L, Jun), "六月",
                "zh (Hans) full month name, Jun");
   Check_Value (R, Full_Month_Name (L, Jul), "七月",
                "zh (Hans) full month name, Jul");
   Check_Value (R, Full_Month_Name (L, Aug), "八月",
                "zh (Hans) full month name, Aug");
   Check_Value (R, Full_Month_Name (L, Sep), "九月",
                "zh (Hans) full month name, Sep");
   Check_Value (R, Full_Month_Name (L, Oct), "十月",
                "zh (Hans) full month name, Oct");
   Check_Value (R, Full_Month_Name (L, Nov), "十一月",
                "zh (Hans) full month name, Nov");
   Check_Value (R, Full_Month_Name (L, Dec), "十二月",
                "zh (Hans) full month name, Dec");
end T_0011;
