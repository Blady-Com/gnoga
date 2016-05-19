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

separate (ZanyBlue.Test.Text.Locales.Day_Periods.Suites)
procedure T_0017 (T : in out Test_Case'Class) is

   L : constant Locale_Type := Make_Locale ("zh");

   procedure Check_Period_For_Time (Hour   : Hour_Type;
                                    Minute : Minute_Type;
                                    Period : Day_Period_Type);

   procedure Check_Period_For_Time (Hour   : Hour_Type;
                                    Minute : Minute_Type;
                                    Period : Day_Period_Type) is
      Val : constant Day_Period_Type := Day_Period_For_Time (L,
                                                             Hour, Minute, 0);
   begin
      WAssert (T, Val = Period,
               Locale_Name (L)
               & Hour_Type'Wide_Image (Hour)
               & "h "
               & Minute_Type'Wide_Image (Minute)
               & "m failed "
               & Day_Period_Type'Wide_Image (Val)
               & " /= "
               & Day_Period_Type'Wide_Image (Period));
   end Check_Period_For_Time;

begin
   Check_Period_For_Time (0,  0, Wee_Hours);
   Check_Period_For_Time (0, 30, Wee_Hours);
   Check_Period_For_Time (1,  0, Wee_Hours);
   Check_Period_For_Time (1, 30, Wee_Hours);
   Check_Period_For_Time (2,  0, Wee_Hours);
   Check_Period_For_Time (2, 30, Wee_Hours);
   Check_Period_For_Time (3,  0, Wee_Hours);
   Check_Period_For_Time (3, 30, Wee_Hours);
   Check_Period_For_Time (4,  0, Early_Morning);
   Check_Period_For_Time (4, 30, Early_Morning);
   Check_Period_For_Time (5,  0, Early_Morning);
   Check_Period_For_Time (5, 30, Early_Morning);
   Check_Period_For_Time (6,  0, Morning);
   Check_Period_For_Time (6, 30, Morning);
   Check_Period_For_Time (7,  0, Morning);
   Check_Period_For_Time (7, 30, Morning);
   Check_Period_For_Time (8,  0, Morning);
   Check_Period_For_Time (8, 30, Morning);
   Check_Period_For_Time (9,  0, Morning);
   Check_Period_For_Time (9, 30, Morning);
   Check_Period_For_Time (10,  0, Morning);
   Check_Period_For_Time (10, 30, Morning);
   Check_Period_For_Time (11,  0, Morning);
   Check_Period_For_Time (11, 30, Morning);
   Check_Period_For_Time (12,  0, Midday);
   Check_Period_For_Time (12, 30, Midday);
   Check_Period_For_Time (13,  0, Afternoon);
   Check_Period_For_Time (13, 30, Afternoon);
   Check_Period_For_Time (14,  0, Afternoon);
   Check_Period_For_Time (14, 30, Afternoon);
   Check_Period_For_Time (15,  0, Afternoon);
   Check_Period_For_Time (15, 30, Afternoon);
   Check_Period_For_Time (16,  0, Afternoon);
   Check_Period_For_Time (16, 30, Afternoon);
   Check_Period_For_Time (17,  0, Afternoon);
   Check_Period_For_Time (17, 30, Afternoon);
   Check_Period_For_Time (18,  0, Night);
   Check_Period_For_Time (18, 30, Night);
   Check_Period_For_Time (19,  0, Night);
   Check_Period_For_Time (19, 30, Night);
   Check_Period_For_Time (20,  0, Night);
   Check_Period_For_Time (20, 30, Night);
   Check_Period_For_Time (21,  0, Night);
   Check_Period_For_Time (21, 30, Night);
   Check_Period_For_Time (22,  0, Night);
   Check_Period_For_Time (22, 30, Night);
   Check_Period_For_Time (23,  0, Night);
   Check_Period_For_Time (23, 30, Night);
end T_0017;
