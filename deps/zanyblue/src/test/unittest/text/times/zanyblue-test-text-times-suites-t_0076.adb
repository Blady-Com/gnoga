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
procedure T_0076 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use Ada.Calendar;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Times;
   use ZanyBlue.Text.Locales;

   type Day_Result_Type is
      record
         Day      : Day_Number;
         Month    : Month_Number;
         Year     : Year_Number;
         Day_Name : Day_Type;
      end record;

   Test_Data : constant array (Positive range <>) of Day_Result_Type := (
      --  January
      (1, 1, 1964, Wed), (2, 1, 1964, Thu), (3, 1, 1964, Fri),
      (4, 1, 1964, Sat), (5, 1, 1964, Sun), (6, 1, 1964, Mon),
      (7, 1, 1964, Tue), (8, 1, 1964, Wed), (9, 1, 1964, Thu),
      (10, 1, 1964, Fri), (11, 1, 1964, Sat), (12, 1, 1964, Sun),
      (13, 1, 1964, Mon), (14, 1, 1964, Tue), (15, 1, 1964, Wed),
      (16, 1, 1964, Thu), (17, 1, 1964, Fri), (18, 1, 1964, Sat),
      (19, 1, 1964, Sun), (20, 1, 1964, Mon), (21, 1, 1964, Tue),
      (22, 1, 1964, Wed), (23, 1, 1964, Thu), (24, 1, 1964, Fri),
      (25, 1, 1964, Sat), (26, 1, 1964, Sun), (27, 1, 1964, Mon),
      (28, 1, 1964, Tue), (29, 1, 1964, Wed), (30, 1, 1964, Thu),
      (31, 1, 1964, Fri),
      --  February
      (1, 2, 1964, Sat), (2, 2, 1964, Sun), (3, 2, 1964, Mon),
      (4, 2, 1964, Tue), (5, 2, 1964, Wed), (6, 2, 1964, Thu),
      (7, 2, 1964, Fri), (8, 2, 1964, Sat), (9, 2, 1964, Sun),
      (10, 2, 1964, Mon), (11, 2, 1964, Tue), (12, 2, 1964, Wed),
      (13, 2, 1964, Thu), (14, 2, 1964, Fri), (15, 2, 1964, Sat),
      (16, 2, 1964, Sun), (17, 2, 1964, Mon), (18, 2, 1964, Tue),
      (19, 2, 1964, Wed), (20, 2, 1964, Thu), (21, 2, 1964, Fri),
      (22, 2, 1964, Sat), (23, 2, 1964, Sun), (24, 2, 1964, Mon),
      (25, 2, 1964, Tue), (26, 2, 1964, Wed), (27, 2, 1964, Thu),
      (28, 2, 1964, Fri), (29, 2, 1964, Sat),
      --  March
      (1, 3, 1964, Sun), (2, 3, 1964, Mon), (3, 3, 1964, Tue),
      (4, 3, 1964, Wed), (5, 3, 1964, Thu), (6, 3, 1964, Fri),
      (7, 3, 1964, Sat), (8, 3, 1964, Sun), (9, 3, 1964, Mon),
      (10, 3, 1964, Tue), (11, 3, 1964, Wed), (12, 3, 1964, Thu),
      (13, 3, 1964, Fri), (14, 3, 1964, Sat), (15, 3, 1964, Sun),
      (16, 3, 1964, Mon), (17, 3, 1964, Tue), (18, 3, 1964, Wed),
      (19, 3, 1964, Thu), (20, 3, 1964, Fri), (21, 3, 1964, Sat),
      (22, 3, 1964, Sun), (23, 3, 1964, Mon), (24, 3, 1964, Tue),
      (25, 3, 1964, Wed), (26, 3, 1964, Thu), (27, 3, 1964, Fri),
      (28, 3, 1964, Sat), (29, 3, 1964, Sun), (30, 3, 1964, Mon),
      (31, 3, 1964, Tue),
      --  April
      (1, 4, 1964, Wed), (2, 4, 1964, Thu), (3, 4, 1964, Fri),
      (4, 4, 1964, Sat), (5, 4, 1964, Sun), (6, 4, 1964, Mon),
      (7, 4, 1964, Tue), (8, 4, 1964, Wed), (9, 4, 1964, Thu),
      (10, 4, 1964, Fri), (11, 4, 1964, Sat), (12, 4, 1964, Sun),
      (13, 4, 1964, Mon), (14, 4, 1964, Tue), (15, 4, 1964, Wed),
      (16, 4, 1964, Thu), (17, 4, 1964, Fri), (18, 4, 1964, Sat),
      (19, 4, 1964, Sun), (20, 4, 1964, Mon), (21, 4, 1964, Tue),
      (22, 4, 1964, Wed), (23, 4, 1964, Thu), (24, 4, 1964, Fri),
      (25, 4, 1964, Sat), (26, 4, 1964, Sun), (27, 4, 1964, Mon),
      (28, 4, 1964, Tue), (29, 4, 1964, Wed), (30, 4, 1964, Thu),
      --  May
      (1, 5, 1964, Fri), (2, 5, 1964, Sat), (3, 5, 1964, Sun),
      (4, 5, 1964, Mon), (5, 5, 1964, Tue), (6, 5, 1964, Wed),
      (7, 5, 1964, Thu), (8, 5, 1964, Fri), (9, 5, 1964, Sat),
      (10, 5, 1964, Sun), (11, 5, 1964, Mon), (12, 5, 1964, Tue),
      (13, 5, 1964, Wed), (14, 5, 1964, Thu), (15, 5, 1964, Fri),
      (16, 5, 1964, Sat), (17, 5, 1964, Sun), (18, 5, 1964, Mon),
      (19, 5, 1964, Tue), (20, 5, 1964, Wed), (21, 5, 1964, Thu),
      (22, 5, 1964, Fri), (23, 5, 1964, Sat), (24, 5, 1964, Sun),
      (25, 5, 1964, Mon), (26, 5, 1964, Tue), (27, 5, 1964, Wed),
      (28, 5, 1964, Thu), (29, 5, 1964, Fri), (30, 5, 1964, Sat),
      (31, 5, 1964, Sun),
      --  June
      (1, 6, 1964, Mon), (2, 6, 1964, Tue), (3, 6, 1964, Wed),
      (4, 6, 1964, Thu), (5, 6, 1964, Fri), (6, 6, 1964, Sat),
      (7, 6, 1964, Sun), (8, 6, 1964, Mon), (9, 6, 1964, Tue),
      (10, 6, 1964, Wed), (11, 6, 1964, Thu), (12, 6, 1964, Fri),
      (13, 6, 1964, Sat), (14, 6, 1964, Sun), (15, 6, 1964, Mon),
      (16, 6, 1964, Tue), (17, 6, 1964, Wed), (18, 6, 1964, Thu),
      (19, 6, 1964, Fri), (20, 6, 1964, Sat), (21, 6, 1964, Sun),
      (22, 6, 1964, Mon), (23, 6, 1964, Tue), (24, 6, 1964, Wed),
      (25, 6, 1964, Thu), (26, 6, 1964, Fri), (27, 6, 1964, Sat),
      (28, 6, 1964, Sun), (29, 6, 1964, Mon), (30, 6, 1964, Tue),
      --  July
      (1, 7, 1964, Wed), (2, 7, 1964, Thu), (3, 7, 1964, Fri),
      (4, 7, 1964, Sat), (5, 7, 1964, Sun), (6, 7, 1964, Mon),
      (7, 7, 1964, Tue), (8, 7, 1964, Wed), (9, 7, 1964, Thu),
      (10, 7, 1964, Fri), (11, 7, 1964, Sat), (12, 7, 1964, Sun),
      (13, 7, 1964, Mon), (14, 7, 1964, Tue), (15, 7, 1964, Wed),
      (16, 7, 1964, Thu), (17, 7, 1964, Fri), (18, 7, 1964, Sat),
      (19, 7, 1964, Sun), (20, 7, 1964, Mon), (21, 7, 1964, Tue),
      (22, 7, 1964, Wed), (23, 7, 1964, Thu), (24, 7, 1964, Fri),
      (25, 7, 1964, Sat), (26, 7, 1964, Sun), (27, 7, 1964, Mon),
      (28, 7, 1964, Tue), (29, 7, 1964, Wed), (30, 7, 1964, Thu),
      (31, 7, 1964, Fri),
      --  August
      (1, 8, 1964, Sat), (2, 8, 1964, Sun), (3, 8, 1964, Mon),
      (4, 8, 1964, Tue), (5, 8, 1964, Wed), (6, 8, 1964, Thu),
      (7, 8, 1964, Fri), (8, 8, 1964, Sat), (9, 8, 1964, Sun),
      (10, 8, 1964, Mon), (11, 8, 1964, Tue), (12, 8, 1964, Wed),
      (13, 8, 1964, Thu), (14, 8, 1964, Fri), (15, 8, 1964, Sat),
      (16, 8, 1964, Sun), (17, 8, 1964, Mon), (18, 8, 1964, Tue),
      (19, 8, 1964, Wed), (20, 8, 1964, Thu), (21, 8, 1964, Fri),
      (22, 8, 1964, Sat), (23, 8, 1964, Sun), (24, 8, 1964, Mon),
      (25, 8, 1964, Tue), (26, 8, 1964, Wed), (27, 8, 1964, Thu),
      (28, 8, 1964, Fri), (29, 8, 1964, Sat), (30, 8, 1964, Sun),
      (31, 8, 1964, Mon),
      --  September
      (1, 9, 1964, Tue), (2, 9, 1964, Wed), (3, 9, 1964, Thu),
      (4, 9, 1964, Fri), (5, 9, 1964, Sat), (6, 9, 1964, Sun),
      (7, 9, 1964, Mon), (8, 9, 1964, Tue), (9, 9, 1964, Wed),
      (10, 9, 1964, Thu), (11, 9, 1964, Fri), (12, 9, 1964, Sat),
      (13, 9, 1964, Sun), (14, 9, 1964, Mon), (15, 9, 1964, Tue),
      (16, 9, 1964, Wed), (17, 9, 1964, Thu), (18, 9, 1964, Fri),
      (19, 9, 1964, Sat), (20, 9, 1964, Sun), (21, 9, 1964, Mon),
      (22, 9, 1964, Tue), (23, 9, 1964, Wed), (24, 9, 1964, Thu),
      (25, 9, 1964, Fri), (26, 9, 1964, Sat), (27, 9, 1964, Sun),
      (28, 9, 1964, Mon), (29, 9, 1964, Tue), (30, 9, 1964, Wed),
      --  October
      (1, 10, 1964, Thu), (2, 10, 1964, Fri), (3, 10, 1964, Sat),
      (4, 10, 1964, Sun), (5, 10, 1964, Mon), (6, 10, 1964, Tue),
      (7, 10, 1964, Wed), (8, 10, 1964, Thu), (9, 10, 1964, Fri),
      (10, 10, 1964, Sat), (11, 10, 1964, Sun), (12, 10, 1964, Mon),
      (13, 10, 1964, Tue), (14, 10, 1964, Wed), (15, 10, 1964, Thu),
      (16, 10, 1964, Fri), (17, 10, 1964, Sat), (18, 10, 1964, Sun),
      (19, 10, 1964, Mon), (20, 10, 1964, Tue), (21, 10, 1964, Wed),
      (22, 10, 1964, Thu), (23, 10, 1964, Fri), (24, 10, 1964, Sat),
      (25, 10, 1964, Sun), (26, 10, 1964, Mon), (27, 10, 1964, Tue),
      (28, 10, 1964, Wed), (29, 10, 1964, Thu), (30, 10, 1964, Fri),
      (31, 10, 1964, Sat),
      --  November
      (1, 11, 1964, Sun), (2, 11, 1964, Mon), (3, 11, 1964, Tue),
      (4, 11, 1964, Wed), (5, 11, 1964, Thu), (6, 11, 1964, Fri),
      (7, 11, 1964, Sat), (8, 11, 1964, Sun), (9, 11, 1964, Mon),
      (10, 11, 1964, Tue), (11, 11, 1964, Wed), (12, 11, 1964, Thu),
      (13, 11, 1964, Fri), (14, 11, 1964, Sat), (15, 11, 1964, Sun),
      (16, 11, 1964, Mon), (17, 11, 1964, Tue), (18, 11, 1964, Wed),
      (19, 11, 1964, Thu), (20, 11, 1964, Fri), (21, 11, 1964, Sat),
      (22, 11, 1964, Sun), (23, 11, 1964, Mon), (24, 11, 1964, Tue),
      (25, 11, 1964, Wed), (26, 11, 1964, Thu), (27, 11, 1964, Fri),
      (28, 11, 1964, Sat), (29, 11, 1964, Sun), (30, 11, 1964, Mon),
      --  December
      (1, 12, 1964, Tue), (2, 12, 1964, Wed), (3, 12, 1964, Thu),
      (4, 12, 1964, Fri), (5, 12, 1964, Sat), (6, 12, 1964, Sun),
      (7, 12, 1964, Mon), (8, 12, 1964, Tue), (9, 12, 1964, Wed),
      (10, 12, 1964, Thu), (11, 12, 1964, Fri), (12, 12, 1964, Sat),
      (13, 12, 1964, Sun), (14, 12, 1964, Mon), (15, 12, 1964, Tue),
      (16, 12, 1964, Wed), (17, 12, 1964, Thu), (18, 12, 1964, Fri),
      (19, 12, 1964, Sat), (20, 12, 1964, Sun), (21, 12, 1964, Mon),
      (22, 12, 1964, Tue), (23, 12, 1964, Wed), (24, 12, 1964, Thu),
      (25, 12, 1964, Fri), (26, 12, 1964, Sat), (27, 12, 1964, Sun),
      (28, 12, 1964, Mon), (29, 12, 1964, Tue), (30, 12, 1964, Wed),
      (31, 12, 1964, Thu));

   procedure Check_Day (Data : Day_Result_Type);

   procedure Check_Day (Data : Day_Result_Type) is
      Day_Name : constant Day_Type := Day_In_Week (Data.Day,
                                                   Data.Month,
                                                   Data.Year);
   begin
      WAssert (R, Day_Name = Data.Day_Name,
                 "Incorrect day calcaulated: "
                 & Day_Number'Wide_Image (Data.Day)
                 & "-"
                 & Month_Number'Wide_Image (Data.Month)
                 & "-"
                 & Year_Number'Wide_Image (Data.Year)
                 & ": "
                 & Day_Type'Wide_Image (Day_Name)
                 & " /= "
                 & Day_Type'Wide_Image (Data.Day_Name));
   end Check_Day;

begin
   for I in Test_Data'Range loop
      Check_Day (Test_Data (I));
   end loop;
end T_0076;
