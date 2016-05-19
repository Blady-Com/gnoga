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
with ZanyBlue.Test.Text.Locales.Currency_Formats.Suites;
with ZanyBlue.Test.Text.Locales.Day_Periods.Suites;
with ZanyBlue.Test.Text.Locales.Days.Suites;
with ZanyBlue.Test.Text.Locales.Months.Suites;
with ZanyBlue.Test.Text.Locales.Date_Formats.Suites;
with ZanyBlue.Test.Text.Locales.Date_Time_Formats.Suites;
with ZanyBlue.Test.Text.Locales.Decimal_Digits.Suites;
with ZanyBlue.Test.Text.Locales.Decimal_Formats.Suites;
with ZanyBlue.Test.Text.Locales.Decimal_Point_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Digit_Pattern_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Eras.Suites;
with ZanyBlue.Test.Text.Locales.Exponent_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Group_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Infinity_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Layout.Suites;
with ZanyBlue.Test.Text.Locales.List_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Localized_Digits.Suites;
with ZanyBlue.Test.Text.Locales.Minus_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Nan_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Percent_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Percent_Formats.Suites;
with ZanyBlue.Test.Text.Locales.Permille_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Plus_Characters.Suites;
with ZanyBlue.Test.Text.Locales.Scientific_Formats.Suites;
with ZanyBlue.Test.Text.Locales.Time_Formats.Suites;
with ZanyBlue.Test.Text.Locales.Zero_Characters.Suites;

package body ZanyBlue.Test.Text.Locales.Suites is

   use Ahven.Framework;
   use ZanyBlue.Text.Locales;

   procedure T_0001 (T : in out Test_Case'Class);
   procedure T_0002 (T : in out Test_Case'Class);
   procedure T_0003 (T : in out Test_Case'Class);
   procedure T_0004 (T : in out Test_Case'Class);
   procedure T_0005 (T : in out Test_Case'Class);
   procedure T_0006 (T : in out Test_Case'Class);
   procedure T_0007 (T : in out Test_Case'Class);
   procedure T_0008 (T : in out Test_Case'Class);
   procedure T_0009 (T : in out Test_Case'Class);
   procedure T_0010 (T : in out Test_Case'Class);
   procedure T_0011 (T : in out Test_Case'Class);
   procedure T_0012 (T : in out Test_Case'Class);
   procedure T_0013 (T : in out Test_Case'Class);
   procedure T_0014 (T : in out Test_Case'Class);
   procedure T_0015 (T : in out Test_Case'Class);
   procedure T_0016 (T : in out Test_Case'Class);
   procedure T_0017 (T : in out Test_Case'Class);
   procedure T_0018 (T : in out Test_Case'Class);
   procedure T_0019 (T : in out Test_Case'Class);
   procedure T_0020 (T : in out Test_Case'Class);
   procedure T_0021 (T : in out Test_Case'Class);
   procedure T_0022 (T : in out Test_Case'Class);
   procedure T_0023 (T : in out Test_Case'Class);
   procedure T_0024 (T : in out Test_Case'Class);
   procedure T_0025 (T : in out Test_Case'Class);
   procedure T_0026 (T : in out Test_Case'Class);
   procedure T_0027 (T : in out Test_Case'Class);
   procedure T_0028 (T : in out Test_Case'Class);
   procedure T_0029 (T : in out Test_Case'Class);
   procedure T_0030 (T : in out Test_Case'Class);
   procedure T_0031 (T : in out Test_Case'Class);
   procedure T_0032 (T : in out Test_Case'Class);
   procedure T_0033 (T : in out Test_Case'Class);
   procedure T_0034 (T : in out Test_Case'Class);
   procedure T_0035 (T : in out Test_Case'Class);
   procedure T_0036 (T : in out Test_Case'Class);
   procedure T_0037 (T : in out Test_Case'Class);
   procedure T_0038 (T : in out Test_Case'Class);

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Locales");
      Add_Test_Routine (T, T_0001'Access, "T_0001, Current locale");
      Add_Test_Routine (T, T_0002'Access, "T_0002, Make_Locale (ZB_LANG)");
      Add_Test_Routine (T, T_0003'Access, "T_0003, Make_Locale (L, T)");
      Add_Test_Routine (T, T_0004'Access, "T_0004, Language_Code");
      Add_Test_Routine (T, T_0005'Access, "T_0005, Locale equality");
      Add_Test_Routine (T, T_0006'Access, "T_0006, Terrority_Code");
      Add_Test_Routine (T, T_0007'Access, "T_0007, Parent locales");
      Add_Test_Routine (T, T_0008'Access, "T_0008, Locale names");
      Add_Test_Routine (T, T_0009'Access, "T_0009, Script names");
      Add_Test_Routine (T, T_0010'Access, "T_0010, Parent locale with scipts");
      Add_Test_Routine (T, T_0011'Access, "T_0011, Set_Locale (Locale)");
      Add_Test_Routine (T, T_0012'Access, "T_0012, Set_Locale (String)");
      Add_Test_Routine (T, T_0013'Access,
                        "T_0013, Terrority_Code (3 character)");
      Add_Test_Routine (T, T_0014'Access,
                        "T_0014, Locale '-' component separator");
      Add_Test_Routine (T, T_0015'Access, "T_0015, Lowercase locale names");
      Add_Test_Routine (T, T_0016'Access,
                        "T_0016, Lowercase locale names w/ '-'");
      Add_Test_Routine (T, T_0017'Access,
                        "T_0017, Parent locale w/ base root");
      Add_Test_Routine (T, T_0018'Access,
                        "T_0018, Parent locale w/ full root");
      Add_Test_Routine (T, T_0019'Access,
                        "T_0019, Parent locale w/ ll_ssss root");
      Add_Test_Routine (T, T_0020'Access,
                        "T_0020, Parent locale w/ ll_tt root");
      Add_Test_Routine (T, T_0021'Access, "T_0021, Locale_Level");
      Add_Test_Routine (T, T_0022'Access, "T_0022, Root locale defined");
      Add_Test_Routine (T, T_0023'Access, "T_0023, en locale defined");
      Add_Test_Routine (T, T_0024'Access,
                        "T_0024, en_Latn locale not defined");
      Add_Test_Routine (T, T_0025'Access, "T_0025, Number_Of_Defined_Locales");
      Add_Test_Routine (T, T_0026'Access, "T_0026, Day_Period_Name fallback");
      Add_Test_Routine (T, T_0027'Access, "T_0027, Era_Name fallback");
      Add_Test_Routine (T, T_0028'Access, "T_0028, Date_Time format fallback");
      Add_Test_Routine (T, T_0029'Access, "T_0029, Traits_Tag value");
      Add_Test_Routine (T, T_0030'Access, "T_0030, Traits_Name value");
      Add_Test_Routine (T, T_0031'Access, "T_0031, Locale traits tags names");
      Add_Test_Routine (T, T_0032'Access,
                        "T_0032, Locale traits for Lang/Terr");
      Add_Test_Routine (T, T_0033'Access,
                        "T_0033, Locale traits Lang/Script/Terr");
      Add_Test_Routine (T, T_0034'Access, "T_0034, Set_Traits functionality");
      Add_Test_Routine (T, T_0035'Access, "T_0035, Set_Traits on String");
      Add_Test_Routine (T, T_0036'Access,
                        "T_0036, Transfer_Locale_Data: empty");
      Add_Test_Routine (T, T_0037'Access,
                        "T_0037, Transfer_Locale_Data: fr/en");
      Add_Test_Routine (T, T_0038'Access,
                        "T_0038, Transfer_Locale_Data: fr/en_US");
   end Initialize;

   function Suite return Test_Suite is
   begin
      return S : Test_Suite do
         Add_Static_Test (S, Currency_Formats.Suites.Suite);
         Add_Static_Test (S, Date_Formats.Suites.Suite);
         Add_Static_Test (S, Date_Time_Formats.Suites.Suite);
         Add_Static_Test (S, Day_Periods.Suites.Suite);
         Add_Static_Test (S, Days.Suites.Suite);
         Add_Static_Test (S, Decimal_Digits.Suites.Suite);
         Add_Static_Test (S, Decimal_Formats.Suites.Suite);
         Add_Static_Test (S, Decimal_Point_Characters.Suites.Suite);
         Add_Static_Test (S, Eras.Suites.Suite);
         Add_Static_Test (S, Exponent_Characters.Suites.Suite);
         Add_Static_Test (S, Group_Characters.Suites.Suite);
         Add_Static_Test (S, Infinity_Characters.Suites.Suite);
         Add_Static_Test (S, Layout.Suites.Suite);
         Add_Static_Test (S, List_Characters.Suites.Suite);
         Add_Static_Test (S, Digit_Pattern_Characters.Suites.Suite);
         Add_Static_Test (S, Localized_Digits.Suites.Suite);
         Add_Static_Test (S, Minus_Characters.Suites.Suite);
         Add_Static_Test (S, Nan_Characters.Suites.Suite);
         Add_Static_Test (S, Months.Suites.Suite);
         Add_Static_Test (S, Percent_Characters.Suites.Suite);
         Add_Static_Test (S, Percent_Formats.Suites.Suite);
         Add_Static_Test (S, Permille_Characters.Suites.Suite);
         Add_Static_Test (S, Plus_Characters.Suites.Suite);
         Add_Static_Test (S, Scientific_Formats.Suites.Suite);
         Add_Static_Test (S, Time_Formats.Suites.Suite);
         Add_Static_Test (S, Zero_Characters.Suites.Suite);
         Add_Test (S, new Test);
      end return;
   end Suite;

   procedure T_0001 (T : in out Test_Case'Class) is separate;
   procedure T_0002 (T : in out Test_Case'Class) is separate;
   procedure T_0003 (T : in out Test_Case'Class) is separate;
   procedure T_0004 (T : in out Test_Case'Class) is separate;
   procedure T_0005 (T : in out Test_Case'Class) is separate;
   procedure T_0006 (T : in out Test_Case'Class) is separate;
   procedure T_0007 (T : in out Test_Case'Class) is separate;
   procedure T_0008 (T : in out Test_Case'Class) is separate;
   procedure T_0009 (T : in out Test_Case'Class) is separate;
   procedure T_0010 (T : in out Test_Case'Class) is separate;
   procedure T_0011 (T : in out Test_Case'Class) is separate;
   procedure T_0012 (T : in out Test_Case'Class) is separate;
   procedure T_0013 (T : in out Test_Case'Class) is separate;
   procedure T_0014 (T : in out Test_Case'Class) is separate;
   procedure T_0015 (T : in out Test_Case'Class) is separate;
   procedure T_0016 (T : in out Test_Case'Class) is separate;
   procedure T_0017 (T : in out Test_Case'Class) is separate;
   procedure T_0018 (T : in out Test_Case'Class) is separate;
   procedure T_0019 (T : in out Test_Case'Class) is separate;
   procedure T_0020 (T : in out Test_Case'Class) is separate;
   procedure T_0021 (T : in out Test_Case'Class) is separate;
   procedure T_0022 (T : in out Test_Case'Class) is separate;
   procedure T_0023 (T : in out Test_Case'Class) is separate;
   procedure T_0024 (T : in out Test_Case'Class) is separate;
   procedure T_0025 (T : in out Test_Case'Class) is separate;
   procedure T_0026 (T : in out Test_Case'Class) is separate;
   procedure T_0027 (T : in out Test_Case'Class) is separate;
   procedure T_0028 (T : in out Test_Case'Class) is separate;
   procedure T_0029 (T : in out Test_Case'Class) is separate;
   procedure T_0030 (T : in out Test_Case'Class) is separate;
   procedure T_0031 (T : in out Test_Case'Class) is separate;
   procedure T_0032 (T : in out Test_Case'Class) is separate;
   procedure T_0033 (T : in out Test_Case'Class) is separate;
   procedure T_0034 (T : in out Test_Case'Class) is separate;
   procedure T_0035 (T : in out Test_Case'Class) is separate;
   procedure T_0036 (T : in out Test_Case'Class) is separate;
   procedure T_0037 (T : in out Test_Case'Class) is separate;
   procedure T_0038 (T : in out Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Locales.Suites;
