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

with AUnit;
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

   use AUnit;
   use ZanyBlue.Text.Locales;

   procedure T_0001 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0002 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0003 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0004 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0005 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0006 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0007 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0008 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0009 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0010 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0011 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0012 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0013 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0014 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0015 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0016 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0017 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0018 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0019 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0020 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0021 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0022 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0023 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0024 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0025 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0026 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0027 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0028 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0029 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0030 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0031 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0032 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0033 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0034 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0035 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0036 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0037 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0038 (R : in out AUnit.Test_Cases.Test_Case'Class);

   overriding
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
      use ZanyBlue.Test.Text.Locales;
   begin
      return Format ("ZanyBlue.Text.Locales");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, Current locale");
      Add_Routine (T, T_0002'Access, "T_0002, Make_Locale (ZB_LANG)");
      Add_Routine (T, T_0003'Access, "T_0003, Make_Locale (L, T)");
      Add_Routine (T, T_0004'Access, "T_0004, Language_Code");
      Add_Routine (T, T_0005'Access, "T_0005, Locale equality");
      Add_Routine (T, T_0006'Access, "T_0006, Terrority_Code");
      Add_Routine (T, T_0007'Access, "T_0007, Parent locales");
      Add_Routine (T, T_0008'Access, "T_0008, Locale names");
      Add_Routine (T, T_0009'Access, "T_0009, Script names");
      Add_Routine (T, T_0010'Access, "T_0010, Parent locale with scipts");
      Add_Routine (T, T_0011'Access, "T_0011, Set_Locale (Locale)");
      Add_Routine (T, T_0012'Access, "T_0012, Set_Locale (String)");
      Add_Routine (T, T_0013'Access, "T_0013, Terrority_Code (3 character)");
      Add_Routine (T, T_0014'Access, "T_0014, Locale '-' component separator");
      Add_Routine (T, T_0015'Access, "T_0015, Lowercase locale names");
      Add_Routine (T, T_0016'Access, "T_0016, Lowercase locale names w/ '-'");
      Add_Routine (T, T_0017'Access, "T_0017, Parent locale w/ base root");
      Add_Routine (T, T_0018'Access, "T_0018, Parent locale w/ full root");
      Add_Routine (T, T_0019'Access, "T_0019, Parent locale w/ ll_ssss root");
      Add_Routine (T, T_0020'Access, "T_0020, Parent locale w/ ll_tt root");
      Add_Routine (T, T_0021'Access, "T_0021, Locale_Level");
      Add_Routine (T, T_0022'Access, "T_0022, Root locale defined");
      Add_Routine (T, T_0023'Access, "T_0023, en locale defined");
      Add_Routine (T, T_0024'Access, "T_0024, en_Latn locale not defined");
      Add_Routine (T, T_0025'Access, "T_0025, Number_Of_Defined_Locales");
      Add_Routine (T, T_0026'Access, "T_0026, Day_Period_Name fallback");
      Add_Routine (T, T_0027'Access, "T_0027, Era_Name fallback");
      Add_Routine (T, T_0028'Access, "T_0028, Date_Time format fallback");
      Add_Routine (T, T_0029'Access, "T_0029, Traits_Tag value");
      Add_Routine (T, T_0030'Access, "T_0030, Traits_Name value");
      Add_Routine (T, T_0031'Access, "T_0031, Locale traits tags names");
      Add_Routine (T, T_0032'Access, "T_0032, Locale traits for Lang/Terr");
      Add_Routine (T, T_0033'Access, "T_0033, Locale traits Lang/Script/Terr");
      Add_Routine (T, T_0034'Access, "T_0034, Set_Traits functionality");
      Add_Routine (T, T_0035'Access, "T_0035, Set_Traits on String");
      Add_Routine (T, T_0036'Access, "T_0036, Transfer_Locale_Data: empty");
      Add_Routine (T, T_0037'Access, "T_0037, Transfer_Locale_Data: fr/en");
      Add_Routine (T, T_0038'Access, "T_0038, Transfer_Locale_Data: fr/en_US");
   end Register_Tests;

   function Suite return Access_Test_Suite is
      use ZanyBlue.Test.Text.Locales;
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, Currency_Formats.Suites.Suite);
      Add_Test (Result, Date_Formats.Suites.Suite);
      Add_Test (Result, Date_Time_Formats.Suites.Suite);
      Add_Test (Result, Day_Periods.Suites.Suite);
      Add_Test (Result, Days.Suites.Suite);
      Add_Test (Result, Decimal_Digits.Suites.Suite);
      Add_Test (Result, Decimal_Formats.Suites.Suite);
      Add_Test (Result, Decimal_Point_Characters.Suites.Suite);
      Add_Test (Result, Eras.Suites.Suite);
      Add_Test (Result, Exponent_Characters.Suites.Suite);
      Add_Test (Result, Group_Characters.Suites.Suite);
      Add_Test (Result, Infinity_Characters.Suites.Suite);
      Add_Test (Result, Layout.Suites.Suite);
      Add_Test (Result, List_Characters.Suites.Suite);
      Add_Test (Result, Digit_Pattern_Characters.Suites.Suite);
      Add_Test (Result, Localized_Digits.Suites.Suite);
      Add_Test (Result, Minus_Characters.Suites.Suite);
      Add_Test (Result, Nan_Characters.Suites.Suite);
      Add_Test (Result, Months.Suites.Suite);
      Add_Test (Result, Percent_Characters.Suites.Suite);
      Add_Test (Result, Percent_Formats.Suites.Suite);
      Add_Test (Result, Permille_Characters.Suites.Suite);
      Add_Test (Result, Plus_Characters.Suites.Suite);
      Add_Test (Result, Scientific_Formats.Suites.Suite);
      Add_Test (Result, Time_Formats.Suites.Suite);
      Add_Test (Result, Zero_Characters.Suites.Suite);
      Add_Test (Result, new Test_Case);
      return Result;
   end Suite;

   procedure T_0001 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0002 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0003 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0004 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0005 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0006 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0007 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0008 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0009 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0010 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0011 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0012 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0013 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0014 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0015 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0016 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0017 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0018 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0019 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0020 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0021 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0022 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0023 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0024 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0025 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0026 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0027 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0028 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0029 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0030 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0031 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0032 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0033 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0034 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0035 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0036 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0037 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0038 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Locales.Suites;
