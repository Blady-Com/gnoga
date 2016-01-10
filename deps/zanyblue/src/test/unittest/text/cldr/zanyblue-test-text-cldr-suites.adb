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

with ZanyBlue.Text.CLDR;
with ZanyBlue.Text.Locales;

package body ZanyBlue.Test.Text.CLDR.Suites is

   use AUnit;
   use ZanyBlue.Text.CLDR;
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
   procedure T_0039 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0040 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0041 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0042 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0043 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0044 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0045 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0046 (R : in out AUnit.Test_Cases.Test_Case'Class);

   overriding
   function Name (T : Test_Case) return AUnit.Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("ZanyBlue.Text.CLDR");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, 'en' mapping in 'en'");
      Add_Routine (T, T_0002'Access, "T_0002, 'en' mapping in 'fr'");
      Add_Routine (T, T_0003'Access, "T_0003, 'fr' mapping in 'fr'");
      Add_Routine (T, T_0004'Access, "T_0004, 'fr' mapping in 'en'");
      Add_Routine (T, T_0005'Access, "T_0005, 'US' mapping in 'en'");
      Add_Routine (T, T_0006'Access, "T_0006, 'RU' mapping in 'en'");
      Add_Routine (T, T_0007'Access, "T_0007, 'US' mapping in 'ru'");
      Add_Routine (T, T_0008'Access, "T_0008, 'RU' mapping in 'ru'");
      Add_Routine (T, T_0009'Access, "T_0009, 'Cyrl' mapping in 'en'");
      Add_Routine (T, T_0010'Access, "T_0010, 'Cyrl' mapping in 'ru'");
      Add_Routine (T, T_0011'Access, "T_0011, 'Cyrl' mapping in 'ja'");
      Add_Routine (T, T_0012'Access, "T_0012, 'Latn' mapping in 'en'");
      Add_Routine (T, T_0013'Access, "T_0013, 'Latn' mapping in 'ru'");
      Add_Routine (T, T_0014'Access, "T_0014, 'Latn' mapping in 'ja'");
      Add_Routine (T, T_0015'Access, "T_0015, en_US full locale name");
      Add_Routine (T, T_0016'Access, "T_0016, de full locale name");
      Add_Routine (T, T_0017'Access, "T_0017, fr full locale name");
      Add_Routine (T, T_0018'Access, "T_0018, ja full locale name");
      Add_Routine (T, T_0019'Access, "T_0019, zh (Hans) full locale name");
      Add_Routine (T, T_0020'Access, "T_0020, Fallback digits");
      Add_Routine (T, T_0021'Access, "T_0021, Fallback full locale name");
      Add_Routine (T, T_0022'Access, "T_0022, '' mapping in 'en'");
      Add_Routine (T, T_0023'Access, "T_0023, '' mapping in 'de'");
      Add_Routine (T, T_0024'Access, "T_0024, '' mapping in 'fr'");
      Add_Routine (T, T_0025'Access, "T_0025, '' mapping in 'ja'");
      Add_Routine (T, T_0026'Access, "T_0026, '' mapping in 'zh (Hans)'");
      Add_Routine (T, T_0027'Access, "T_0027, zh_Hant full locale name");
      Add_Routine (T, T_0028'Access, "T_0028, '' mapping in 'zh_Hant'");
      Add_Routine (T, T_0029'Access, "T_0029, Selected fr language names");
      Add_Routine (T, T_0030'Access, "T_0030, Selected ar language names");
      Add_Routine (T, T_0031'Access, "T_0031, Selected he language names");
      Add_Routine (T, T_0032'Access, "T_0032, Selected ja language names");
      Add_Routine (T, T_0033'Access, "T_0033, Selected zh language names");
      Add_Routine (T, T_0034'Access, "T_0034, Selected fr script names");
      Add_Routine (T, T_0035'Access, "T_0035, Selected ar script names");
      Add_Routine (T, T_0036'Access, "T_0036, Selected he script names");
      Add_Routine (T, T_0037'Access, "T_0037, Selected ja script names");
      Add_Routine (T, T_0038'Access, "T_0038, Selected zh script names");
      Add_Routine (T, T_0039'Access, "T_0039, Selected fr territory names");
      Add_Routine (T, T_0040'Access, "T_0040, Selected ar territory names");
      Add_Routine (T, T_0041'Access, "T_0041, Selected he territory names");
      Add_Routine (T, T_0042'Access, "T_0042, Selected ja territory names");
      Add_Routine (T, T_0043'Access, "T_0043, Selected zh territory names");
      Add_Routine (T, T_0044'Access, "T_0044, no = nb for language names");
      Add_Routine (T, T_0045'Access, "T_0045, no = nb for script names");
      Add_Routine (T, T_0046'Access, "T_0046, no = nb for territory names");
   end Register_Tests;

   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
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
   procedure T_0039 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0040 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0041 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0042 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0043 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0044 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0045 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0046 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;

end ZanyBlue.Test.Text.CLDR.Suites;
