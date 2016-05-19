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

with ZanyBlue.Text.Times;

package body ZanyBlue.Test.Text.Times.Suites is

   use Ahven.Framework;

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
   procedure T_0039 (T : in out Test_Case'Class);
   procedure T_0040 (T : in out Test_Case'Class);
   procedure T_0041 (T : in out Test_Case'Class);
   procedure T_0042 (T : in out Test_Case'Class);
   procedure T_0043 (T : in out Test_Case'Class);
   procedure T_0044 (T : in out Test_Case'Class);
   procedure T_0045 (T : in out Test_Case'Class);
   procedure T_0046 (T : in out Test_Case'Class);
   procedure T_0047 (T : in out Test_Case'Class);
   procedure T_0048 (T : in out Test_Case'Class);
   procedure T_0049 (T : in out Test_Case'Class);
   procedure T_0050 (T : in out Test_Case'Class);
   procedure T_0051 (T : in out Test_Case'Class);
   procedure T_0052 (T : in out Test_Case'Class);
   procedure T_0053 (T : in out Test_Case'Class);
   procedure T_0054 (T : in out Test_Case'Class);
   procedure T_0055 (T : in out Test_Case'Class);
   procedure T_0056 (T : in out Test_Case'Class);
   procedure T_0057 (T : in out Test_Case'Class);
   procedure T_0058 (T : in out Test_Case'Class);
   procedure T_0059 (T : in out Test_Case'Class);
   procedure T_0060 (T : in out Test_Case'Class);
   procedure T_0061 (T : in out Test_Case'Class);
   procedure T_0062 (T : in out Test_Case'Class);
   procedure T_0063 (T : in out Test_Case'Class);
   procedure T_0064 (T : in out Test_Case'Class);
   procedure T_0065 (T : in out Test_Case'Class);
   procedure T_0066 (T : in out Test_Case'Class);
   procedure T_0067 (T : in out Test_Case'Class);
   procedure T_0068 (T : in out Test_Case'Class);
   procedure T_0069 (T : in out Test_Case'Class);
   procedure T_0070 (T : in out Test_Case'Class);
   procedure T_0071 (T : in out Test_Case'Class);
   procedure T_0072 (T : in out Test_Case'Class);
   procedure T_0073 (T : in out Test_Case'Class);
   procedure T_0074 (T : in out Test_Case'Class);
   procedure T_0075 (T : in out Test_Case'Class);
   procedure T_0076 (T : in out Test_Case'Class);
   procedure T_0077 (T : in out Test_Case'Class);

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Times");
      Add_Test_Routine (T, T_0001'Access, "T_0001, Create/Format");
      Add_Test_Routine (T, T_0002'Access, "T_0002, +/Format");
      Add_Test_Routine (T, T_0003'Access,
                        "T_0003, fr_FR: 'EEE': day abbreviation");
      Add_Test_Routine (T, T_0004'Access,
                        "T_0004, en_US: 'EEE': day abbreviation");
      Add_Test_Routine (T, T_0005'Access, "T_0005, ja_JP: Bloomsday");
      Add_Test_Routine (T, T_0006'Access, "T_0006, Argument_List/Format");
      Add_Test_Routine (T, T_0007'Access, "T_0007, Format - Time");
      Add_Test_Routine (T, T_0008'Access, "T_0008, List.Format - Time");
      Add_Test_Routine (T, T_0009'Access, "T_0009, Format - Date");
      Add_Test_Routine (T, T_0010'Access, "T_0010, List.Format - Date");
      Add_Test_Routine (T, T_0011'Access, "T_0011, en_US: 'a': AM/PM");
      Add_Test_Routine (T, T_0012'Access, "T_0012, en_US: 'd': 1/2 digit day");
      Add_Test_Routine (T, T_0013'Access, "T_0013, en_US: 'dd': 2 digit day");
      Add_Test_Routine (T, T_0014'Access, "T_0014, en_US: 'EEEE': day name");
      Add_Test_Routine (T, T_0015'Access, "T_0015, en_US: 'G': era name");
      Add_Test_Routine (T, T_0016'Access,
                        "T_0016, en_US: 'h': 1/2 digit hour, 12");
      Add_Test_Routine (T, T_0017'Access,
                        "T_0017, en_US: 'H': 1/2 digit hour, 24");
      Add_Test_Routine (T, T_0018'Access,
                        "T_0018, en_US: 'HH': 2 digit hour, 24");
      Add_Test_Routine (T, T_0019'Access,
                        "T_0019, en_US: 'm': 1/2 digit minute");
      Add_Test_Routine (T, T_0020'Access,
                        "T_0020, en_US: 'mm': 2 digit minute");
      Add_Test_Routine (T, T_0021'Access,
                        "T_0021, en_US: 'M': 1/2 digit month");
      Add_Test_Routine (T, T_0022'Access,
                        "T_0022, en_US: 'MM': 2 digit month");
      Add_Test_Routine (T, T_0023'Access, "T_0023, en_US: 'MMM': month abbr");
      Add_Test_Routine (T, T_0024'Access, "T_0024, en_US: 'MMMM': month name");
      Add_Test_Routine (T, T_0025'Access,
                        "T_0025, en_US: 's': 1/2 digit second");
      Add_Test_Routine (T, T_0026'Access,
                        "T_0026, en_US: 'ss': 2 digit second");
      Add_Test_Routine (T, T_0027'Access,
                        "T_0027, en_US: 'y': 1/2 digit year");
      Add_Test_Routine (T, T_0028'Access, "T_0028, en_US: 'yy': 2 digit year");
      Add_Test_Routine (T, T_0029'Access,
                        "T_0029, en_US: 'yyyy': 4 digit year");
      Add_Test_Routine (T, T_0030'Access, "T_0030, en_US: 'z': tz offset");
      Add_Test_Routine (T, T_0031'Access, "T_0031, en_US: 'zzzz': tz offset");

      Add_Test_Routine (T, T_0032'Access, "T_0032, fr_FR: 'a': AM/PM");
      Add_Test_Routine (T, T_0033'Access, "T_0033, fr_FR: 'd': 1/2 digit day");
      Add_Test_Routine (T, T_0034'Access, "T_0034, fr_FR: 'dd': 2 digit day");
      Add_Test_Routine (T, T_0035'Access, "T_0035, fr_FR: 'EEEE': day name");
      Add_Test_Routine (T, T_0036'Access, "T_0036, fr_FR: 'G': era name");
      Add_Test_Routine (T, T_0037'Access,
                        "T_0037, fr_FR: 'h': 1/2 digit hour, 12");
      Add_Test_Routine (T, T_0038'Access,
                        "T_0038, fr_FR: 'H': 1/2 digit hour, 24");
      Add_Test_Routine (T, T_0039'Access,
                        "T_0039, fr_FR: 'HH': 2 digit hour, 24");
      Add_Test_Routine (T, T_0040'Access,
                        "T_0040, fr_FR: 'm': 1/2 digit minute");
      Add_Test_Routine (T, T_0041'Access,
                        "T_0041, fr_FR: 'mm': 2 digit minute");
      Add_Test_Routine (T, T_0042'Access,
                        "T_0042, fr_FR: 'M': 1/2 digit month");
      Add_Test_Routine (T, T_0043'Access,
                        "T_0043, fr_FR: 'MM': 2 digit month");
      Add_Test_Routine (T, T_0044'Access,
                        "T_0044, fr_FR: 'MMM': month abbr");
      Add_Test_Routine (T, T_0045'Access, "T_0045, fr_FR: 'MMMM': month name");
      Add_Test_Routine (T, T_0046'Access,
                        "T_0046, fr_FR: 's': 1/2 digit second");
      Add_Test_Routine (T, T_0047'Access,
                        "T_0047, fr_FR: 'ss': 2 digit second");
      Add_Test_Routine (T, T_0048'Access,
                        "T_0048, fr_FR: 'y': 1/2 digit year");
      Add_Test_Routine (T, T_0049'Access, "T_0049, fr_FR: 'yy': 2 digit year");
      Add_Test_Routine (T, T_0050'Access,
                        "T_0050, fr_FR: 'yyyy': 4 digit year");
      Add_Test_Routine (T, T_0051'Access, "T_0051, fr_FR: 'z': tz offset");
      Add_Test_Routine (T, T_0052'Access, "T_0052, fr_FR: 'zzzz': tz offset");

      Add_Test_Routine (T, T_0053'Access, "T_0053, en_IE: Bloomsday");
      Add_Test_Routine (T, T_0054'Access, "T_0054, fr_FR: Bloomsday");
      Add_Test_Routine (T, T_0055'Access, "T_0055, ru_RU: Bloomsday");
      Add_Test_Routine (T, T_0056'Access, "T_0056, el_GR: Bloomsday");
      Add_Test_Routine (T, T_0057'Access, "T_0057, he_IL: Bloomsday");
      Add_Test_Routine (T, T_0058'Access, "T_0058, ar_SA: Bloomsday");
      Add_Test_Routine (T, T_0059'Access, "T_0059, zh_CN: Bloomsday");
      Add_Test_Routine (T, T_0060'Access, "T_0060, ko_KR: Bloomsday");

      Add_Test_Routine (T, T_0061'Access, "T_0061, en_IE: Bloomsday, time");
      Add_Test_Routine (T, T_0062'Access, "T_0062, fr_FR: Bloomsday, time");
      Add_Test_Routine (T, T_0063'Access, "T_0063, en_IE: Bloomsday, date");
      Add_Test_Routine (T, T_0064'Access, "T_0064, fr_FR: Bloomsday, date");

      Add_Test_Routine (T, T_0065'Access, "T_0065, en_US: 'EEE': day name");
      Add_Test_Routine (T, T_0066'Access, "T_0066, fr_FR: 'EEE': day name");

      Add_Test_Routine (T, T_0067'Access, "T_0067, en_US: 'E'/'EE': identity");

      Add_Test_Routine (T, T_0068'Access, "T_0068, zh_Hans: Bloomsday");

      Add_Test_Routine (T, T_0069'Access, "T_0069, en: time with day period");
      Add_Test_Routine (T, T_0070'Access, "T_0070, de: time with day period");
      Add_Test_Routine (T, T_0071'Access, "T_0071, fr: time with day period");
      Add_Test_Routine (T, T_0072'Access, "T_0072, ja: time with day period");
      Add_Test_Routine (T, T_0073'Access, "T_0073, zh: time with day period");
      Add_Test_Routine (T, T_0074'Access, "T_0074, ar: time with day period");
      Add_Test_Routine (T, T_0075'Access, "T_0075, ko: time with day period");

      Add_Test_Routine (T, T_0076'Access, "T_0076, Day_In_Week calculation");
      Add_Test_Routine (T, T_0077'Access,
                        "T_0077, Force root locale formatting");
   end Initialize;

   function Suite return Test_Suite is
   begin
      return S : Test_Suite do
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
   procedure T_0039 (T : in out Test_Case'Class) is separate;
   procedure T_0040 (T : in out Test_Case'Class) is separate;
   procedure T_0041 (T : in out Test_Case'Class) is separate;
   procedure T_0042 (T : in out Test_Case'Class) is separate;
   procedure T_0043 (T : in out Test_Case'Class) is separate;
   procedure T_0044 (T : in out Test_Case'Class) is separate;
   procedure T_0045 (T : in out Test_Case'Class) is separate;
   procedure T_0046 (T : in out Test_Case'Class) is separate;
   procedure T_0047 (T : in out Test_Case'Class) is separate;
   procedure T_0048 (T : in out Test_Case'Class) is separate;
   procedure T_0049 (T : in out Test_Case'Class) is separate;
   procedure T_0050 (T : in out Test_Case'Class) is separate;
   procedure T_0051 (T : in out Test_Case'Class) is separate;
   procedure T_0052 (T : in out Test_Case'Class) is separate;
   procedure T_0053 (T : in out Test_Case'Class) is separate;
   procedure T_0054 (T : in out Test_Case'Class) is separate;
   procedure T_0055 (T : in out Test_Case'Class) is separate;
   procedure T_0056 (T : in out Test_Case'Class) is separate;
   procedure T_0057 (T : in out Test_Case'Class) is separate;
   procedure T_0058 (T : in out Test_Case'Class) is separate;
   procedure T_0059 (T : in out Test_Case'Class) is separate;
   procedure T_0060 (T : in out Test_Case'Class) is separate;
   procedure T_0061 (T : in out Test_Case'Class) is separate;
   procedure T_0062 (T : in out Test_Case'Class) is separate;
   procedure T_0063 (T : in out Test_Case'Class) is separate;
   procedure T_0064 (T : in out Test_Case'Class) is separate;
   procedure T_0065 (T : in out Test_Case'Class) is separate;
   procedure T_0066 (T : in out Test_Case'Class) is separate;
   procedure T_0067 (T : in out Test_Case'Class) is separate;
   procedure T_0068 (T : in out Test_Case'Class) is separate;
   procedure T_0069 (T : in out Test_Case'Class) is separate;
   procedure T_0070 (T : in out Test_Case'Class) is separate;
   procedure T_0071 (T : in out Test_Case'Class) is separate;
   procedure T_0072 (T : in out Test_Case'Class) is separate;
   procedure T_0073 (T : in out Test_Case'Class) is separate;
   procedure T_0074 (T : in out Test_Case'Class) is separate;
   procedure T_0075 (T : in out Test_Case'Class) is separate;
   procedure T_0076 (T : in out Test_Case'Class) is separate;
   procedure T_0077 (T : in out Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Times.Suites;
