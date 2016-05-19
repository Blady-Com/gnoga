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

with ZanyBlue.Test.Text.Generic_Floats.My_Float_Type;

package body ZanyBlue.Test.Text.Generic_Floats.Format_F.Suites is

   use Ahven.Framework;
   use ZanyBlue.Test.Text.Generic_Floats.My_Float_Type;
   use ZanyBlue.Test.Text.Generic_Floats.My_Float_Type.My_Float_Arguments;

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
   procedure T_0078 (T : in out Test_Case'Class);
   procedure T_0079 (T : in out Test_Case'Class);
   procedure T_0080 (T : in out Test_Case'Class);
   procedure T_0081 (T : in out Test_Case'Class);
   procedure T_0082 (T : in out Test_Case'Class);
   procedure T_0083 (T : in out Test_Case'Class);
   procedure T_0084 (T : in out Test_Case'Class);
   procedure T_0085 (T : in out Test_Case'Class);
   procedure T_0086 (T : in out Test_Case'Class);
   procedure T_0087 (T : in out Test_Case'Class);
   procedure T_0088 (T : in out Test_Case'Class);
   procedure T_0089 (T : in out Test_Case'Class);
   procedure T_0090 (T : in out Test_Case'Class);
   procedure T_0091 (T : in out Test_Case'Class);

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Generic_Floats.Format_F");
      Add_Test_Routine (T, T_0001'Access, "T_0001, Format 0.0");
      Add_Test_Routine (T, T_0002'Access, "T_0002, Format NaN (root locale)");
      Add_Test_Routine (T, T_0003'Access, "T_0003, Format NaN (ar locale)");
      Add_Test_Routine (T, T_0004'Access, "T_0004, Format NaN (ru locale)");
      Add_Test_Routine (T, T_0005'Access, "T_0005, Format NaN (fi locale)");
      Add_Test_Routine (T, T_0006'Access, "T_0006, Format Inf (root locale)");
      Add_Test_Routine (T, T_0007'Access, "T_0007, Format -Inf (root locale)");
      Add_Test_Routine (T, T_0008'Access, "T_0008, Format Inf (en locale)");
      Add_Test_Routine (T, T_0009'Access, "T_0009, Format -Inf (en locale)");
      Add_Test_Routine (T, T_0010'Access, "T_0010, Format Inf (ar locale)");
      Add_Test_Routine (T, T_0011'Access, "T_0011, Format -Inf (ar locale)");
      Add_Test_Routine (T, T_0012'Access, "T_0012, Format Inf (ru locale)");
      Add_Test_Routine (T, T_0013'Access, "T_0013, Format -Inf (ru locale)");
      Add_Test_Routine (T, T_0014'Access, "T_0014, Format Inf (fi locale)");
      Add_Test_Routine (T, T_0015'Access, "T_0015, Format -Inf (fi locale)");
      Add_Test_Routine (T, T_0016'Access, "T_0016, Format 1.23456789e-9");
      Add_Test_Routine (T, T_0017'Access, "T_0017, Format 1.23456789e-8");
      Add_Test_Routine (T, T_0018'Access, "T_0018, Format 1.23456789e-7");
      Add_Test_Routine (T, T_0019'Access, "T_0019, Format 1.23456789e-6");
      Add_Test_Routine (T, T_0020'Access, "T_0020, Format 1.23456789e-5");
      Add_Test_Routine (T, T_0021'Access, "T_0021, Format 1.23456789e-4");
      Add_Test_Routine (T, T_0022'Access, "T_0022, Format 1.23456789e-3");
      Add_Test_Routine (T, T_0023'Access, "T_0023, Format 1.23456789e-2");
      Add_Test_Routine (T, T_0024'Access, "T_0024, Format 1.23456789e-1");
      Add_Test_Routine (T, T_0025'Access, "T_0025, Format 1.23456789e+0");
      Add_Test_Routine (T, T_0026'Access, "T_0026, Format 1.23456789e+1");
      Add_Test_Routine (T, T_0027'Access, "T_0027, Format 1.23456789e+2");
      Add_Test_Routine (T, T_0028'Access, "T_0028, Format 1.23456789e+3");
      Add_Test_Routine (T, T_0029'Access, "T_0029, Format 1.23456789e+4");
      Add_Test_Routine (T, T_0030'Access, "T_0030, Format 1.23456789e+5");
      Add_Test_Routine (T, T_0031'Access, "T_0031, Format 1.23456789e+6");
      Add_Test_Routine (T, T_0032'Access, "T_0032, Format 1.23456789e+7");
      Add_Test_Routine (T, T_0033'Access, "T_0033, Format 1.23456789e+8");
      Add_Test_Routine (T, T_0034'Access, "T_0034, Format 1.23456789e+9");
      Add_Test_Routine (T, T_0035'Access,
                        "T_0035, Format 1.23456789e-9 w/prec 5");
      Add_Test_Routine (T, T_0036'Access,
                        "T_0036, Format 1.23456789e-8 w/prec 5");
      Add_Test_Routine (T, T_0037'Access,
                        "T_0037, Format 1.23456789e-7 w/prec 5");
      Add_Test_Routine (T, T_0038'Access,
                        "T_0038, Format 1.23456789e-6 w/prec 5");
      Add_Test_Routine (T, T_0039'Access,
                        "T_0039, Format 1.23456789e-5 w/prec 5");
      Add_Test_Routine (T, T_0040'Access,
                        "T_0040, Format 1.23456789e-4 w/prec 5");
      Add_Test_Routine (T, T_0041'Access,
                        "T_0041, Format 1.23456789e-3 w/prec 5");
      Add_Test_Routine (T, T_0042'Access,
                        "T_0042, Format 1.23456789e-2 w/prec 5");
      Add_Test_Routine (T, T_0043'Access,
                        "T_0043, Format 1.23456789e-1 w/prec 5");
      Add_Test_Routine (T, T_0044'Access,
                        "T_0044, Format 1.23456789e+0 w/prec 5");
      Add_Test_Routine (T, T_0045'Access,
                        "T_0045, Format 1.23456789e+1 w/prec 5");
      Add_Test_Routine (T, T_0046'Access,
                        "T_0046, Format 1.23456789e+2 w/prec 5");
      Add_Test_Routine (T, T_0047'Access,
                        "T_0047, Format 1.23456789e+3 w/prec 5");
      Add_Test_Routine (T, T_0048'Access,
                        "T_0048, Format 1.23456789e+4 w/prec 5");
      Add_Test_Routine (T, T_0049'Access,
                        "T_0049, Format 1.23456789e+5 w/prec 5");
      Add_Test_Routine (T, T_0050'Access,
                        "T_0050, Format 1.23456789e+6 w/prec 5");
      Add_Test_Routine (T, T_0051'Access,
                        "T_0051, Format 1.23456789e+7 w/prec 5");
      Add_Test_Routine (T, T_0052'Access,
                        "T_0052, Format 1.23456789e+8 w/prec 5");
      Add_Test_Routine (T, T_0053'Access,
                        "T_0053, Format 1.23456789e+9 w/prec 5");
      Add_Test_Routine (T, T_0054'Access,
                        "T_0054, Format 7.777e-9 w/prec 5");
      Add_Test_Routine (T, T_0055'Access,
                        "T_0055, Format 7.777e-8 w/prec 5");
      Add_Test_Routine (T, T_0056'Access,
                        "T_0056, Format 7.777e-7 w/prec 5");
      Add_Test_Routine (T, T_0057'Access, "T_0057, Format 7.777e-6 w/prec 5");
      Add_Test_Routine (T, T_0058'Access, "T_0058, Format 7.777e-5 w/prec 5");
      Add_Test_Routine (T, T_0059'Access, "T_0059, Format 7.777e-4 w/prec 5");
      Add_Test_Routine (T, T_0060'Access, "T_0060, Format 7.777e-3 w/prec 5");
      Add_Test_Routine (T, T_0061'Access, "T_0061, Format 7.777e-2 w/prec 5");
      Add_Test_Routine (T, T_0062'Access, "T_0062, Format 7.777e-1 w/prec 5");
      Add_Test_Routine (T, T_0063'Access, "T_0063, Format 7.777e+0 w/prec 5");
      Add_Test_Routine (T, T_0064'Access, "T_0064, Format 7.777e+1 w/prec 5");
      Add_Test_Routine (T, T_0065'Access, "T_0065, Format 7.777e+2 w/prec 5");
      Add_Test_Routine (T, T_0066'Access, "T_0066, Format 7.777e+3 w/prec 5");
      Add_Test_Routine (T, T_0067'Access, "T_0067, Format 7.777e+4 w/prec 5");
      Add_Test_Routine (T, T_0068'Access, "T_0068, Format 7.777e+5 w/prec 5");
      Add_Test_Routine (T, T_0069'Access, "T_0069, Format 7.777e+6 w/prec 5");
      Add_Test_Routine (T, T_0070'Access, "T_0070, Format 7.777e+7 w/prec 5");
      Add_Test_Routine (T, T_0071'Access, "T_0071, Format 7.777e+8 w/prec 5");
      Add_Test_Routine (T, T_0072'Access, "T_0072, Format 7.777e+9 w/prec 5");
      Add_Test_Routine (T, T_0073'Access, "T_0073, Format 9.999e-9 w/prec 5");
      Add_Test_Routine (T, T_0074'Access, "T_0074, Format 9.999e-8 w/prec 5");
      Add_Test_Routine (T, T_0075'Access, "T_0075, Format 9.999e-7 w/prec 5");
      Add_Test_Routine (T, T_0076'Access, "T_0076, Format 9.999e-6 w/prec 5");
      Add_Test_Routine (T, T_0077'Access, "T_0077, Format 9.999e-5 w/prec 5");
      Add_Test_Routine (T, T_0078'Access, "T_0078, Format 9.999e-4 w/prec 5");
      Add_Test_Routine (T, T_0079'Access, "T_0079, Format 9.999e-3 w/prec 5");
      Add_Test_Routine (T, T_0080'Access, "T_0080, Format 9.999e-2 w/prec 5");
      Add_Test_Routine (T, T_0081'Access, "T_0081, Format 9.999e-1 w/prec 5");
      Add_Test_Routine (T, T_0082'Access, "T_0082, Format 9.999e+0 w/prec 5");
      Add_Test_Routine (T, T_0083'Access, "T_0083, Format 9.999e+1 w/prec 5");
      Add_Test_Routine (T, T_0084'Access, "T_0084, Format 9.999e+2 w/prec 5");
      Add_Test_Routine (T, T_0085'Access, "T_0085, Format 9.999e+3 w/prec 5");
      Add_Test_Routine (T, T_0086'Access, "T_0086, Format 9.999e+4 w/prec 5");
      Add_Test_Routine (T, T_0087'Access, "T_0087, Format 9.999e+5 w/prec 5");
      Add_Test_Routine (T, T_0088'Access, "T_0088, Format 9.999e+6 w/prec 5");
      Add_Test_Routine (T, T_0089'Access, "T_0089, Format 9.999e+7 w/prec 5");
      Add_Test_Routine (T, T_0090'Access, "T_0090, Format 9.999e+8 w/prec 5");
      Add_Test_Routine (T, T_0091'Access, "T_0091, Format 9.999e+9 w/prec 5");
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
   procedure T_0078 (T : in out Test_Case'Class) is separate;
   procedure T_0079 (T : in out Test_Case'Class) is separate;
   procedure T_0080 (T : in out Test_Case'Class) is separate;
   procedure T_0081 (T : in out Test_Case'Class) is separate;
   procedure T_0082 (T : in out Test_Case'Class) is separate;
   procedure T_0083 (T : in out Test_Case'Class) is separate;
   procedure T_0084 (T : in out Test_Case'Class) is separate;
   procedure T_0085 (T : in out Test_Case'Class) is separate;
   procedure T_0086 (T : in out Test_Case'Class) is separate;
   procedure T_0087 (T : in out Test_Case'Class) is separate;
   procedure T_0088 (T : in out Test_Case'Class) is separate;
   procedure T_0089 (T : in out Test_Case'Class) is separate;
   procedure T_0090 (T : in out Test_Case'Class) is separate;
   procedure T_0091 (T : in out Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Generic_Floats.Format_F.Suites;
