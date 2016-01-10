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

with ZanyBlue.Test.Text.Generic_Floats.My_Float_Type;

package body ZanyBlue.Test.Text.Generic_Floats.Format_F.Suites is

   use AUnit;
   use ZanyBlue.Test.Text.Generic_Floats.My_Float_Type;
   use ZanyBlue.Test.Text.Generic_Floats.My_Float_Type.My_Float_Arguments;

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
   procedure T_0047 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0048 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0049 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0050 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0051 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0052 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0053 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0054 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0055 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0056 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0057 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0058 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0059 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0060 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0061 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0062 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0063 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0064 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0065 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0066 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0067 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0068 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0069 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0070 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0071 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0072 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0073 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0074 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0075 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0076 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0077 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0078 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0079 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0080 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0081 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0082 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0083 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0084 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0085 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0086 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0087 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0088 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0089 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0090 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0091 (R : in out AUnit.Test_Cases.Test_Case'Class);

   overriding
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("ZanyBlue.Text.Generic_Floats.Format_F");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, Format 0.0");
      Add_Routine (T, T_0002'Access, "T_0002, Format NaN (root locale)");
      Add_Routine (T, T_0003'Access, "T_0003, Format NaN (ar locale)");
      Add_Routine (T, T_0004'Access, "T_0004, Format NaN (ru locale)");
      Add_Routine (T, T_0005'Access, "T_0005, Format NaN (fi locale)");
      Add_Routine (T, T_0006'Access, "T_0006, Format Inf (root locale)");
      Add_Routine (T, T_0007'Access, "T_0007, Format -Inf (root locale)");
      Add_Routine (T, T_0008'Access, "T_0008, Format Inf (en locale)");
      Add_Routine (T, T_0009'Access, "T_0009, Format -Inf (en locale)");
      Add_Routine (T, T_0010'Access, "T_0010, Format Inf (ar locale)");
      Add_Routine (T, T_0011'Access, "T_0011, Format -Inf (ar locale)");
      Add_Routine (T, T_0012'Access, "T_0012, Format Inf (ru locale)");
      Add_Routine (T, T_0013'Access, "T_0013, Format -Inf (ru locale)");
      Add_Routine (T, T_0014'Access, "T_0014, Format Inf (fi locale)");
      Add_Routine (T, T_0015'Access, "T_0015, Format -Inf (fi locale)");
      Add_Routine (T, T_0016'Access, "T_0016, Format 1.23456789e-9");
      Add_Routine (T, T_0017'Access, "T_0017, Format 1.23456789e-8");
      Add_Routine (T, T_0018'Access, "T_0018, Format 1.23456789e-7");
      Add_Routine (T, T_0019'Access, "T_0019, Format 1.23456789e-6");
      Add_Routine (T, T_0020'Access, "T_0020, Format 1.23456789e-5");
      Add_Routine (T, T_0021'Access, "T_0021, Format 1.23456789e-4");
      Add_Routine (T, T_0022'Access, "T_0022, Format 1.23456789e-3");
      Add_Routine (T, T_0023'Access, "T_0023, Format 1.23456789e-2");
      Add_Routine (T, T_0024'Access, "T_0024, Format 1.23456789e-1");
      Add_Routine (T, T_0025'Access, "T_0025, Format 1.23456789e+0");
      Add_Routine (T, T_0026'Access, "T_0026, Format 1.23456789e+1");
      Add_Routine (T, T_0027'Access, "T_0027, Format 1.23456789e+2");
      Add_Routine (T, T_0028'Access, "T_0028, Format 1.23456789e+3");
      Add_Routine (T, T_0029'Access, "T_0029, Format 1.23456789e+4");
      Add_Routine (T, T_0030'Access, "T_0030, Format 1.23456789e+5");
      Add_Routine (T, T_0031'Access, "T_0031, Format 1.23456789e+6");
      Add_Routine (T, T_0032'Access, "T_0032, Format 1.23456789e+7");
      Add_Routine (T, T_0033'Access, "T_0033, Format 1.23456789e+8");
      Add_Routine (T, T_0034'Access, "T_0034, Format 1.23456789e+9");
      Add_Routine (T, T_0035'Access, "T_0035, Format 1.23456789e-9 w/prec 5");
      Add_Routine (T, T_0036'Access, "T_0036, Format 1.23456789e-8 w/prec 5");
      Add_Routine (T, T_0037'Access, "T_0037, Format 1.23456789e-7 w/prec 5");
      Add_Routine (T, T_0038'Access, "T_0038, Format 1.23456789e-6 w/prec 5");
      Add_Routine (T, T_0039'Access, "T_0039, Format 1.23456789e-5 w/prec 5");
      Add_Routine (T, T_0040'Access, "T_0040, Format 1.23456789e-4 w/prec 5");
      Add_Routine (T, T_0041'Access, "T_0041, Format 1.23456789e-3 w/prec 5");
      Add_Routine (T, T_0042'Access, "T_0042, Format 1.23456789e-2 w/prec 5");
      Add_Routine (T, T_0043'Access, "T_0043, Format 1.23456789e-1 w/prec 5");
      Add_Routine (T, T_0044'Access, "T_0044, Format 1.23456789e+0 w/prec 5");
      Add_Routine (T, T_0045'Access, "T_0045, Format 1.23456789e+1 w/prec 5");
      Add_Routine (T, T_0046'Access, "T_0046, Format 1.23456789e+2 w/prec 5");
      Add_Routine (T, T_0047'Access, "T_0047, Format 1.23456789e+3 w/prec 5");
      Add_Routine (T, T_0048'Access, "T_0048, Format 1.23456789e+4 w/prec 5");
      Add_Routine (T, T_0049'Access, "T_0049, Format 1.23456789e+5 w/prec 5");
      Add_Routine (T, T_0050'Access, "T_0050, Format 1.23456789e+6 w/prec 5");
      Add_Routine (T, T_0051'Access, "T_0051, Format 1.23456789e+7 w/prec 5");
      Add_Routine (T, T_0052'Access, "T_0052, Format 1.23456789e+8 w/prec 5");
      Add_Routine (T, T_0053'Access, "T_0053, Format 1.23456789e+9 w/prec 5");
      Add_Routine (T, T_0054'Access, "T_0054, Format 7.777e-9 w/prec 5");
      Add_Routine (T, T_0055'Access, "T_0055, Format 7.777e-8 w/prec 5");
      Add_Routine (T, T_0056'Access, "T_0056, Format 7.777e-7 w/prec 5");
      Add_Routine (T, T_0057'Access, "T_0057, Format 7.777e-6 w/prec 5");
      Add_Routine (T, T_0058'Access, "T_0058, Format 7.777e-5 w/prec 5");
      Add_Routine (T, T_0059'Access, "T_0059, Format 7.777e-4 w/prec 5");
      Add_Routine (T, T_0060'Access, "T_0060, Format 7.777e-3 w/prec 5");
      Add_Routine (T, T_0061'Access, "T_0061, Format 7.777e-2 w/prec 5");
      Add_Routine (T, T_0062'Access, "T_0062, Format 7.777e-1 w/prec 5");
      Add_Routine (T, T_0063'Access, "T_0063, Format 7.777e+0 w/prec 5");
      Add_Routine (T, T_0064'Access, "T_0064, Format 7.777e+1 w/prec 5");
      Add_Routine (T, T_0065'Access, "T_0065, Format 7.777e+2 w/prec 5");
      Add_Routine (T, T_0066'Access, "T_0066, Format 7.777e+3 w/prec 5");
      Add_Routine (T, T_0067'Access, "T_0067, Format 7.777e+4 w/prec 5");
      Add_Routine (T, T_0068'Access, "T_0068, Format 7.777e+5 w/prec 5");
      Add_Routine (T, T_0069'Access, "T_0069, Format 7.777e+6 w/prec 5");
      Add_Routine (T, T_0070'Access, "T_0070, Format 7.777e+7 w/prec 5");
      Add_Routine (T, T_0071'Access, "T_0071, Format 7.777e+8 w/prec 5");
      Add_Routine (T, T_0072'Access, "T_0072, Format 7.777e+9 w/prec 5");
      Add_Routine (T, T_0073'Access, "T_0073, Format 9.999e-9 w/prec 5");
      Add_Routine (T, T_0074'Access, "T_0074, Format 9.999e-8 w/prec 5");
      Add_Routine (T, T_0075'Access, "T_0075, Format 9.999e-7 w/prec 5");
      Add_Routine (T, T_0076'Access, "T_0076, Format 9.999e-6 w/prec 5");
      Add_Routine (T, T_0077'Access, "T_0077, Format 9.999e-5 w/prec 5");
      Add_Routine (T, T_0078'Access, "T_0078, Format 9.999e-4 w/prec 5");
      Add_Routine (T, T_0079'Access, "T_0079, Format 9.999e-3 w/prec 5");
      Add_Routine (T, T_0080'Access, "T_0080, Format 9.999e-2 w/prec 5");
      Add_Routine (T, T_0081'Access, "T_0081, Format 9.999e-1 w/prec 5");
      Add_Routine (T, T_0082'Access, "T_0082, Format 9.999e+0 w/prec 5");
      Add_Routine (T, T_0083'Access, "T_0083, Format 9.999e+1 w/prec 5");
      Add_Routine (T, T_0084'Access, "T_0084, Format 9.999e+2 w/prec 5");
      Add_Routine (T, T_0085'Access, "T_0085, Format 9.999e+3 w/prec 5");
      Add_Routine (T, T_0086'Access, "T_0086, Format 9.999e+4 w/prec 5");
      Add_Routine (T, T_0087'Access, "T_0087, Format 9.999e+5 w/prec 5");
      Add_Routine (T, T_0088'Access, "T_0088, Format 9.999e+6 w/prec 5");
      Add_Routine (T, T_0089'Access, "T_0089, Format 9.999e+7 w/prec 5");
      Add_Routine (T, T_0090'Access, "T_0090, Format 9.999e+8 w/prec 5");
      Add_Routine (T, T_0091'Access, "T_0091, Format 9.999e+9 w/prec 5");
   end Register_Tests;

   function Suite return Access_Test_Suite is
      use ZanyBlue.Test.Text.Generic_Floats;
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
   procedure T_0047 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0048 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0049 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0050 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0051 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0052 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0053 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0054 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0055 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0056 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0057 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0058 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0059 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0060 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0061 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0062 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0063 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0064 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0065 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0066 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0067 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0068 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0069 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0070 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0071 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0072 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0073 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0074 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0075 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0076 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0077 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0078 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0079 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0080 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0081 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0082 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0083 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0084 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0085 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0086 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0087 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0088 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0089 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0090 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0091 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Generic_Floats.Format_F.Suites;
