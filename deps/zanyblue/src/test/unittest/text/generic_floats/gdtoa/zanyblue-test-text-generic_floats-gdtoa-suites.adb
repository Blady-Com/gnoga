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
with ZanyBlue.Text.Generic_Floats;
pragma Elaborate_All (ZanyBlue.Text.Generic_Floats);

package body ZanyBlue.Test.Text.Generic_Floats.GDTOA.Suites is

   use Ahven.Framework;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;

   package Floats is
      new ZanyBlue.Text.Generic_Floats (Float);

   package Long_Floats is
      new ZanyBlue.Text.Generic_Floats (Long_Float);

   procedure Check_Float (T      : in out Test_Case'Class;
                          Value  : Float;
                          Expect : Wide_String);
   procedure Check_LFloat (T      : in out Test_Case'Class;
                           Value  : Long_Float;
                           Expect : Wide_String);

   procedure Check_Float (T      : in out Test_Case'Class;
                          Value  : Float;
                          Expect : Wide_String) is
      use Floats;
      A : constant Float_Argument_Type := Create (Value);
   begin
      Check_Value (T, A.Format ("float", "e", Root_Locale), Expect);
   end Check_Float;

   procedure Check_LFloat (T      : in out Test_Case'Class;
                           Value  : Long_Float;
                           Expect : Wide_String) is
      use Long_Floats;
      A : constant Float_Argument_Type := Create (Value);
   begin
      Check_Value (T, A.Format ("float", "e", Root_Locale), Expect);
   end Check_LFloat;

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

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Generic_Floats.GDTOA");
      Add_Test_Routine (T, T_0001'Access, "T_0001, -1.1");
      Add_Test_Routine (T, T_0002'Access, "T_0002, 1.1");
      Add_Test_Routine (T, T_0003'Access, "T_0003, -1.2");
      Add_Test_Routine (T, T_0004'Access, "T_0004, 1.2");
      Add_Test_Routine (T, T_0005'Access, "T_0005, 1.23");
      Add_Test_Routine (T, T_0006'Access, "T_0006, 1.23456589e+20");
      Add_Test_Routine (T, T_0007'Access, "T_0007, 1.23456789");
      Add_Test_Routine (T, T_0008'Access, "T_0008, 1.234567890123456789");
      Add_Test_Routine (T, T_0009'Access,
                     "T_0009, 1.23456789012345678901234567890123456789");
      Add_Test_Routine (T, T_0010'Access, "T_0010, 1.23456789e-20");
      Add_Test_Routine (T, T_0011'Access, "T_0011, 1.23456789e-30");
      Add_Test_Routine (T, T_0012'Access, "T_0012, 1.23e-20");
      Add_Test_Routine (T, T_0013'Access, "T_0013, 1.23e+20");
      Add_Test_Routine (T, T_0014'Access, "T_0014, 1.23e-30");
      Add_Test_Routine (T, T_0015'Access, "T_0015, 1.23e+30");
      Add_Test_Routine (T, T_0016'Access, "T_0016, -1.3");
      Add_Test_Routine (T, T_0017'Access, "T_0017, 1.3");
      Add_Test_Routine (T, T_0018'Access, "T_0018, -1.4");
      Add_Test_Routine (T, T_0019'Access, "T_0019, 1.4");
      Add_Test_Routine (T, T_0020'Access, "T_0020, -1.5");
      Add_Test_Routine (T, T_0021'Access, "T_0021, 1.5");
      Add_Test_Routine (T, T_0022'Access, "T_0022, -1.6");
      Add_Test_Routine (T, T_0023'Access, "T_0023, 1.6");
      Add_Test_Routine (T, T_0024'Access, "T_0024, -1.7");
      Add_Test_Routine (T, T_0025'Access, "T_0025, 1.7");
      Add_Test_Routine (T, T_0026'Access, "T_0026, -1.8");
      Add_Test_Routine (T, T_0027'Access, "T_0027, 1.8");
      Add_Test_Routine (T, T_0028'Access, "T_0028, -1.9");
      Add_Test_Routine (T, T_0029'Access, "T_0029, 1.9");
      Add_Test_Routine (T, T_0030'Access, "T_0030, 1e23");
      Add_Test_Routine (T, T_0031'Access, "T_0031, 1.23456589e-307");
      Add_Test_Routine (T, T_0032'Access, "T_0032, 1.234567890123456789e301");
      Add_Test_Routine (T, T_0033'Access, "T_0033, 1.234567890123456789e-301");
      Add_Test_Routine (T, T_0034'Access, "T_0034, 1.234567890123456789e-321");
      Add_Test_Routine (T, T_0035'Access, "T_0035, 1.23456789e307");
      Add_Test_Routine (T, T_0036'Access, "T_0036, 1.23e306");
      Add_Test_Routine (T, T_0037'Access, "T_0037, 1.23e-306");
      Add_Test_Routine (T, T_0038'Access, "T_0038, 1.23e-320");
      Add_Test_Routine (T, T_0039'Access, "T_0039, 2.2250738585072013e-308");
      Add_Test_Routine (T, T_0040'Access, "T_0040, 2.2250738585072014e-308");
      Add_Test_Routine (T, T_0041'Access, "T_0041, 9.0259718793241475e-277");
      Add_Test_Routine (T, T_0042'Access,
                     "T_0042, 9.025971879324147880346310405868e-277");
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

end ZanyBlue.Test.Text.Generic_Floats.GDTOA.Suites;
