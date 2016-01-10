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

with ZanyBlue.Parameters.Values;

package body ZanyBlue.Test.Parameters.Values.Suites is

   use AUnit;
   use ZanyBlue.Parameters;
   use ZanyBlue.Parameters.Values;

   Test_Area : constant Wide_String := "parameters/values";

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

   ----------
   -- Name --
   ----------

   overriding
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("ZanyBlue.Parameters.Values");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, to/from Boolean");
      Add_Routine (T, T_0002'Access, "T_0002, to/from Float");
      Add_Routine (T, T_0003'Access, "T_0003, to/from Integer");
      Add_Routine (T, T_0004'Access, "T_0004, to/from List");
      Add_Routine (T, T_0005'Access, "T_0005, to/from String");
      Add_Routine (T, T_0006'Access, "T_0006, to/from Time");
      Add_Routine (T, T_0007'Access, "T_0007, typename(Boolean)");
      Add_Routine (T, T_0008'Access, "T_0008, typename(Float)");
      Add_Routine (T, T_0009'Access, "T_0009, typename(Integer)");
      Add_Routine (T, T_0010'Access, "T_0010, typename(List)");
      Add_Routine (T, T_0011'Access, "T_0011, typename(String)");
      Add_Routine (T, T_0012'Access, "T_0012, typename(String)");
      Add_Routine (T, T_0013'Access, "T_0013, Increment(Boolean)");
      Add_Routine (T, T_0014'Access, "T_0014, Increment(Float)");
      Add_Routine (T, T_0015'Access, "T_0015, Increment(Integer)");
      Add_Routine (T, T_0016'Access, "T_0016, Increment(List)");
      Add_Routine (T, T_0017'Access, "T_0017, Increment(String)");
      Add_Routine (T, T_0018'Access, "T_0018, Increment(Time)");
      Add_Routine (T, T_0019'Access, "T_0019, Dump(Boolean)");
      Add_Routine (T, T_0020'Access, "T_0020, Dump(Float)");
      Add_Routine (T, T_0021'Access, "T_0021, Dump(Integer)");
      Add_Routine (T, T_0022'Access, "T_0022, Dump(List)");
      Add_Routine (T, T_0023'Access, "T_0023, Dump(String)");
      Add_Routine (T, T_0024'Access, "T_0024, Dump(Time)");
      Add_Routine (T, T_0025'Access, "T_0025, to Float/from Boolean");
      Add_Routine (T, T_0026'Access, "T_0026, to Integer/from Boolean");
      Add_Routine (T, T_0027'Access, "T_0027, to List/from Boolean");
      Add_Routine (T, T_0028'Access, "T_0028, to String/from Boolean");
      Add_Routine (T, T_0029'Access, "T_0029, to Time/from Boolean");
      Add_Routine (T, T_0030'Access, "T_0030, to Boolean/from Float");
      Add_Routine (T, T_0031'Access, "T_0031, to Integer/from Float");
      Add_Routine (T, T_0032'Access, "T_0032, to List/from Float");
      Add_Routine (T, T_0033'Access, "T_0033, to String/from Float");
      Add_Routine (T, T_0034'Access, "T_0034, to Time/from Float");
      Add_Routine (T, T_0035'Access, "T_0035, to Boolean/from Integer");
      Add_Routine (T, T_0036'Access, "T_0036, to Float/from Integer");
      Add_Routine (T, T_0037'Access, "T_0037, to List/from Integer");
      Add_Routine (T, T_0038'Access, "T_0038, to String/from Integer");
      Add_Routine (T, T_0039'Access, "T_0039, to Time/from Integer");
      Add_Routine (T, T_0040'Access, "T_0040, to Boolean/from List");
      Add_Routine (T, T_0041'Access, "T_0041, to Float/from List");
      Add_Routine (T, T_0042'Access, "T_0042, to Integer/from List");
      Add_Routine (T, T_0043'Access, "T_0043, to String/from List");
      Add_Routine (T, T_0044'Access, "T_0044, to Time/from List");
      Add_Routine (T, T_0045'Access, "T_0045, to Boolean/from String");
      Add_Routine (T, T_0046'Access, "T_0046, to Float/from String");
      Add_Routine (T, T_0047'Access, "T_0047, to Integer/from String");
      Add_Routine (T, T_0048'Access, "T_0048, to List/from String");
      Add_Routine (T, T_0049'Access, "T_0049, to Time/from String");
      Add_Routine (T, T_0050'Access, "T_0050, to Boolean/from Time");
      Add_Routine (T, T_0051'Access, "T_0051, to Float/from Time");
      Add_Routine (T, T_0052'Access, "T_0052, to Integer/from Time");
      Add_Routine (T, T_0053'Access, "T_0053, to List/from Time");
      Add_Routine (T, T_0054'Access, "T_0054, to String/from Time");
      Add_Routine (T, T_0055'Access, "T_0055, List.To_String multiple elems");
   end Register_Tests;

   -----------
   -- Suite --
   -----------

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
   procedure T_0047 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0048 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0049 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0050 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0051 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0052 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0053 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0054 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0055 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;

end ZanyBlue.Test.Parameters.Values.Suites;
