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

with ZanyBlue.Parameters.Values;

package body ZanyBlue.Test.Parameters.Values.Suites is

   use Ahven.Framework;
   use ZanyBlue.Parameters;
   use ZanyBlue.Parameters.Values;

   Test_Area : constant String := "parameters/values";

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

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Parameters.Values");
      Add_Test_Routine (T, T_0001'Access, "T_0001, to/from Boolean");
      Add_Test_Routine (T, T_0002'Access, "T_0002, to/from Float");
      Add_Test_Routine (T, T_0003'Access, "T_0003, to/from Integer");
      Add_Test_Routine (T, T_0004'Access, "T_0004, to/from List");
      Add_Test_Routine (T, T_0005'Access, "T_0005, to/from String");
      Add_Test_Routine (T, T_0006'Access, "T_0006, to/from Time");
      Add_Test_Routine (T, T_0007'Access, "T_0007, typename(Boolean)");
      Add_Test_Routine (T, T_0008'Access, "T_0008, typename(Float)");
      Add_Test_Routine (T, T_0009'Access, "T_0009, typename(Integer)");
      Add_Test_Routine (T, T_0010'Access, "T_0010, typename(List)");
      Add_Test_Routine (T, T_0011'Access, "T_0011, typename(String)");
      Add_Test_Routine (T, T_0012'Access, "T_0012, typename(String)");
      Add_Test_Routine (T, T_0013'Access, "T_0013, Increment(Boolean)");
      Add_Test_Routine (T, T_0014'Access, "T_0014, Increment(Float)");
      Add_Test_Routine (T, T_0015'Access, "T_0015, Increment(Integer)");
      Add_Test_Routine (T, T_0016'Access, "T_0016, Increment(List)");
      Add_Test_Routine (T, T_0017'Access, "T_0017, Increment(String)");
      Add_Test_Routine (T, T_0018'Access, "T_0018, Increment(Time)");
      Add_Test_Routine (T, T_0019'Access, "T_0019, Dump(Boolean)");
      Add_Test_Routine (T, T_0020'Access, "T_0020, Dump(Float)");
      Add_Test_Routine (T, T_0021'Access, "T_0021, Dump(Integer)");
      Add_Test_Routine (T, T_0022'Access, "T_0022, Dump(List)");
      Add_Test_Routine (T, T_0023'Access, "T_0023, Dump(String)");
      Add_Test_Routine (T, T_0024'Access, "T_0024, Dump(Time)");
      Add_Test_Routine (T, T_0025'Access, "T_0025, to Float/from Boolean");
      Add_Test_Routine (T, T_0026'Access, "T_0026, to Integer/from Boolean");
      Add_Test_Routine (T, T_0027'Access, "T_0027, to List/from Boolean");
      Add_Test_Routine (T, T_0028'Access, "T_0028, to String/from Boolean");
      Add_Test_Routine (T, T_0029'Access, "T_0029, to Time/from Boolean");
      Add_Test_Routine (T, T_0030'Access, "T_0030, to Boolean/from Float");
      Add_Test_Routine (T, T_0031'Access, "T_0031, to Integer/from Float");
      Add_Test_Routine (T, T_0032'Access, "T_0032, to List/from Float");
      Add_Test_Routine (T, T_0033'Access, "T_0033, to String/from Float");
      Add_Test_Routine (T, T_0034'Access, "T_0034, to Time/from Float");
      Add_Test_Routine (T, T_0035'Access, "T_0035, to Boolean/from Integer");
      Add_Test_Routine (T, T_0036'Access, "T_0036, to Float/from Integer");
      Add_Test_Routine (T, T_0037'Access, "T_0037, to List/from Integer");
      Add_Test_Routine (T, T_0038'Access, "T_0038, to String/from Integer");
      Add_Test_Routine (T, T_0039'Access, "T_0039, to Time/from Integer");
      Add_Test_Routine (T, T_0040'Access, "T_0040, to Boolean/from List");
      Add_Test_Routine (T, T_0041'Access, "T_0041, to Float/from List");
      Add_Test_Routine (T, T_0042'Access, "T_0042, to Integer/from List");
      Add_Test_Routine (T, T_0043'Access, "T_0043, to String/from List");
      Add_Test_Routine (T, T_0044'Access, "T_0044, to Time/from List");
      Add_Test_Routine (T, T_0045'Access, "T_0045, to Boolean/from String");
      Add_Test_Routine (T, T_0046'Access, "T_0046, to Float/from String");
      Add_Test_Routine (T, T_0047'Access, "T_0047, to Integer/from String");
      Add_Test_Routine (T, T_0048'Access, "T_0048, to List/from String");
      Add_Test_Routine (T, T_0049'Access, "T_0049, to Time/from String");
      Add_Test_Routine (T, T_0050'Access, "T_0050, to Boolean/from Time");
      Add_Test_Routine (T, T_0051'Access, "T_0051, to Float/from Time");
      Add_Test_Routine (T, T_0052'Access, "T_0052, to Integer/from Time");
      Add_Test_Routine (T, T_0053'Access, "T_0053, to List/from Time");
      Add_Test_Routine (T, T_0054'Access, "T_0054, to String/from Time");
      Add_Test_Routine (T, T_0055'Access,
                        "T_0055, List.To_String multiple elems");
   end Initialize;

   -----------
   -- Suite --
   -----------

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

end ZanyBlue.Test.Parameters.Values.Suites;
