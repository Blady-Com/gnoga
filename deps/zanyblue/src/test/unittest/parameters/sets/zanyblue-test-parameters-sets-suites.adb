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

with ZanyBlue.Parameters.Sets;

package body ZanyBlue.Test.Parameters.Sets.Suites is

   use ZanyBlue.Parameters.Sets;

   Test_Area : constant String := "parameters/sets";

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

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Parameters.Sets");
      Add_Test_Routine (T, T_0001'Access, "T_0001, Set/Get Name");
      Add_Test_Routine (T, T_0002'Access, "T_0002, Number_Of_Parameters");
      Add_Test_Routine (T, T_0003'Access, "T_0003, Clear parameter set");
      Add_Test_Routine (T, T_0004'Access, "T_0004, Append to parameter value");
      Add_Test_Routine (T, T_0005'Access,
                        "T_0005, Prepend to parameter value");
      Add_Test_Routine (T, T_0006'Access,
                        "T_0006, Generic Get parameter value");
      Add_Test_Routine (T, T_0007'Access,
                        "T_0007, Generic Get undef parameter");
      Add_Test_Routine (T, T_0008'Access, "T_0008, Set/Get Boolean");
      Add_Test_Routine (T, T_0009'Access, "T_0009, Set/Get Float");
      Add_Test_Routine (T, T_0010'Access, "T_0010, Set/Get Integer");
      Add_Test_Routine (T, T_0011'Access, "T_0011, Set/Get Time");
      Add_Test_Routine (T, T_0012'Access, "T_0012, Set/Get String");
      Add_Test_Routine (T, T_0013'Access, "T_0013, Type_Name(Boolean)");
      Add_Test_Routine (T, T_0014'Access, "T_0014, Type_Name(Float)");
      Add_Test_Routine (T, T_0015'Access, "T_0015, Type_Name(Integer)");
      Add_Test_Routine (T, T_0016'Access, "T_0016, Type_Name(Time)");
      Add_Test_Routine (T, T_0017'Access, "T_0017, Type_Name(String)");
      Add_Test_Routine (T, T_0018'Access, "T_0018, Increment");
      Add_Test_Routine (T, T_0019'Access, "T_0019, Increment non-Integer");
      Add_Test_Routine (T, T_0020'Access, "T_0020, Increment undefined");
      Add_Test_Routine (T, T_0021'Access, "T_0021, Is_Defined");
      Add_Test_Routine (T, T_0022'Access, "T_0022, Dump parameter set");
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

end ZanyBlue.Test.Parameters.Sets.Suites;
