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

with ZanyBlue.Text.Generic_Integers;
pragma Elaborate_All (ZanyBlue.Text.Generic_Integers);

package body ZanyBlue.Test.Text.Generic_Integers.Suites is

   use Ahven.Framework;

   type My_Integer is range -5000 .. 4999;
   package My_Integer_Arguments is
      new ZanyBlue.Text.Generic_Integers (My_Integer);
   use My_Integer_Arguments;

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

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Generic_Integers");
      Add_Test_Routine (T, T_0001'Access, "T_0001, Negative Format");
      Add_Test_Routine (T, T_0002'Access, "T_0002, Test Max/Min Arguments");
      Add_Test_Routine (T, T_0003'Access, "T_0003, Positive Format");
      Add_Test_Routine (T, T_0004'Access, "T_0004, Zero List Argument");
      Add_Test_Routine (T, T_0005'Access, "T_0005, Zero Format");
      Add_Test_Routine (T, T_0006'Access, "T_0006, Positive List Argument");
      Add_Test_Routine (T, T_0007'Access, "T_0007, Negative List Argument");
      Add_Test_Routine (T, T_0008'Access, "T_0008, Binary format");
      Add_Test_Routine (T, T_0009'Access, "T_0009, Octal format");
      Add_Test_Routine (T, T_0010'Access, "T_0010, Hex (lowercase) format");
      Add_Test_Routine (T, T_0011'Access, "T_0011, Hex (uppercase) format");
      Add_Test_Routine (T, T_0012'Access, "T_0012, Format w/ '+'");
      Add_Test_Routine (T, T_0013'Access, "T_0013, Format w/ ' '");
      Add_Test_Routine (T, T_0014'Access, "T_0014, Decorated binary");
      Add_Test_Routine (T, T_0015'Access, "T_0015, Decorated octal");
      Add_Test_Routine (T, T_0016'Access, "T_0016, Decorated hexadecimal");
      Add_Test_Routine (T, T_0017'Access, "T_0017, Decorated negative binary");
      Add_Test_Routine (T, T_0018'Access, "T_0018, Decorated negative octal");
      Add_Test_Routine (T, T_0019'Access,
                        "T_0019, Decorated negative hexadecimal");
      Add_Test_Routine (T, T_0020'Access,
                        "T_0020, Decorated binary, wdith=20");
      Add_Test_Routine (T, T_0021'Access, "T_0021, Decorated octal, wdith=20");
      Add_Test_Routine (T, T_0022'Access, "T_0022, Decorated hex, wdith=20");
      Add_Test_Routine (T, T_0023'Access,
                        "T_0023, Decorated -binary, wdith=20");
      Add_Test_Routine (T, T_0024'Access,
                        "T_0024, Decorated -octal, wdith=20");
      Add_Test_Routine (T, T_0025'Access, "T_0025, Decorated -hex, wdith=20");
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

end ZanyBlue.Test.Text.Generic_Integers.Suites;
