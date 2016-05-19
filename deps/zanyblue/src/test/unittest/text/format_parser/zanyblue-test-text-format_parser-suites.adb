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
with ZanyBlue.Text.Format_Parser;

package body ZanyBlue.Test.Text.Format_Parser.Suites is

   use Ahven.Framework;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Format_Parser;

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

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Format_Parser");
      Add_Test_Routine (T, T_0001'Access, "T_0001, Empty format");
      Add_Test_Routine (T, T_0002'Access, "T_0002, Left align format");
      Add_Test_Routine (T, T_0003'Access, "T_0003, Right align format");
      Add_Test_Routine (T, T_0004'Access, "T_0004, Center align format");
      Add_Test_Routine (T, T_0005'Access, "T_0005, Numeric align format");
      Add_Test_Routine (T, T_0006'Access,
                        "T_0006, Left align, * fill, format");
      Add_Test_Routine (T, T_0007'Access,
                        "T_0007, Right align, * fill, format");
      Add_Test_Routine (T, T_0008'Access,
                        "T_0008, Center align, * fill, format");
      Add_Test_Routine (T, T_0009'Access,
                        "T_0009, Numeric align, * fill, format");
      Add_Test_Routine (T, T_0010'Access, "T_0010, Plus sign format");
      Add_Test_Routine (T, T_0011'Access, "T_0011, Minus sign format");
      Add_Test_Routine (T, T_0012'Access, "T_0012, Space sign format");
      Add_Test_Routine (T, T_0013'Access, "T_0013, Include base format");
      Add_Test_Routine (T, T_0014'Access, "T_0014, Base format");
      Add_Test_Routine (T, T_0015'Access, "T_0015, Base zero fill format");
      Add_Test_Routine (T, T_0016'Access, "T_0016, Width format");
      Add_Test_Routine (T, T_0017'Access, "T_0017, Precision format");
      Add_Test_Routine (T, T_0018'Access,
                        "T_0018, Width and precision format");
      Add_Test_Routine (T, T_0019'Access,
                     "T_0019, Center, width and precision format");
      Add_Test_Routine (T, T_0020'Access,
                     "T_0020, Left, width and precision format");
      Add_Test_Routine (T, T_0021'Access,
                     "T_0021, Right, width and precision format");
      Add_Test_Routine (T, T_0022'Access,
                     "T_0022, Numeric, width and precision format");
      Add_Test_Routine (T, T_0023'Access, "T_0023, Full format spec");
      Add_Test_Routine (T, T_0024'Access, "T_0024, Full format spec with '%'");
      Add_Test_Routine (T, T_0025'Access, "T_0025, Full format spec with 'c'");
      Add_Test_Routine (T, T_0026'Access, "T_0026, Full format spec with 'e'");
      Add_Test_Routine (T, T_0027'Access, "T_0027, Full format spec with 'E'");
      Add_Test_Routine (T, T_0028'Access, "T_0028, Full format spec with 'f'");
      Add_Test_Routine (T, T_0029'Access, "T_0029, Full format spec with 'F'");
      Add_Test_Routine (T, T_0030'Access, "T_0030, Full format spec with 'G'");
      Add_Test_Routine (T, T_0031'Access, "T_0031, Full format spec with 'n'");
      Add_Test_Routine (T, T_0032'Access, "T_0032, Full format spec with 'o'");
      Add_Test_Routine (T, T_0033'Access, "T_0033, Extra characters");
      Add_Test_Routine (T, T_0034'Access, "T_0034, Full format spec with 'T'");
      Add_Test_Routine (T, T_0035'Access, "T_0035, Full format spec with 'D'");
      Add_Test_Routine (T, T_0036'Access, "T_0036, Exeption for illegal char");
      Add_Test_Routine (T, T_0037'Access, "T_0037, Full format spec with 'b'");
      Add_Test_Routine (T, T_0038'Access, "T_0038, Full format spec with 'd'");
      Add_Test_Routine (T, T_0039'Access, "T_0039, Full format spec with 'x'");
      Add_Test_Routine (T, T_0040'Access, "T_0040, Full format spec with 'X'");
      Add_Test_Routine (T, T_0041'Access, "T_0041, Maximum field width");
      Add_Test_Routine (T, T_0042'Access,
                        "T_0042, Resetting maximum field width");
      Add_Test_Routine (T, T_0043'Access, "T_0043, Field width exception");
      Add_Test_Routine (T, T_0044'Access,
                        "T_0044, Field width exception value");
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

end ZanyBlue.Test.Text.Format_Parser.Suites;
