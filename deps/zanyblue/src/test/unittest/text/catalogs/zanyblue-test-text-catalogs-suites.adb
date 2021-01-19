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
with ZanyBlue.Text.Catalogs;

package body ZanyBlue.Test.Text.Catalogs.Suites is

   use Ahven.Framework;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Locales;

   Test_Area : constant String := "text/catalogs";

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

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Catalogs");
      Add_Test_Routine (T, T_0001'Access, "T_0001, Create test");
      Add_Test_Routine (T, T_0002'Access,
                        "T_0002, Failure to enable indexing");
      Add_Test_Routine (T, T_0003'Access,
                        "T_0003, Indexing too late (facility)");
      Add_Test_Routine (T, T_0004'Access, "T_0004, Indexing one facility");
      Add_Test_Routine (T, T_0005'Access,
                        "T_0005, Indexing multiple facilities");
      Add_Test_Routine (T, T_0006'Access,
                        "T_0006, Index access for facilities");
      Add_Test_Routine (T, T_0007'Access, "T_0007, Indexing too late (keys)");
      Add_Test_Routine (T, T_0008'Access, "T_0008, Number_Of_Keys");
      Add_Test_Routine (T, T_0009'Access, "T_0009, Indexing multiple keys");
      Add_Test_Routine (T, T_0010'Access, "T_0010, Index access for keys");
      Add_Test_Routine (T, T_0011'Access,
                        "T_0011, Indexing too late (locales)");
      Add_Test_Routine (T, T_0012'Access, "T_0012, Indexing one locale");
      Add_Test_Routine (T, T_0013'Access, "T_0013, Indexing multiple locales");
      Add_Test_Routine (T, T_0014'Access, "T_0014, Index access for locales");
      Add_Test_Routine (T, T_0015'Access, "T_0015, Query static pool data");
      Add_Test_Routine (T, T_0016'Access, "T_0016, Query dynamic pool data");
      Add_Test_Routine (T, T_0017'Access,
                        "T_0017, Query static+dynamic pool data");
      Add_Test_Routine (T, T_0018'Access,
                        "T_0018, Message query requires indexes");
      Add_Test_Routine (T, T_0019'Access,
                        "T_0019, Message query for static pool");
      Add_Test_Routine (T, T_0020'Access,
                        "T_0020, Message query for dynamic pool");
      Add_Test_Routine (T, T_0021'Access,
                     "T_0021, Message query static+dynamic pool");
      Add_Test_Routine (T, T_0022'Access,
                     "T_0022, Invalid_Static_Message exception");
      Add_Test_Routine (T, T_0023'Access, "T_0023, Multiple static pools");
      Add_Test_Routine (T, T_0024'Access,
                     "T_0024, Multiple static pools with indexing");
      Add_Test_Routine (T, T_0025'Access,
                        "T_0025, No_Such_Facility exception");
      Add_Test_Routine (T, T_0026'Access, "T_0026, No_Such_Message exception");
      Add_Test_Routine (T, T_0027'Access,
                        "T_0027, Enable_Pseudo_Translations");
      Add_Test_Routine (T, T_0028'Access,
                     "T_0028, No_Such_Facility exception (Indexed)");
      Add_Test_Routine (T, T_0029'Access,
                     "T_0029, No_Such_Message exception (Indexed)");
      Add_Test_Routine (T, T_0030'Access, "T_0030, No_Such_Key exception");
      Add_Test_Routine (T, T_0031'Access,
                     "T_0031, No_Such_Key (Constraint_Error) exception "
                            & "(Indexed)");
      Add_Test_Routine (T, T_0032'Access,
                        "T_0032, Multiple Enable_Indexes calls");
      Add_Test_Routine (T, T_0033'Access, "T_0033, Get_Pseudo_Map");
      Add_Test_Routine (T, T_0034'Access, "T_0034, Query_Message extra keys");
      Add_Test_Routine (T, T_0035'Access, "T_0035, Load_File functionality");
      Add_Test_Routine (T, T_0036'Access,
                        "T_0036, Load_Facility functionality");
      Add_Test_Routine (T, T_0037'Access,
                     "T_0037, Load_Facility functionality, implicit facility");
      Add_Test_Routine (T, T_0038'Access,
                        "T_0038, Number_Of_Messages functionality");
      Add_Test_Routine (T, T_0039'Access, "T_0039, Adding facility names");
      Add_Test_Routine (T, T_0040'Access,
                        "T_0040, zbmcompile Initialize support procedure");
      Add_Test_Routine (T, T_0041'Access, "T_0041, Dump empty catalog");
      Add_Test_Routine (T, T_0042'Access,
                        "T_0042, Dump catalog with 1 message");
      Add_Test_Routine (T, T_0043'Access,
                        "T_0043, Dump catalog with multiple message");
      Add_Test_Routine (T, T_0044'Access, "T_0044, Dump catalog named output");
      Add_Test_Routine (T, T_0045'Access,
                        "T_0045, Load_Facility functionality, no base locale");
      Add_Test_Routine (T, T_0046'Access,
                        "T_0046, Enable/Disable exceptions");
      Add_Test_Routine (T, T_0047'Access,
                        "T_0047, Get_Locale_Name (Catalog, Index)");
      Add_Test_Routine (T, T_0048'Access,
                        "T_0048, Logical vs. Stored pool size");
      Add_Test_Routine (T, T_0049'Access,
                        "T_0049, Pool_Size (Catalog) with Single_Pool");
      Add_Test_Routine (T, T_0050'Access,
                        "T_0050, Pseudo translation formatting");
      Add_Test_Routine (T, T_0051'Access,
                        "T_0051, Pseudo translation without message markers");
      Add_Test_Routine (T, T_0052'Access,
                        "T_0052, Pseudo translation without argument markers");
      Add_Test_Routine (T, T_0053'Access,
                        "T_0053, Pseudo translation without markers");
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

end ZanyBlue.Test.Text.Catalogs.Suites;
