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

with AUnit;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;

package body ZanyBlue.Test.Text.Catalogs.Suites is

   use AUnit;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Locales;

   Test_Area : constant Wide_String := "text/catalogs";

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

   overriding
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("ZanyBlue.Text.Catalogs");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, Create test");
      Add_Routine (T, T_0002'Access, "T_0002, Failure to enable indexing");
      Add_Routine (T, T_0003'Access, "T_0003, Indexing too late (facility)");
      Add_Routine (T, T_0004'Access, "T_0004, Indexing one facility");
      Add_Routine (T, T_0005'Access, "T_0005, Indexing multiple facilities");
      Add_Routine (T, T_0006'Access, "T_0006, Index access for facilities");
      Add_Routine (T, T_0007'Access, "T_0007, Indexing too late (keys)");
      Add_Routine (T, T_0008'Access, "T_0008, Number_Of_Keys");
      Add_Routine (T, T_0009'Access, "T_0009, Indexing multiple keys");
      Add_Routine (T, T_0010'Access, "T_0010, Index access for keys");
      Add_Routine (T, T_0011'Access, "T_0011, Indexing too late (locales)");
      Add_Routine (T, T_0012'Access, "T_0012, Indexing one locale");
      Add_Routine (T, T_0013'Access, "T_0013, Indexing multiple locales");
      Add_Routine (T, T_0014'Access, "T_0014, Index access for locales");
      Add_Routine (T, T_0015'Access, "T_0015, Query static pool data");
      Add_Routine (T, T_0016'Access, "T_0016, Query dynamic pool data");
      Add_Routine (T, T_0017'Access, "T_0017, Query static+dynamic pool data");
      Add_Routine (T, T_0018'Access, "T_0018, Message query requires indexes");
      Add_Routine (T, T_0019'Access, "T_0019, Message query for static pool");
      Add_Routine (T, T_0020'Access, "T_0020, Message query for dynamic pool");
      Add_Routine (T, T_0021'Access,
                     "T_0021, Message query static+dynamic pool");
      Add_Routine (T, T_0022'Access,
                     "T_0022, Invalid_Static_Message exception");
      Add_Routine (T, T_0023'Access, "T_0023, Multiple static pools");
      Add_Routine (T, T_0024'Access,
                     "T_0024, Multiple static pools with indexing");
      Add_Routine (T, T_0025'Access, "T_0025, No_Such_Facility exception");
      Add_Routine (T, T_0026'Access, "T_0026, No_Such_Message exception");
      Add_Routine (T, T_0027'Access, "T_0027, Enable_Pseudo_Translations");
      Add_Routine (T, T_0028'Access,
                     "T_0028, No_Such_Facility exception (Indexed)");
      Add_Routine (T, T_0029'Access,
                     "T_0029, No_Such_Message exception (Indexed)");
      Add_Routine (T, T_0030'Access, "T_0030, No_Such_Key exception");
      Add_Routine (T, T_0031'Access,
                     "T_0031, No_Such_Key (Constraint_Error) exception "
                            & "(Indexed)");
      Add_Routine (T, T_0032'Access, "T_0032, Multiple Enable_Indexes calls");
      Add_Routine (T, T_0033'Access, "T_0033, Get_Pseudo_Map");
      Add_Routine (T, T_0034'Access, "T_0034, Query_Message extra keys");
      Add_Routine (T, T_0035'Access, "T_0035, Load_File functionality");
      Add_Routine (T, T_0036'Access, "T_0036, Load_Facility functionality");
      Add_Routine (T, T_0037'Access,
                     "T_0037, Load_Facility functionality, implicit facility");
      Add_Routine (T, T_0038'Access,
                     "T_0038, Number_Of_Messages functionality");
      Add_Routine (T, T_0039'Access, "T_0039, Adding facility names");
      Add_Routine (T, T_0040'Access,
                     "T_0040, zbmcompile Initialize support procedure");
      Add_Routine (T, T_0041'Access, "T_0041, Dump empty catalog");
      Add_Routine (T, T_0042'Access, "T_0042, Dump catalog with 1 message");
      Add_Routine (T, T_0043'Access,
                     "T_0043, Dump catalog with multiple message");
      Add_Routine (T, T_0044'Access, "T_0044, Dump catalog named output");
      Add_Routine (T, T_0045'Access,
                     "T_0045, Load_Facility functionality, no base locale");
      Add_Routine (T, T_0046'Access,
                     "T_0046, Enable/Disable exceptions");
      Add_Routine (T, T_0047'Access,
                     "T_0047, Get_Locale_Name (Catalog, Index)");
      Add_Routine (T, T_0048'Access,
                     "T_0048, Logical vs. Stored pool size");
      Add_Routine (T, T_0049'Access,
                     "T_0049, Pool_Size (Catalog) with Single_Pool");
      Add_Routine (T, T_0050'Access,
                     "T_0050, Pseudo translation formatting");
      Add_Routine (T, T_0051'Access,
                     "T_0051, Pseudo translation without message markers");
      Add_Routine (T, T_0052'Access,
                     "T_0052, Pseudo translation without argument markers");
      Add_Routine (T, T_0053'Access,
                     "T_0053, Pseudo translation without markers");
   end Register_Tests;

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

end ZanyBlue.Test.Text.Catalogs.Suites;
