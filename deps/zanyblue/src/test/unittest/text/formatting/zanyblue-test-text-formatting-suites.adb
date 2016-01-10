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
with Ada.Wide_Text_IO;
with ZanyBlue.OS;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Formatting;

package body ZanyBlue.Test.Text.Formatting.Suites is

   use AUnit;
   use Ada.Wide_Text_IO;
   use ZanyBlue.OS;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Formatting;

   Test_Area : constant Wide_String := "text/formatting";

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
   procedure T_0092 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0093 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0094 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0095 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0096 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0097 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0098 (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure T_0099 (R : in out AUnit.Test_Cases.Test_Case'Class);

   overriding
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("ZanyBlue.Text.Formatting");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, Raise_Exception, regular/1");
      Add_Routine (T, T_0002'Access, "T_0002, Raise_Exception, format/5");
      Add_Routine (T, T_0003'Access, "T_0003, Raise_Exception, regular/2");
      Add_Routine (T, T_0004'Access, "T_0004, Raise_Exception, regular/3");
      Add_Routine (T, T_0005'Access, "T_0005, Raise_Exception, regular/4");
      Add_Routine (T, T_0006'Access, "T_0006, Raise_Exception, regular/5");
      Add_Routine (T, T_0007'Access, "T_0007, Raise_Exception, format/1");
      Add_Routine (T, T_0008'Access, "T_0008, Raise_Exception, format/2");
      Add_Routine (T, T_0009'Access, "T_0009, Raise_Exception, format/3");
      Add_Routine (T, T_0010'Access, "T_0010, Raise_Exception, format/4");
      Add_Routine (T, T_0011'Access, "T_0011, File procedure renamings");
      Add_Routine (T, T_0012'Access, "T_0012, Std File procedure renaming");
      Add_Routine (T, T_0013'Access, "T_0013, Standard catalog exists");
      Add_Routine (T, T_0014'Access, "T_0014, Formatting with argument list");
      Add_Routine (T, T_0015'Access, "T_0015, Formatting with no arguments");
      Add_Routine (T, T_0016'Access, "T_0016, Formatting with 1 argument");
      Add_Routine (T, T_0017'Access, "T_0017, Formatting with 2 arguments");
      Add_Routine (T, T_0018'Access, "T_0018, Formatting with 3 arguments");
      Add_Routine (T, T_0019'Access, "T_0019, Formatting with 4 arguments");
      Add_Routine (T, T_0020'Access, "T_0020, Formatting with 5 arguments");
      Add_Routine (T, T_0021'Access, "T_0021, Print_Line with argument list");
      Add_Routine (T, T_0022'Access, "T_0022, Print_Line with no arguments");
      Add_Routine (T, T_0023'Access, "T_0023, Print_Line with 1 argument");
      Add_Routine (T, T_0024'Access, "T_0024, Print_Line with 2 arguments");
      Add_Routine (T, T_0025'Access, "T_0025, Print_Line with 3 arguments");
      Add_Routine (T, T_0026'Access, "T_0026, Print_Line with 4 arguments");
      Add_Routine (T, T_0027'Access, "T_0027, Print_Line with 5 arguments");
      Add_Routine (T, T_0028'Access, "T_0028, Print_Line/D w/ argument list");
      Add_Routine (T, T_0029'Access, "T_0029, Print_Line/D with no arguments");
      Add_Routine (T, T_0030'Access, "T_0030, Print_Line/D with 1 argument");
      Add_Routine (T, T_0031'Access, "T_0031, Print_Line/D with 2 arguments");
      Add_Routine (T, T_0032'Access, "T_0032, Print_Line/D with 3 arguments");
      Add_Routine (T, T_0033'Access, "T_0033, Print_Line/D with 4 arguments");
      Add_Routine (T, T_0034'Access, "T_0034, Print_Line/D with 5 arguments");
      Add_Routine (T, T_0035'Access,
                     "T_0035, Print_Line/F with argument list");
      Add_Routine (T, T_0036'Access, "T_0036, Print_Line/F with no arguments");
      Add_Routine (T, T_0037'Access, "T_0037, Print_Line/F with 1 argument");
      Add_Routine (T, T_0038'Access, "T_0038, Print_Line/F with 2 argument");
      Add_Routine (T, T_0039'Access, "T_0039, Print_Line/F with 3 argument");
      Add_Routine (T, T_0040'Access, "T_0040, Print_Line/F with 4 argument");
      Add_Routine (T, T_0041'Access, "T_0041, Print_Line/F with 5 argument");
      Add_Routine (T, T_0042'Access,
                     "T_0042, Print_Line/DF with argument list");
      Add_Routine (T, T_0043'Access,
                     "T_0043, Print_Line/DF with no arguments");
      Add_Routine (T, T_0044'Access, "T_0044, Print_Line/DF with 1 argument");
      Add_Routine (T, T_0045'Access, "T_0045, Print_Line/DF with 2 argument");
      Add_Routine (T, T_0046'Access, "T_0046, Print_Line/DF with 3 argument");
      Add_Routine (T, T_0047'Access, "T_0047, Print_Line/DF with 4 argument");
      Add_Routine (T, T_0048'Access, "T_0048, Print_Line/DF with 5 argument");
      Add_Routine (T, T_0049'Access, "T_0049, Formatting exceptions");
      Add_Routine (T, T_0050'Access, "T_0050, Print_Line format exception");
      Add_Routine (T, T_0051'Access, "T_0051, Print_Line/D format exception");
      Add_Routine (T, T_0052'Access, "T_0052, Print_Line/F format exception");
      Add_Routine (T, T_0053'Access, "T_0053, Print_Line/DF format exception");
      Add_Routine (T, T_0054'Access, "T_0054, Format pseudo translation");
      Add_Routine (T, T_0055'Access, "T_0055, Print_Line pseudo translation");
      Add_Routine (T, T_0056'Access,
                     "T_0056, Print_Line/D pseudo translation");
      Add_Routine (T, T_0057'Access, "T_0057, Format pseudo translation");
      Add_Routine (T, T_0058'Access, "T_0058, Print_Line pseudo translation");
      Add_Routine (T, T_0059'Access,
                     "T_0059, Print_Line/D pseudo translation");
      Add_Routine (T, T_0060'Access, "T_0060, Print with argument list");
      Add_Routine (T, T_0061'Access, "T_0061, Print with no arguments");
      Add_Routine (T, T_0062'Access, "T_0062, Print with 1 argument");
      Add_Routine (T, T_0063'Access, "T_0063, Print with 2 arguments");
      Add_Routine (T, T_0064'Access, "T_0064, Print with 3 arguments");
      Add_Routine (T, T_0065'Access, "T_0065, Print with 4 arguments");
      Add_Routine (T, T_0066'Access, "T_0066, Print with 5 arguments");
      Add_Routine (T, T_0067'Access, "T_0067, Print/D with argument list");
      Add_Routine (T, T_0068'Access, "T_0068, Print/D with no arguments");
      Add_Routine (T, T_0069'Access, "T_0069, Print/D with 1 argument");
      Add_Routine (T, T_0070'Access, "T_0070, Print/D with 2 arguments");
      Add_Routine (T, T_0071'Access, "T_0071, Print/D with 3 arguments");
      Add_Routine (T, T_0072'Access, "T_0072, Print/D with 4 arguments");
      Add_Routine (T, T_0073'Access, "T_0073, Print/D with 5 arguments");
      Add_Routine (T, T_0074'Access, "T_0074, Print/F with argument list");
      Add_Routine (T, T_0075'Access, "T_0075, Print/F with no arguments");
      Add_Routine (T, T_0076'Access, "T_0076, Print/F with 1 argument");
      Add_Routine (T, T_0077'Access, "T_0077, Print/F with 2 argument");
      Add_Routine (T, T_0078'Access, "T_0078, Print/F with 3 argument");
      Add_Routine (T, T_0079'Access, "T_0079, Print/F with 4 argument");
      Add_Routine (T, T_0080'Access, "T_0080, Print/F with 5 argument");
      Add_Routine (T, T_0081'Access, "T_0081, Print/DF with argument list");
      Add_Routine (T, T_0082'Access, "T_0082, Print/DF with no arguments");
      Add_Routine (T, T_0083'Access, "T_0083, Print/DF with 1 argument");
      Add_Routine (T, T_0084'Access, "T_0084, Print/DF with 2 argument");
      Add_Routine (T, T_0085'Access, "T_0085, Print/DF with 3 argument");
      Add_Routine (T, T_0086'Access, "T_0086, Print/DF with 4 argument");
      Add_Routine (T, T_0087'Access, "T_0087, Print/DF with 5 argument");
      Add_Routine (T, T_0088'Access, "T_0088, Print format exception");
      Add_Routine (T, T_0089'Access, "T_0089, Print/D format exception");
      Add_Routine (T, T_0090'Access, "T_0090, Print/F format exception");
      Add_Routine (T, T_0091'Access, "T_0091, Print/DF format exception");
      Add_Routine (T, T_0092'Access, "T_0092, Format quoting");
      Add_Routine (T, T_0093'Access, "T_0093, Format repeated quoting");
      Add_Routine (T, T_0094'Access, "T_0094, Format templates");
      Add_Routine (T, T_0095'Access, "T_0095, Invalid_Format exception");
      Add_Routine (T, T_0096'Access, "T_0096, Standard_Catalog exceptions");
      Add_Routine (T, T_0097'Access, "T_0097, Standard_Catalog exceptions");
      Add_Routine (T, T_0098'Access, "T_0098, Raise_Exception, message");
      Add_Routine (T, T_0099'Access, "T_0099, Source message formatting");
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
   procedure T_0092 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0093 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0094 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0095 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0096 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0097 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0098 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;
   procedure T_0099 (R : in out AUnit.Test_Cases.Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Formatting.Suites;
