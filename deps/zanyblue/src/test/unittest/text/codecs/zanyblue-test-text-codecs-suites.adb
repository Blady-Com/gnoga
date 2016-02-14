--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2016, Michael Rohan <mrohan@zanyblue.com>
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

with AUnit.Assertions;
with ZanyBlue.Text.Codecs;

package body ZanyBlue.Test.Text.Codecs.Suites is

   use AUnit;
   use AUnit.Assertions;

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

   overriding
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("ZanyBlue.Text.Codecs");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Add_Routine (T, T_0001'Access, "T_0001, UTF8 encoding");
      Add_Routine (T, T_0002'Access, "T_0002, ASCII encoding");
      Add_Routine (T, T_0003'Access, "T_0003, ISO8859-1 encoding");
      Add_Routine (T, T_0004'Access, "T_0004, Invalid encoding");
      Add_Routine (T, T_0005'Access, "T_0005, Unsupported_Encoding exception");
      Add_Routine (T, T_0006'Access, "T_0006, ASCII encode");
      Add_Routine (T, T_0007'Access, "T_0007, ASCII encode exception");
      Add_Routine (T, T_0008'Access, "T_0008, ISO8859-1 encode (>7F)");
      Add_Routine (T, T_0009'Access, "T_0009, ISO8859-1 encode (replace)");
      Add_Routine (T, T_0010'Access, "T_0010, ISO8859-1 encode exception");
      Add_Routine (T, T_0011'Access, "T_0011, UTF-8 encode");
      Add_Routine (T, T_0012'Access, "T_0012, UTF-8 encoding name");
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

end ZanyBlue.Test.Text.Codecs.Suites;
