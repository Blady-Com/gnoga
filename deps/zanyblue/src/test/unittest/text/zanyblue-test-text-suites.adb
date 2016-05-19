--  -*- coding: utf-8 -*-
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

with ZanyBlue.Test.Text.Arguments.Suites;
with ZanyBlue.Test.Text.Booleans.Suites;
with ZanyBlue.Test.Text.Buffer.Suites;
with ZanyBlue.Test.Text.Catalogs.Suites;
with ZanyBlue.Test.Text.Characters.Suites;
with ZanyBlue.Test.Text.CLDR.Suites;
with ZanyBlue.Test.Text.Codecs.Suites;
with ZanyBlue.Test.Text.Durations.Suites;
with ZanyBlue.Test.Text.Exceptions.Suites;
with ZanyBlue.Test.Text.Floats.Suites;
with ZanyBlue.Test.Text.Long_Floats.Suites;
with ZanyBlue.Test.Text.Format_Errors.Suites;
with ZanyBlue.Test.Text.Format_Message.Suites;
with ZanyBlue.Test.Text.Format_Parser.Suites;
with ZanyBlue.Test.Text.Formatting.Suites;
with ZanyBlue.Test.Text.Generic_Buffer.Suites;
with ZanyBlue.Test.Text.Generic_Enumerations.Suites;
with ZanyBlue.Test.Text.Generic_Fixed.Suites;
with ZanyBlue.Test.Text.Generic_Floats.Suites;
with ZanyBlue.Test.Text.Generic_Integers.Suites;
with ZanyBlue.Test.Text.Generic_Modulars.Suites;
with ZanyBlue.Test.Text.Integers.Suites;
with ZanyBlue.Test.Text.Locales.Suites;
with ZanyBlue.Test.Text.Metrics.Suites;
with ZanyBlue.Test.Text.Null_Object.Suites;
with ZanyBlue.Test.Text.Properties_Parser.Suites;
with ZanyBlue.Test.Text.Pseudo.Suites;
with ZanyBlue.Test.Text.Strings.Suites;
with ZanyBlue.Test.Text.Times.Suites;
with ZanyBlue.Test.Text.Unbounded_Strings.Suites;
with ZanyBlue.Test.Text.Unbounded_Wide_Strings.Suites;
with ZanyBlue.Test.Text.Utils.Suites;
with ZanyBlue.Test.Text.Wide_Characters.Suites;
with ZanyBlue.Test.Text.Wide_Strings.Suites;

package body ZanyBlue.Test.Text.Suites is

   use Ahven.Framework;

   procedure T_0001 (T : in out Test_Case'Class);
   procedure T_0002 (T : in out Test_Case'Class);

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text");
      Add_Test_Routine (T, T_0001'Access, "T_0004, To_UTF8 ('')");
      Add_Test_Routine (T, T_0002'Access, "T_0005, From_UTF8 ('')");
   end Initialize;

   function Suite return Test_Suite is
   begin
      return S : Test_Suite do
         Add_Static_Test (S, Arguments.Suites.Suite);
         Add_Static_Test (S, Booleans.Suites.Suite);
         Add_Static_Test (S, Buffer.Suites.Suite);
         Add_Static_Test (S, Catalogs.Suites.Suite);
         Add_Static_Test (S, Characters.Suites.Suite);
         Add_Static_Test (S, CLDR.Suites.Suite);
         Add_Static_Test (S, Codecs.Suites.Suite);
         Add_Static_Test (S, Durations.Suites.Suite);
         Add_Static_Test (S, Exceptions.Suites.Suite);
         Add_Static_Test (S, Floats.Suites.Suite);
         Add_Static_Test (S, Long_Floats.Suites.Suite);
         Add_Static_Test (S, Format_Errors.Suites.Suite);
         Add_Static_Test (S, Format_Message.Suites.Suite);
         Add_Static_Test (S, Format_Parser.Suites.Suite);
         Add_Static_Test (S, Formatting.Suites.Suite);
         Add_Static_Test (S, Generic_Buffer.Suites.Suite);
         Add_Static_Test (S, Generic_Enumerations.Suites.Suite);
         Add_Static_Test (S, Generic_Fixed.Suites.Suite);
         Add_Static_Test (S, Generic_Floats.Suites.Suite);
         Add_Static_Test (S, Generic_Integers.Suites.Suite);
         Add_Static_Test (S, Generic_Modulars.Suites.Suite);
         Add_Static_Test (S, Integers.Suites.Suite);
         Add_Static_Test (S, Locales.Suites.Suite);
         Add_Static_Test (S, Metrics.Suites.Suite);
         Add_Static_Test (S, Null_Object.Suites.Suite);
         Add_Static_Test (S, Properties_Parser.Suites.Suite);
         Add_Static_Test (S, Pseudo.Suites.Suite);
         Add_Static_Test (S, Strings.Suites.Suite);
         Add_Static_Test (S, Times.Suites.Suite);
         Add_Static_Test (S, Unbounded_Strings.Suites.Suite);
         Add_Static_Test (S, Unbounded_Wide_Strings.Suites.Suite);
         Add_Static_Test (S, Utils.Suites.Suite);
         Add_Static_Test (S, Wide_Characters.Suites.Suite);
         Add_Static_Test (S, Wide_Strings.Suites.Suite);
         Add_Test (S, new Test);
      end return;
   end Suite;

   procedure T_0001 (T : in out Test_Case'Class) is separate;
   procedure T_0002 (T : in out Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Suites;
