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

   use ZanyBlue.Test.Text;

   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, Arguments.Suites.Suite);
      Add_Test (Result, Booleans.Suites.Suite);
      Add_Test (Result, Buffer.Suites.Suite);
      Add_Test (Result, Catalogs.Suites.Suite);
      Add_Test (Result, Characters.Suites.Suite);
      Add_Test (Result, CLDR.Suites.Suite);
      Add_Test (Result, Codecs.Suites.Suite);
      Add_Test (Result, Durations.Suites.Suite);
      Add_Test (Result, Exceptions.Suites.Suite);
      Add_Test (Result, Floats.Suites.Suite);
      Add_Test (Result, Long_Floats.Suites.Suite);
      Add_Test (Result, Format_Errors.Suites.Suite);
      Add_Test (Result, Format_Message.Suites.Suite);
      Add_Test (Result, Format_Parser.Suites.Suite);
      Add_Test (Result, Formatting.Suites.Suite);
      Add_Test (Result, Generic_Buffer.Suites.Suite);
      Add_Test (Result, Generic_Enumerations.Suites.Suite);
      Add_Test (Result, Generic_Fixed.Suites.Suite);
      Add_Test (Result, Generic_Floats.Suites.Suite);
      Add_Test (Result, Generic_Integers.Suites.Suite);
      Add_Test (Result, Generic_Modulars.Suites.Suite);
      Add_Test (Result, Integers.Suites.Suite);
      Add_Test (Result, Locales.Suites.Suite);
      Add_Test (Result, Metrics.Suites.Suite);
      Add_Test (Result, Null_Object.Suites.Suite);
      Add_Test (Result, Properties_Parser.Suites.Suite);
      Add_Test (Result, Pseudo.Suites.Suite);
      Add_Test (Result, Strings.Suites.Suite);
      Add_Test (Result, Times.Suites.Suite);
      Add_Test (Result, Unbounded_Strings.Suites.Suite);
      Add_Test (Result, Unbounded_Wide_Strings.Suites.Suite);
      Add_Test (Result, Utils.Suites.Suite);
      Add_Test (Result, Wide_Characters.Suites.Suite);
      Add_Test (Result, Wide_Strings.Suites.Suite);
      return Result;
   end Suite;

end ZanyBlue.Test.Text.Suites;
