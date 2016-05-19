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

with ZanyBlue.Test.Text.Codecs.Encoding.ASCII.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP874.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP932.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP936.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP949.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP950.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP1251.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP1252.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP1253.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP1254.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP1255.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP1256.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP1257.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.CP1258.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.BIG5.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.GB2312.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_1.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_2.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_3.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_4.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_5.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_6.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_7.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_8.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_9.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_10.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_11.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_13.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_14.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_15.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_16.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.SHIFT_JIS.Suites;
with ZanyBlue.Test.Text.Codecs.Encoding.UTF_8.Suites;

package body ZanyBlue.Test.Text.Codecs.Encoding.Suites is

   use Ahven;

   procedure T_0001 (T : in out Test_Case'Class);
   procedure T_0002 (T : in out Test_Case'Class);

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Codecs.Encoding");
      Add_Test_Routine (T, T_0001'Access, "T_0001, Invalid encoding");
      Add_Test_Routine (T, T_0002'Access,
                        "T_0002, Unsupported_Encoding exception");
   end Initialize;

   function Suite return Test_Suite is
   begin
      return S : Test_Suite do
         Add_Static_Test (S, ASCII.Suites.Suite);
         Add_Static_Test (S, CP874.Suites.Suite);
         Add_Static_Test (S, CP932.Suites.Suite);
         Add_Static_Test (S, CP936.Suites.Suite);
         Add_Static_Test (S, CP949.Suites.Suite);
         Add_Static_Test (S, CP950.Suites.Suite);
         Add_Static_Test (S, CP1251.Suites.Suite);
         Add_Static_Test (S, CP1252.Suites.Suite);
         Add_Static_Test (S, CP1253.Suites.Suite);
         Add_Static_Test (S, CP1254.Suites.Suite);
         Add_Static_Test (S, CP1255.Suites.Suite);
         Add_Static_Test (S, CP1256.Suites.Suite);
         Add_Static_Test (S, CP1257.Suites.Suite);
         Add_Static_Test (S, CP1258.Suites.Suite);
         Add_Static_Test (S, BIG5.Suites.Suite);
         Add_Static_Test (S, GB2312.Suites.Suite);
         Add_Static_Test (S, ISO8859_1.Suites.Suite);
         Add_Static_Test (S, ISO8859_2.Suites.Suite);
         Add_Static_Test (S, ISO8859_3.Suites.Suite);
         Add_Static_Test (S, ISO8859_4.Suites.Suite);
         Add_Static_Test (S, ISO8859_5.Suites.Suite);
         Add_Static_Test (S, ISO8859_6.Suites.Suite);
         Add_Static_Test (S, ISO8859_7.Suites.Suite);
         Add_Static_Test (S, ISO8859_8.Suites.Suite);
         Add_Static_Test (S, ISO8859_9.Suites.Suite);
         Add_Static_Test (S, ISO8859_10.Suites.Suite);
         Add_Static_Test (S, ISO8859_11.Suites.Suite);
         Add_Static_Test (S, ISO8859_13.Suites.Suite);
         Add_Static_Test (S, ISO8859_14.Suites.Suite);
         Add_Static_Test (S, ISO8859_15.Suites.Suite);
         Add_Static_Test (S, ISO8859_16.Suites.Suite);
         Add_Static_Test (S, SHIFT_JIS.Suites.Suite);
         Add_Static_Test (S, UTF_8.Suites.Suite);
         Add_Test (S, new Test);
      end return;
   end Suite;

   procedure T_0001 (T : in out Test_Case'Class) is separate;
   procedure T_0002 (T : in out Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Codecs.Encoding.Suites;
