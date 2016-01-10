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

separate (ZanyBlue.Test.Text.Catalogs.Suites)
procedure T_0049 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L_Root         : constant Locale_Type := Make_Locale ("");
   Catalog        : Catalog_Type;

begin
   Catalog := Create;
   Use_Single_Pool (Catalog);
   Add (Catalog, "myfac1", "mykey1", "msg1", L_Root);
   WAssert (R, Pool_Size (Catalog) = 4, "Expected Pool Size of 4");
   Add (Catalog, "myfac1", "mykey2", "msg2", L_Root);
   WAssert (R, Pool_Size (Catalog) = 8, "Expected Pool Size of 8");
   Add (Catalog, "myfac1", "mykey3", "msg3", L_Root);
   WAssert (R, Pool_Size (Catalog) = 12, "Expected Pool Size of 12");
   Add (Catalog, "myfac1", "mykey4", "msg4", L_Root);
   WAssert (R, Pool_Size (Catalog) = 16, "Expected Pool Size of 16");
end T_0049;
