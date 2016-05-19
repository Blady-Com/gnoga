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

separate (ZanyBlue.Test.Text.Catalogs.Suites)
procedure T_0038 (T : in out Test_Case'Class) is

   Catalog : Catalog_Type;
   L1      : constant Locale_Type := Make_Locale ("en_US");
   L2      : constant Locale_Type := Make_Locale ("fr_FR");

begin
   Catalog := Create;
   WAssert (T, Number_Of_Messages (Catalog) = 0, "Expected 0 messages");
   Add (Catalog, "fac1", "key1", "This is fac1/key1", L1);
   WAssert (T, Number_Of_Messages (Catalog) = 1, "Expected 1 message");
   Add (Catalog, "fac1", "key1", "This is different fac1/key1", L1);
   WAssert (T, Number_Of_Messages (Catalog) = 1, "Expected 1 message");
   Add (Catalog, "fac1", "key2", "This is fac1/key2", L1);
   WAssert (T, Number_Of_Messages (Catalog) = 2, "Expected 2 messages");
   Add (Catalog, "fac1", "key3", "This is fac1/key3", L1);
   WAssert (T, Number_Of_Messages (Catalog) = 3, "Expected 3 messages");
   Add (Catalog, "fac2", "key3", "This is fac2/key3", L1);
   WAssert (T, Number_Of_Messages (Catalog) = 4, "Expected 4 messages");
   Add (Catalog, "fac2", "key2", "This is fac2/key2", L1);
   WAssert (T, Number_Of_Messages (Catalog) = 5, "Expected 5 messages");
   Add (Catalog, "fac1", "key3", "This is fac1/key3", L2);
   WAssert (T, Number_Of_Messages (Catalog) = 6, "Expected 6 messages");
   Add (Catalog, "fac2", "key1", "This is fac2/key1", L2);
   WAssert (T, Number_Of_Messages (Catalog) = 7, "Expected 7 messages");
end T_0038;
