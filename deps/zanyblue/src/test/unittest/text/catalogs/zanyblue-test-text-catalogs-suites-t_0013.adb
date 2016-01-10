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
procedure T_0013 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L_en_US      : constant Locale_Type := Make_Locale ("en_US");
   L_fr_FR      : constant Locale_Type := Make_Locale ("fr_FR");
   L_en         : constant Locale_Type := Make_Locale ("en");
   L_fr         : constant Locale_Type := Make_Locale ("fr");
   L            : constant Locale_Type := Make_Locale ("");
   Catalog     : Catalog_Type;

begin
   Catalog := Create;
   Use_Single_Pool (Catalog);
   WAssert (R, Number_Of_Locales (Catalog) = 0, "Expected 0 locales");
   Add (Catalog, "myfac1", "mykey1", "myfac1/mykey1/en_US", L_en_US);
   WAssert (R, Number_Of_Locales (Catalog) = 1, "Expected 1 locale");
   Add (Catalog, "myfac2", "mykey2", "myfac2/mykey2/L_fr_FR", L_fr_FR);
   WAssert (R, Number_Of_Locales (Catalog) = 2, "Expected 2 locales");
   Add (Catalog, "myfac3", "mykey3", "myfac3/mykey3/L_en", L_en);
   WAssert (R, Number_Of_Locales (Catalog) = 3, "Expected 3 locales");
   Add (Catalog, "myfac4", "mykey4", "myfac4/mykey4/L_fr", L_fr);
   WAssert (R, Number_Of_Locales (Catalog) = 4, "Expected 4 locales");
   Add (Catalog, "myfac5", "mykey5", "myfac5/mykey5", L);
   WAssert (R, Number_Of_Locales (Catalog) = 5, "Expected 5 locales");
end T_0013;
