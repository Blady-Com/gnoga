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
procedure T_0047 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   L_en_Latn_US   : constant Locale_Type := Make_Locale ("en", "Latn", "US");
   L_en_US        : constant Locale_Type := Make_Locale ("en", "US");
   L_en           : constant Locale_Type := Make_Locale ("en", "");
   L_Root         : constant Locale_Type := Make_Locale ("");
   Catalog        : Catalog_Type;

begin
   Catalog := Create;
   Add (Catalog, "myfac1", "mykey1", "msg: en_Latn_US", L_en_Latn_US);
   Check_Value (R, Get_Locale_Name (Catalog, 1), "en_Latn_US");
   Add (Catalog, "myfac1", "mykey1", "msg: en_US", L_en_US);
   Check_Value (R, Get_Locale_Name (Catalog, 2), "en_US");
   Add (Catalog, "myfac1", "mykey1", "msg: en", L_en);
   Check_Value (R, Get_Locale_Name (Catalog, 3), "en");
   Add (Catalog, "myfac1", "mykey1", "msg: Root", L_Root);
   Check_Value (R, Get_Locale_Name (Catalog, 4), "");
end T_0047;
