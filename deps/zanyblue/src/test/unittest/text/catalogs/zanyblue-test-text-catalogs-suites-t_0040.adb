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

with ZanyBlue.Test.Text.Catalogs.Xmpl_Data3;

separate (ZanyBlue.Test.Text.Catalogs.Suites)
procedure T_0040 (T : in out Test_Case'Class) is

   use ZanyBlue.Test.Text.Catalogs.Xmpl_Data3;

   Catalog : constant Catalog_Type := Create;
   base : constant Locale_Type := Make_Locale ("");
   fr   : constant Locale_Type := Make_Locale ("fr");
   he   : constant Locale_Type := Make_Locale ("he");

begin
   WAssert (T, Number_Of_Facilities (Catalog) = 0, "Expected 0 facilities");
   begin
      Initialize (Catalog);
   exception
   when others =>
      WAssert (T, False, "Unexpected exception for Initialize");
   end;
   WAssert (T, Number_Of_Facilities (Catalog) = 2, "Expected 2 facilities");
   Check_Value (T, Get_Facility (Catalog, 1), "f1", "Expected 'f1'");
   Check_Value (T, Get_Facility (Catalog, 2), "f2", "Expected 'f2'");
   WAssert (T, Number_Of_Keys (Catalog) = 2, "Expected 2 keys");
   Check_Value (T, Get_Key (Catalog, 1), "01", "Expected '01'");
   Check_Value (T, Get_Key (Catalog, 2), "02", "Expected '02'");

   Check_Value (T, Get_Text (Catalog, "f1", "01", base),
                "This fac1 message 0001",
                "Expected 'f1'/'01'");
   Check_Value (T, Get_Text (Catalog, "f1", "02", base),
                "This fac1 message 0002",
                "Expected 'f1'/'02'");
   Check_Value (T, Get_Text (Catalog, "f1", "01", fr),
                "This fac1 message 0001 (fr)",
                "Expected 'f1'/'01'/fr");
   Check_Value (T, Get_Text (Catalog, "f1", "02", fr),
                "This fac1 message 0002 (fr)",
                "Expected 'f1'/'02'/fr");
   Check_Value (T, Get_Text (Catalog, "f1", "01", he),
                "This fac1 message 0001",
                "Expected 'f1'/'01'/he");
   Check_Value (T, Get_Text (Catalog, "f1", "02", he),
                "This fac1 message 0002",
                "Expected 'f1'/'02'/he");

   Check_Value (T, Get_Text (Catalog, "f2", "01", base),
                "This fac2 message 0001",
                "Expected 'f2'/'01'");
   Check_Value (T, Get_Text (Catalog, "f2", "02", base),
                "This fac2 message 0002",
                "Expected 'f2'/'02'");
   Check_Value (T, Get_Text (Catalog, "f2", "01", fr),
                "This fac2 message 0001",
                "Expected 'f2'/'01'/fr");
   Check_Value (T, Get_Text (Catalog, "f2", "02", fr),
                "This fac2 message 0002",
                "Expected 'f2'/'02'/fr");
   Check_Value (T, Get_Text (Catalog, "f2", "01", he),
                "This fac2 message 0001 (he)",
                "Expected 'f2'/'01'/he");
   Check_Value (T, Get_Text (Catalog, "f2", "02", he),
                "This fac2 message 0002 (he)",
                "Expected 'f2'/'02'/he");

end T_0040;
