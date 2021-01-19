--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2018, Michael Rohan <mrohan@zanyblue.com>
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
procedure T_0003 (T : in out Test_Case'Class) is

   F          : constant String := "f";
   K          : constant String := "k";
   b          : constant Locale_Type := Make_Locale ("");
   en         : constant Locale_Type := Make_Locale ("en");
   en_US      : constant Locale_Type := Make_Locale ("en_US");
   en_Latn    : constant Locale_Type := Make_Locale ("en_Latn");
   en_Latn_US : constant Locale_Type := Make_Locale ("en_Latn_US");
   Catalog    : constant Catalog_Type := Create;

   procedure Check (L            : Locale_Type;
                    V            : String;
                    b_M          : String;
                    en_M         : String;
                    en_US_M      : String;
                    en_Latn_M    : String;
                    en_Latn_US_M : String);
   procedure Check (V : String;
                    L : Locale_Type);

   procedure Check (V : String;
                    L : Locale_Type) is
   begin
      Check_Value (T, Get_Text (Catalog, F, K, L), V,
                   "Locale """ & Locale_Name (L) & """");
   end Check;

   procedure Check (L            : Locale_Type;
                    V            : String;
                    b_M          : String;
                    en_M         : String;
                    en_US_M      : String;
                    en_Latn_M    : String;
                    en_Latn_US_M : String) is
   begin
      Add (Catalog, F, K, V, L);
      Check (en_Latn_US_M, en_Latn_US);
      Check (en_Latn_M, en_Latn);
      Check (en_US_M, en_US);
      Check (en_M, en);
      Check (b_M, b);
   end Check;

begin
   Check (b, "B",            "B", "B", "B",  "B",  "B");
   Check (en, "E",           "B", "E", "E",  "E",  "E");
   Check (en_US, "EU",       "B", "E", "EU", "E",  "EU");
   Check (en_Latn, "EL",     "B", "E", "EU", "EL", "EL");
   Check (en_Latn_US, "ELU", "B", "E", "EU", "EL", "ELU");
end T_0003;
