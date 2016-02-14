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

with Ada.Wide_Text_IO;

----------
-- Dump --
----------
separate (ZanyBlue.Text.Catalogs)
procedure Dump (Catalog   : Catalog_Type;
                File_Name : Wide_String := "") is

   use Ada.Wide_Text_IO;

   procedure Dump_Pool (File    : File_Type;
                        Catalog : Catalog_Type);
   --  Dump the contents of the dynamic pool.
   procedure Dump_To_File (Catalog : Catalog_Type;
                           File    : File_Type);
   --  Dump the catalog to an opened file handle.

   procedure Hdr (File  : File_Type;
                  Title : Wide_String;
                  N     : Natural);
   procedure Hdr (File  : File_Type;
                  Title : Wide_String;
                  N     : Natural;
                  M     : Natural);
   --  Write the header data about the catalog.

   procedure Dump_Message (File : File_Type;
                           N    : in out Positive;
                           F    : Facility_Index_Type;
                           K    : Key_Index_Type;
                           L    : Locale_Index_Type);
   --  Dump a message given the message indexes.

   procedure Dump_Message (File     : File_Type;
                           N        : in out Positive;
                           Facility : Wide_String;
                           Key      : Wide_String;
                           Locale   : Locale_Type;
                           Message  : Wide_String);
   --  Dump a message given the text.

   ------------------
   -- Dump_Message --
   ------------------

   procedure Dump_Message (File     : File_Type;
                           N        : in out Positive;
                           Facility : Wide_String;
                           Key      : Wide_String;
                           Locale   : Locale_Type;
                           Message  : Wide_String) is
   begin
      Put (File, Positive'Wide_Image (N));
      Put (File, ": """);
      Put (File, Facility);
      Put (File, """, """);
      Put (File, Key);
      Put (File, """, """);
      Put (File, Locale_Name (Locale));
      Put (File, """, """);
      Put (File, Message);
      Put (File, """");
      New_Line (File);
      N := N + 1;
   end Dump_Message;

   ------------------
   -- Dump_Message --
   ------------------

   procedure Dump_Message (File : File_Type;
                           N    : in out Positive;
                           F    : Facility_Index_Type;
                           K    : Key_Index_Type;
                           L    : Locale_Index_Type) is
   begin
      Dump_Message (File, N,
                    Get_Facility (Catalog, F),
                    Get_Key (Catalog, K),
                    Get_Locale (Catalog, L),
                    Get_Text (Catalog, F, K, L));
   exception
   when No_Such_Facility_Error |
        No_Such_Key_Error |
        No_Such_Locale_Error |
        No_Such_Message_Error =>
      null;
   end Dump_Message;

   procedure Dump_Pool (File    : File_Type;
                        Catalog : Catalog_Type) is
      Pool  : constant Wide_String := Catalog.C.Messages.Get_Pool;
      Limit : constant Natural := Pool_Size (Catalog);
      Span  : constant Positive := 75;
      Start : Positive := 1;
   begin
      Hdr (File, "Dynmaic Pool",
           Limit, Logical_Pool_Size (Catalog));
      Put_Line (File, "---START OF STRING POOL---");
      while Start < Pool'Length loop
         Put_Line (File, Pool (Start .. Natural'Min (Start + Span, Limit)));
         Start := Start + Span + 1;
      end loop;
      Put_Line (File, "---END OF STRING POOL---");
   end Dump_Pool;

   ------------------
   -- Dump_To_File --
   ------------------

   procedure Dump_To_File (Catalog : Catalog_Type;
                           File    : File_Type) is
      N : Positive := 1;

   begin
      Put_Line (File, "Catalog dump");
      Put_Line (File, "------------");
      Hdr (File, "Number_Of_Facilities", Number_Of_Facilities (Catalog));
      for F in 1 .. Number_Of_Facilities (Catalog) loop
         Put_Line (File, "    """ & Get_Facility (Catalog, F) & """");
      end loop;
      Hdr (File, "Number_Of_Keys", Number_Of_Keys (Catalog));
      for K in 1 .. Number_Of_Keys (Catalog) loop
         Put_Line (File, "    """ & Get_Key (Catalog, K) & """");
      end loop;
      Hdr (File, "Number_Of_Locales", Number_Of_Locales (Catalog));
      for L in 1 .. Number_Of_Locales (Catalog) loop
         Put_Line (File, "    """
                & Locale_Name (Get_Locale (Catalog, L))
                & """");
      end loop;
      Hdr (File, "Number_Of_Messages", Number_Of_Messages (Catalog));
      --  Note: This needs to be reimplemented in terms of "Iterate".
      Locales : for L in 1 .. Number_Of_Locales (Catalog) loop
         Facilities : for F in 1 .. Number_Of_Facilities (Catalog) loop
            Keys : for K in 1 .. Number_Of_Keys (Catalog) loop
               Dump_Message (File, N, F, K, L);
            end loop Keys;
         end loop Facilities;
      end loop Locales;
      Dump_Pool (File, Catalog);
   end Dump_To_File;

   ---------
   -- Hdr --
   ---------

   procedure Hdr (File  : File_Type;
                  Title : Wide_String;
                  N     : Natural) is
   begin
      Put (File, Title);
      Set_Col (File, 25);
      Put (File, ": ");
      Put (File, Natural'Wide_Image (N));
      New_Line (File);
   end Hdr;

   ---------
   -- Hdr --
   ---------

   procedure Hdr (File  : File_Type;
                  Title : Wide_String;
                  N     : Natural;
                  M     : Natural) is
   begin
      Put (File, Title);
      Set_Col (File, 25);
      Put (File, ": ");
      Put (File, Natural'Wide_Image (N));
      Set_Col (File, 40);
      Put (File, Natural'Wide_Image (M));
      New_Line (File);
   end Hdr;

   File : File_Type;

begin
   if File_Name'Length /= 0 then
      Wide_Create (File, File_Name);
      Dump_To_File (Catalog, File);
      Close (File);
   else
      Dump_To_File (Catalog, Current_Output);
   end if;
end Dump;
