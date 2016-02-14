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

with Ada.Directories;
with Ada.Wide_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Fixed;

-------------------
-- Load_Facility --
-------------------

separate (ZanyBlue.Text.Catalogs)
procedure Load_Facility (
   Facility           : Wide_String;
   Source_Name        : Wide_String;
   N_Locales          : out Natural;
   N_Messages         : out Natural;
   Handler            : in out Catalog_Handler_Type'Class;
   Directory          : Wide_String := ".";
   Extension          : Wide_String := Default_Extension;
   Base_Locale_Only   : Boolean := False;
   Locale_Prefix      : Wide_String := "";
   Source_Root_Locale : Locale_Type := Root_Locale)
is

   use Ada.Directories;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Wide_Fixed;

   type Property_File is
      record
         File_Name : Unbounded_String;
         Locale    : Locale_Type;
      end record;
   --  The loading of a facility requires scanning directory for propreties
   --  files associated with the facility.  The file paths and associated
   --  locales are stored in a list by the scanning routine and later loaded
   --  by the loading routine.  The Property_File stores an instance of
   --  such a file.

   function "<" (Left, Right : Property_File) return Boolean;
   --  Need an ordering function to support sorting.  The list of properties
   --  files is sorted to ensure a given set of files is always processed in
   --  the same order on different platforms (the order of files generated by
   --  directory scanning is not defined.  The file are ordered by locale.

   package Property_File_Vectors is
      new Indefinite_Vectors (Index_Type      => Positive,
                              Element_Type    => Property_File);
   subtype Property_File_List is Property_File_Vectors.Vector;
   --  The package implementing the list of properties files.

   package Sorter is new Property_File_Vectors.Generic_Sorting;
   --  The package implementing the sorting routine for list of files.

   procedure Scan_Properties (Result : in out Property_File_List);
   --  Scan the source directory for properties files matching the facility
   --  name.  The file found are added to the Result list.

   function Load_Properties (List : Property_File_List) return Natural;
   --  Once the list of files has been generated, the properties are loaded
   --  using the the Load_Properties routine.

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Property_File) return Boolean is
   begin
      if Left.Locale = Right.Locale then
         return Left.File_Name < Right.File_Name;
      else
         return Locale_Name (Left.Locale) < Locale_Name (Right.Locale);
      end if;
   end "<";

   ---------------------
   -- Load_Properties --
   ---------------------

   function Load_Properties (List : Property_File_List) return Natural is

      procedure Load_Source (E : Property_File);
      --  Load an individual properties file.

      -----------------
      -- Load_Source --
      -----------------

      procedure Load_Source (E : Property_File) is
         L_Name : constant Wide_String := E.Locale.Locale_Name;
         Do_Load : Boolean;
      begin
         if Base_Locale_Only then
            Do_Load := L_Name = "";
         else
            Do_Load := Head (L_Name, Locale_Prefix'Length) = Locale_Prefix;
         end if;
         if Do_Load then
            if E.Locale = Root_Locale then
               Load_File (From_UTF8 (To_String (E.File_Name)), Facility,
                          E.Locale, Handler, Source_Root_Locale);
            else
               Load_File (From_UTF8 (To_String (E.File_Name)), Facility,
                          E.Locale, Handler, E.Locale);
            end if;
            N_Locales := N_Locales + 1;
         end if;
      exception
      when Name_Error =>
         null;
      end Load_Source;

   begin
      Handler.Reset_N_Messages;
      for I in 1 .. Integer (List.Length) loop
         List.Query_Element (I, Load_Source'Access);
      end loop;
      return Handler.Get_N_Messages;
   end Load_Properties;

   ---------------------
   -- Scan_Properties --
   ---------------------

   procedure Scan_Properties (Result : in out Property_File_List) is

      function File_Locale (File_Name      : String;
                            Simple_Name    : String) return Locale_Type;
      --  Determine the locale defined by the file name, e.g.,
      --  "xmpl_fr.properties" => "fr"

      -----------------
      -- File_Locale --
      -----------------

      function File_Locale (File_Name      : String;
                            Simple_Name    : String) return Locale_Type is

         B_Name : constant String := Base_Name (File_Name);
         A      : constant Positive := B_Name'First + Simple_Name'Length + 1;
         B      : constant Positive := B_Name'Last;
         L_Name : constant Wide_String := To_Wide_String (B_Name (A .. B));
         Locale : constant Locale_Type := Make_Locale (L_Name);

      begin
         return Locale;
      end File_Locale;

      Base_File_Name   : constant String :=
         Compose (Containing_Directory => To_UTF8 (Directory),
                  Name                 => To_UTF8 (Source_Name),
                  Extension            => To_UTF8 (Extension));
      Base_Full_Name   : constant String := Full_Name (Base_File_Name);
      Base_Directory   : constant String
                               := Containing_Directory (Base_Full_Name);
      Simple_File_Name : constant String
                               := Base_Name (Simple_Name (Base_Full_Name));
      Search_Pattern   : constant String := To_UTF8 (Source_Name)
                                          & "_*."
                                          & To_UTF8 (Extension);
      File_Search : Search_Type;
      Item : Directory_Entry_Type;
      Filter : constant Filter_Type := (Ordinary_File => True,
                                        others => False);
   begin
      Result.Append (Property_File'(
                        File_Name => To_Unbounded_String (Base_File_Name),
                        Locale    => <>));
      Start_Search (File_Search, Base_Directory, Search_Pattern,
                    Filter => Filter);
      while More_Entries (File_Search) loop
         Get_Next_Entry (File_Search, Item);
         Result.Append (Property_File'(
                           File_Name => To_Unbounded_String (Full_Name (Item)),
                           Locale    => File_Locale (Full_Name (Item),
                                                     Simple_File_Name)));
      end loop;
      End_Search (File_Search);
   end Scan_Properties;

   Property_Files : Property_File_List;

begin
   Add_Locale (Get_Catalog (Handler), Source_Root_Locale);
   N_Locales := 0;
   Scan_Properties (Property_Files);
   Sorter.Sort (Property_Files);
   N_Messages := Load_Properties (Property_Files);
end Load_Facility;
