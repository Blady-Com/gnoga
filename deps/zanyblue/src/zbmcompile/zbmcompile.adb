--  -*- coding: utf-8 -*-
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

with Ada.Calendar;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.Wide_Wide_Maps.Wide_Constants;
with ZanyBlue.OS;
with ZanyBlue.Parameters;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Formatting;
with ZBMCompile.Checks;
with ZBMCompile.Parser_Handler;
with ZBMCompile.Codegen;
with ZBMCompile.Codegen.Base;
with ZBMCompile.Codegen.Accessors;

package body ZBMCompile is

   use ZanyBlue.OS;
   use ZanyBlue.Parameters;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Formatting;
   use ZBMCompile.Checks;
   use ZBMCompile.Parser_Handler;
   use ZBMCompile.Codegen;
   use ZBMCompile.Codegen.Base;
   use ZBMCompile.Codegen.Accessors;

   procedure Check_Loaded (Handler : in out ZBMC_Handler_Type;
                           Options : Parameter_Set_Type);
   --  Perform consistency checkes on the messages loaded.

   procedure Load_Files (Handler      : in out ZBMC_Handler_Type;
                         Files_Loaded : out Boolean;
                         Options      : Parameter_Set_Type);
   --  Load the files defined by the command line arguments.  The File_Loaded
   --  argument is set to True if any files were loaded, i.e., something was
   --  processed.

   function Savings (Total  : Natural;
                     Stored : Natural) return Natural;
   --  Return the percentage savings for stored characters in a catalog pool.

   procedure Summarize (Catalog : Catalog_Type);
   --  Print a summary of the facilities and messages loaded.

   function Update_Stamp_File (File_Name : String) return Boolean;
   --  Update the contents of the "touch" file.  The file timestamp can
   --  be used to trigger dependency rules via make.  Return True if the
   --  file was successfully created, false on error.

   ------------------
   -- Check_Loaded --
   ------------------

   procedure Check_Loaded (Handler : in out ZBMC_Handler_Type;
                           Options : Parameter_Set_Type) is
      Facilities  : constant List_Type := Options.Get_List ("facilities");
   begin
      if Options.Get_Boolean ("disable_checks") then
         return;
      end if;
      for I in 1 ..  Length (Facilities) loop
         Consistency_Check (Handler, Value (Facilities, I),
                            Options.Get_String ("reference_locale"));
      end loop;
      if Options.Get_Boolean ("generate_accessors") then
         Accessors_Check (
            Handler,
            Options.Get_String ("invalid_ada_key_handler") = "ignore");
      end if;
   end Check_Loaded;

   --------------------------
   -- Is_Ada_Identifier_OK --
   --------------------------

   function Is_Ada_Identifier_OK (Name : String) return Boolean is
      use Ada.Strings.Wide_Fixed;
      use Ada.Strings.Wide_Wide_Maps;
      use Ada.Strings.Wide_Wide_Maps.Wide_Constants;
   begin
      return Index (Name, not (Alphanumeric_Set or To_Set ("_"))) = 0;
   end Is_Ada_Identifier_OK;

   ----------------
   -- Load_Files --
   ----------------

   procedure Load_Files (Handler      : in out ZBMC_Handler_Type;
                         Files_Loaded : out Boolean;
                         Options      : Parameter_Set_Type) is

      Directories : constant List_Type := Options.Get_List ("mesg_dirs");
      Facilities  : constant List_Type := Options.Get_List ("facilities");
      Extension   : constant String := Options.Get_String ("extension");
      Source_Root_Locale : constant Locale_Type :=
                Make_Locale (Options.Get_String ("source_root_locale"));
      N_Locales    : Natural := 0;
      N_Messages   : Natural := 0;

   begin
      Files_Loaded := False;
      for I in 1 ..  Length (Facilities) loop
         Add_Facility (Get_Catalog (Handler), Value (Facilities, I));
         if Options.Get_Boolean ("base_locale") then
            Load_Facility (Facility            => Value (Facilities, I),
                           Source_Name         => Value (Facilities, I),
                           N_Locales           => N_Locales,
                           N_Messages          => N_Messages,
                           Handler             => Handler,
                           Directory           => Value (Directories, I),
                           Extension           => Extension,
                           Base_Locale_Only    => True,
                           Source_Root_Locale  => Source_Root_Locale);
            Files_Loaded := Files_Loaded
                               or else N_Locales > 0
                               or else N_Messages > 0;
         end if;
         if Options.Is_Defined ("locales") then
            declare
               Locales : constant List_Type := Options.Get_List ("locales");
            begin
               for L in 1 .. Length (Locales) loop
                  Load_Facility (
                     Facility            => Value (Facilities, I),
                     Source_Name         => Value (Facilities, I),
                     N_Locales           => N_Locales,
                     N_Messages          => N_Messages,
                     Handler             => Handler,
                     Directory           => Value (Directories, I),
                     Extension           => Extension,
                     Locale_Prefix       => Value (Locales, L),
                     Source_Root_Locale  => Source_Root_Locale);
                  Files_Loaded := Files_Loaded
                                     or else N_Locales > 0
                                     or else N_Messages > 0;
               end loop;
            end;
         end if;
         Print_Line (ZBMCompile_Facility, "V00001",
                     Argument0 => +N_Messages,
                     Argument1 => +Value (Facilities, I),
                     Argument2 => +N_Locales);
      end loop;
   end Load_Files;

   --------------
   -- Print_If --
   --------------

   procedure Print_If (Condition  : Boolean;
                       File       : File_Type;
                       Facility   : String;
                       Key        : String;
                       Argument0  : Argument_Type'Class := Null_Argument;
                       Argument1  : Argument_Type'Class := Null_Argument;
                       Argument2  : Argument_Type'Class := Null_Argument;
                       Argument3  : Argument_Type'Class := Null_Argument;
                       Argument4  : Argument_Type'Class := Null_Argument) is
   begin
      if Condition then
         Print_Line (File, Facility, Key,
                     Argument0, Argument1, Argument2, Argument3, Argument4);
      end if;
   end Print_If;

   -------------
   -- Process --
   -------------

   function Process (Options : Parameter_Set_Type) return Boolean is

      Disable_Checks : constant Boolean :=
                          Options.Get_Boolean ("disable_checks");
      Catalog      : Catalog_Type := Create;
      Handler      : ZBMC_Handler_Type (Disable_Checks);
      Files_Loaded : Boolean;

   begin
      Use_Single_Pool (Catalog);
      Handler.Set_Catalog (Catalog);
      Load_Files (Handler, Files_Loaded, Options);
      Check_Loaded (Handler, Options);
      if not Files_Loaded then
         Print_Line (ZBMCompile_Facility, "E00005");
         return False;
      elsif Handler.Get_N_Errors > 0 then
         Print_Line (ZBMCompile_Facility,
                     Select_Message (Options.Get_Boolean ("force"),
                                     "E00010", "E00009"),
                     Argument0 => +Handler.Get_N_Errors);
         if not Options.Get_Boolean ("force") then
            return False;
         end if;
      end if;
      if Options.Get_Boolean ("optimize") then
         Catalog := Optimize (Catalog, Options);
         Handler.Set_Catalog (Catalog);
      end if;
      Summarize (Catalog);
      Create_Root_Spec (Catalog, Options);
      Create_Root_Body (Catalog, Options);
      if Options.Get_Boolean ("generate_accessors") then
         Create_Accessor_Packages (Handler, Options);
      end if;
      if Options.Is_Defined ("stamp_file") then
         return Update_Stamp_File (Options.Get_String ("stamp_file"));
      else
         return True;
      end if;
   end Process;

   -------------
   -- Savings --
   -------------

   function Savings (Total  : Natural;
                     Stored : Natural) return Natural is
      Difference : Float;
   begin
      if Total = 0 then
         return 0;
      end if;
      Difference := Float (Total - Stored);
      return Natural (100.0 * Difference / Float (Total));
   end Savings;

   --------------------
   -- Select_Message --
   --------------------

   function Select_Message (Condition  : Boolean;
                            True_Id    : String;
                            False_Id   : String) return String is
   begin
      if Condition then
         return True_Id;
      else
         return False_Id;
      end if;
   end Select_Message;

   ---------------
   -- Summarize --
   ---------------

   procedure Summarize (Catalog : Catalog_Type) is
   begin
      Print_Line (ZBMCompile_Facility, "V00002",
                  Argument0 => +Number_Of_Facilities (Catalog),
                  Argument1 => +Number_Of_Keys (Catalog),
                  Argument2 => +Number_Of_Locales (Catalog),
                  Argument3 => +Number_Of_Messages (Catalog));
      Print_Line (ZBMCompile_Facility, "V00003",
                  Argument0 => +Logical_Pool_Size (Catalog),
                  Argument1 => +Pool_Size (Catalog),
                  Argument2 => +Savings (Logical_Pool_Size (Catalog),
                                       Pool_Size (Catalog)));
   end Summarize;

   -----------------------
   -- Update_Stamp_File --
   -----------------------

   function Update_Stamp_File (File_Name : String) return Boolean is
      use Ada.Calendar;
      Now  : constant Time := Clock;
      File : File_Type;
   begin
      Wide_Create (File, File_Name);
      Print_Line (File, ZBMCompile_Facility, "I00006",
                  Argument0 => +Clock);
      Close (File);
      Print_Line (ZBMCompile_Facility, "I00007",
                  Argument0 => +File_Name,
                  Argument1 => +Now);
      return True;
   exception
   when E : Name_Error =>
      Print_Line (ZBMCompile_Facility, "E00019",
                  Argument0 => +File_Name,
                  Argument1 => +E);
      return False;
   end Update_Stamp_File;

end ZBMCompile;
