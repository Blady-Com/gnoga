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

with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Maps.Wide_Constants;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Formatting;

package body ZBMCompile.Checks is

   use ZanyBlue;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Formatting;

   function Is_Ada_Identifier_OK (Name : in Wide_String) return Boolean;
   --  Is the given name a valid Ada identifier name?  Leading digits are
   --  OK as the name will be prefixed with standard prefixes later.

   ---------------------
   -- Accessors_Check --
   ---------------------

   procedure Accessors_Check (Handler  : in out ZBMC_Handler_Type) is
      Catalog : constant Catalog_Type := Get_Catalog (Handler);
      Facility_Index : Facility_Index_Type;
      Key_Index : Key_Index_Type;
   begin
      Print_Line (ZBMCompile_Facility, "V00004");
      for I in 1 .. Number_Of_Facilities (Catalog) loop
         Facility_Index := I;
         if not Is_Ada_Identifier_OK (Get_Facility (Catalog,
                                                    Facility_Index))
         then
            Print_Line (ZBMCompile_Facility, "E00017",
                        Argument0 => +Get_Facility (Catalog, Facility_Index));
            Handler.Increment_Errors;
         end if;
      end loop;
      for I in 1 .. Number_Of_Keys (Catalog) loop
         Key_Index := I;
         if not Is_Ada_Identifier_OK (Get_Key (Catalog, Key_Index)) then
            Print_Line (ZBMCompile_Facility, "E00018",
                        Argument0 => +Get_Key (Catalog, Key_Index));
            Handler.Increment_Errors;
         end if;
      end loop;
   end Accessors_Check;

   -----------------------
   -- Consistency_Check --
   -----------------------

   procedure Consistency_Check (Handler     : in out ZBMC_Handler_Type;
                                Facility    : in Wide_String;
                                Base_Locale : in Wide_String) is

      procedure Callback (Catalog  : in Catalog_Type;
                          Facility : in Wide_String;
                          Key_Name : in Wide_String;
                          Locales  : in Locale_Definitions_Map);

      --------------
      -- Callback --
      --------------

      procedure Callback (Catalog  : in Catalog_Type;
                          Facility : in Wide_String;
                          Key_Name : in Wide_String;
                          Locales  : in Locale_Definitions_Map) is

         pragma Unreferenced (Catalog);

         use Locale_Definitions_Package;

         Ref_Arg_Count : Natural;

         procedure Check_Other_Locales (Position : in Cursor);

         procedure Report_Extra_Locales (Position : in Cursor);

         procedure Check_Other_Locales (Position : in Cursor) is
            L : constant Natural :=
                Natural (String_Vectors.Length (Element (Position).Arg_Types));
         begin
            if L > Ref_Arg_Count then
               Print_Line (ZBMCompile_Facility, "E00015",
                           Argument0 => +Key_Name,
                           Argument1 => +Facility,
                           Argument2 => +Key (Position));
               Handler.Increment_Errors;
            end if;
         end Check_Other_Locales;

         procedure Report_Extra_Locales (Position : in Cursor) is
         begin
            Print_Line (ZBMCompile_Facility, "E00014",
                        Argument0 => +Key_Name,
                        Argument1 => +Facility,
                        Argument2 => +Key (Position));
         end Report_Extra_Locales;

      begin
         if not Has_Element (Locales.Find (Base_Locale)) then
            Print_Line (ZBMCompile_Facility, "E00013",
                        Argument0 => +Key_Name,
                        Argument1 => +Facility);
            Locales.Iterate (Report_Extra_Locales'Access);
            Handler.Increment_Errors;
         else
            Ref_Arg_Count := Natural (String_Vectors.Length (Element (
                               Locales.Find (Base_Locale)).Arg_Types));
            Locales.Iterate (Check_Other_Locales'Access);
         end if;
      end Callback;

   begin
      if Facility_Defines_Locale (Handler, Facility, Base_Locale) then
         Print_Line (ZBMCompile_Facility, "V00005",
                     Argument0 => +Facility);
         Message_Iterate (Handler, Facility, Callback'Access);
      else
         Print_Line (ZBMCompile_Facility, "V00006",
                     Argument0 => +Facility,
                     Argument1 => +Base_Locale);
      end if;
   end Consistency_Check;

   --------------------------
   -- Is_Ada_Identifier_OK --
   --------------------------

   function Is_Ada_Identifier_OK (Name : in Wide_String) return Boolean is
      use Ada.Strings.Wide_Fixed;
      use Ada.Strings.Wide_Maps;
      use Ada.Strings.Wide_Maps.Wide_Constants;
   begin
      return Index (Name, not (Alphanumeric_Set or To_Set ("_"))) = 0;
   end Is_Ada_Identifier_OK;

end ZBMCompile.Checks;