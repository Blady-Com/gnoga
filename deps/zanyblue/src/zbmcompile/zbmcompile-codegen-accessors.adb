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

with Ada.Wide_Characters.Unicode;
with Ada.Containers.Vectors;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Version_Status_Arguments;
with ZanyBlue.Utils;
with ZanyBlue.Wide_Directories;

package body ZBMCompile.Codegen.Accessors is

   use ZanyBlue;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Formatting;
   use ZanyBlue.Text.Version_Status_Arguments;
   use ZanyBlue.Utils;
   use ZanyBlue.Wide_Directories;

   type Key_Descriptor_Type is
      record
         Index     : Key_Index_Type;
         N_Args    : Natural;
         Arg_Types : String_Vectors.Vector;
      end record;

   package Key_Descriptor_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Key_Descriptor_Type);
   subtype Key_Descriptor_Vector is Key_Descriptor_Vectors.Vector;

   function Argument_Class (Index     : in Natural;
                            Arg_Types : in String_Vectors.Vector)
      return Wide_String;
   --  Return the argument class string associated with a argument index.

   procedure Create_Key_Descriptors
     (Handler             : in ZBMC_Handler_Type;
      Facility            : in Wide_String;
      Base_Locale         : in Wide_String;
      Max_Args            : out Natural;
      Key_Descriptors     : in out Key_Descriptor_Vector);
   --  Create the sorted list of descriptors for the keys used in a facility.
   --  The descriptors give the number of arguments expected for each message
   --  in a facility and give the accessor argument signature.

   procedure Create_Facility_Accessors
     (Catalog             : in Catalog_Type;
      Facility            : in Wide_String;
      Options             : in Parameter_Set_Type;
      Modes               : in Wide_String;
      Max_Args            : in Natural;
      Key_Descriptors     : in Key_Descriptor_Vector;
      Base_Locale         : in Wide_String;
      Accessor_Facility   : in Wide_String);
   --  Create the spec and body for the facility accessors based on the
   --  accessor facility.

   procedure Create_Facility_Packages
     (Handler             : in ZBMC_Handler_Type;
      Facility            : in Wide_String;
      Options             : in Parameter_Set_Type);
   --  Create the accessor packages for an individual facility.

   procedure Copy_Argument_Types (Base_Locale : in Wide_String;
                                  Locales     : in Locale_Definitions_Map;
                                  N_Args      : in Natural;
                                  Arg_Types   : in out String_Vectors.Vector);

   function Message_Arg_Count (Base_Locale : in Wide_String;
                               Locales     : in Locale_Definitions_Map)
      return Natural;
   --  Return the expected number of arguments for a message.  This is the
   --  the number of argument of the base locale or, if the message is not
   --  defined for the base locale, the max number of argument for any locale.

   procedure Write_Base_Message_Text (File        : in File_Type;
                                      Catalog     : in Catalog_Type;
                                      Facility    : in Wide_String;
                                      Key         : in Wide_String;
                                      Base_Locale : in Wide_String;
                                      Block_Size  : in Positive);
   --  Write the value associated with the key in the base locale as a
   --  comment after the accessor function/procedure in the spec.  This
   --  allows GPS to display the text associated with the message.

   --------------------
   -- Argument_Class --
   --------------------

   function Argument_Class (Index     : in Natural;
                            Arg_Types : in String_Vectors.Vector)
      return Wide_String
   is
      use String_Vectors;
      Result : constant Wide_String := Element (Arg_Types, Index);
   begin
      if Result = "" then
         return "Any_Category_Type";
      else
         return Result;
      end if;
   end Argument_Class;

   -------------------------
   -- Copy_Argument_Types --
   -------------------------

   procedure Copy_Argument_Types (Base_Locale : in Wide_String;
                                  Locales     : in Locale_Definitions_Map;
                                  N_Args      : in Natural;
                                  Arg_Types   : in out String_Vectors.Vector)
   is
      use Locale_Definitions_Package;
   begin
      if Has_Element (Locales.Find (Base_Locale)) then
         Arg_Types := Element (Locales.Find (Base_Locale)).Arg_Types;
      else
         --  No base message, simply append empty strings (any argument type)
         --  to the type list.
         for I in 1 .. N_Args loop
            String_Vectors.Append (Arg_Types, "");
         end loop;
      end if;
   end Copy_Argument_Types;

   ------------------------------
   -- Create_Accessor_Packages --
   ------------------------------

   procedure Create_Accessor_Packages
      (Handler          : in ZBMC_Handler_Type;
       Options          : in Parameter_Set_Type)
   is

      Catalog    : constant Catalog_Type := Get_Catalog (Handler);

   begin
      for I in 1 .. Number_Of_Facilities (Catalog) loop
         Create_Facility_Packages (
            Handler,
            Get_Facility (Catalog, I),
            Options);
      end loop;
   end Create_Accessor_Packages;

   -------------------------------
   -- Create_Facility_Accessors --
   -------------------------------

   procedure Create_Facility_Accessors (
      Catalog             : in Catalog_Type;
      Facility            : in Wide_String;
      Options             : in Parameter_Set_Type;
      Modes               : in Wide_String;
      Max_Args            : in Natural;
      Key_Descriptors     : in Key_Descriptor_Vector;
      Base_Locale         : in Wide_String;
      Accessor_Facility   : in Wide_String)
   is

      use Key_Descriptor_Vectors;

      procedure Close_And_Report (File             : in out File_Type;
                                  File_Name        : in Wide_String;
                                  Facility_Package : in Wide_String;
                                  Updated_Id       : in Wide_String;
                                  Retained_Id      : in Wide_String);

      procedure Write_Accessor (Spec_File       : in File_Type;
                                Body_File       : in File_Type;
                                Key_Descriptor  : in Key_Descriptor_Type);

      procedure Close_And_Report (File             : in out File_Type;
                                  File_Name        : in Wide_String;
                                  Facility_Package : in Wide_String;
                                  Updated_Id       : in Wide_String;
                                  Retained_Id      : in Wide_String) is
         Updated : Boolean;
      begin
         Close_And_Update (File, Updated);
         Print_Line (ZBMCompile_Facility,
                     Select_Message (Updated, Updated_Id, Retained_Id),
                     +Facility_Package, +File_Name);
      end Close_And_Report;

      procedure Write_Accessor (Spec_File       : in File_Type;
                                Body_File       : in File_Type;
                                Key_Descriptor  : in Key_Descriptor_Type) is
         Local_Arguments : constant Wide_String := "Arguments";
         Empty_Arguments : constant Wide_String := "Empty_Argument_List";
         Key  : constant Wide_String := Get_Key (Catalog,
                                                 Key_Descriptor.Index);
         Dash : constant Wide_Character := '-';
      begin
         Print_Line (Spec_File, Accessor_Facility, "10007",
                     +Key, +Modes);
         Print_Line (Body_File, Accessor_Facility, "20007",
                     +Key, +Dash, +Key'Length, +Modes);
         --  Print the argument list for the routine signature
         for I in 1 .. Key_Descriptor.N_Args loop
            Print_Line (Spec_File, Accessor_Facility, "10008",
                        +(I - 1), +Modes,
                        +Argument_Class (I - 1, Key_Descriptor.Arg_Types));
            Print_Line (Body_File, Accessor_Facility, "10008",
                        +(I - 1), +Modes,
                        +Argument_Class (I - 1, Key_Descriptor.Arg_Types));
         end loop;
         Print_Line (Spec_File, Accessor_Facility, "10009", +Key, +Modes);
         if Options.Get_Boolean ("accessor_comments") then
            Write_Base_Message_Text (Spec_File, Catalog, Facility,
                                      Key, Base_Locale,
                                      Options.Get_Integer ("comment_size"));
         end if;
         New_Line (Spec_File);
         Print_Line (Body_File, Accessor_Facility, "20008", +Modes);
         if Key_Descriptor.N_Args > 0 then
            --  Arguments, include the local arguments list
            Print_Line (Body_File, Accessor_Facility, "20009", +Modes);
         end if;
         Print_Line (Body_File, Accessor_Facility, "20010", +Modes);
         --  Append the arguments to the local Argument_List
         for I in 1 .. Key_Descriptor.N_Args loop
            Print_Line (Body_File, Accessor_Facility, "20011", +(I - 1));
         end loop;
         --  Write the call to the underlying ZB routine.
         if Key_Descriptor.N_Args > 0 then
            Print_Line (Body_File, Accessor_Facility, "20012",
                        +Get_Facility_Index (Catalog, Facility),
                        +Get_Key_Index (Catalog, Key),
                        +Key,
                        +Local_Arguments);
         else
            Print_Line (Body_File, Accessor_Facility, "20012",
                        +Get_Facility_Index (Catalog, Facility),
                        +Get_Key_Index (Catalog, Key),
                        +Key,
                        +Empty_Arguments);
         end if;
      end Write_Accessor;

      Output_Directory : constant Wide_String :=
                             Options.Get_String ("output_directory");
      Package_Name : constant Wide_String := Options.Get_String ("package");
      Facility_Package : constant Wide_String :=
                           Format (Accessor_Facility, "00000",
                                   +Package_Name, +Facility);
      Spec_Name : constant Wide_String := Wide_Compose (
                             Output_Directory,
                             Spec_File_Name (Facility_Package,
                                             GNAT_Naming_Style));
      Body_Name : constant Wide_String := Wide_Compose (
                             Output_Directory,
                             Body_File_Name (Facility_Package,
                                             GNAT_Naming_Style));
      Spec_File : File_Type;
      Body_File : File_Type;

   begin
      Wide_Create_For_Update (Spec_File, Spec_Name);
      Wide_Create_For_Update (Body_File, Body_Name);
      --  Write the header comment ...
      Print_Line (Spec_File, Accessor_Facility, "10001",
                  Argument0 => +Version_Major,
                  Argument1 => +Version_Minor,
                  Argument2 => +Version_Patch,
                  Argument3 => +Version_Status);
      Print_Line (Body_File, Accessor_Facility, "20001",
                  Argument0 => +Version_Major,
                  Argument1 => +Version_Minor,
                  Argument2 => +Version_Patch,
                  Argument3 => +Version_Status);
      --  Write the with clauses.  If there are no message arguments then
      --  the Arguments package in with'ed in the body rather than the
      --  spec.
      if Max_Args > 0 then
         Print_Line (Spec_File, Accessor_Facility, "10002");
         Print_Line (Body_File, Accessor_Facility, "20002");
      else
         Print_Line (Spec_File, Accessor_Facility, "10003");
         Print_Line (Body_File, Accessor_Facility, "20003");
      end if;
      --  Write the package declaration/implementation
      Print_Line (Spec_File, Accessor_Facility, "10004", +Facility_Package);
      Print_Line (Body_File, Accessor_Facility, "20004", +Facility_Package);
      --  Write the use clauses.  Again, need to differentiate between spec
      --  and body use of the Argument package (different keys).
      if Max_Args > 0 then
         Print_Line (Spec_File, Accessor_Facility, "10005");
         Print_Line (Body_File, Accessor_Facility, "20005");
      else
         Print_Line (Spec_File, Accessor_Facility, "10006");
         Print_Line (Body_File, Accessor_Facility, "20006");
      end if;
      --  Write the declaration and implementation of the accessor routines
      --  for the keys in the facility.
      for I in 1 .. Natural (Length (Key_Descriptors)) loop
         Write_Accessor (Spec_File, Body_File, Element (Key_Descriptors, I));
      end loop;
      --  Write the end of package declaration/implementation
      Print_Line (Spec_File, Accessor_Facility, "10010", +Facility_Package);
      Print_Line (Body_File, Accessor_Facility, "20013", +Facility_Package);
      Close_And_Report (Spec_File, Spec_Name, Facility_Package,
                        "V00007", "V00008");
      Close_And_Report (Body_File, Body_Name, Facility_Package,
                        "V00009", "V00010");
   end Create_Facility_Accessors;

   ------------------------------
   -- Create_Facility_Packages --
   ------------------------------

   procedure Create_Facility_Packages
     (Handler             : in ZBMC_Handler_Type;
      Facility            : in Wide_String;
      Options             : in Parameter_Set_Type) is

      Catalog         : constant Catalog_Type := Get_Catalog (Handler);
      Base_Locale      : constant Wide_String
                              := Options.Get_String ("reference_locale");
      Modes           : constant Wide_String := Modes_String (Options);
      Key_Descriptors : Key_Descriptor_Vector;
      Max_Args        : Natural;

   begin
      if not Options.Get_Boolean ("accessor_comments") then
         Print_Line (ZBMCompile_Facility, "I00008");
      end if;
      Create_Key_Descriptors (Handler, Facility, Base_Locale,
                              Max_Args, Key_Descriptors);
      --  Create an instance for each selected accessor type.
      for I in Accessor_Types'Range loop
         if Options.Get_Boolean ("accessor:" & Accessor_Types (I).all) then
            Create_Facility_Accessors (Catalog, Facility, Options,
                                    Modes, Max_Args, Key_Descriptors,
                                    Base_Locale,
                                    "zbm" & Accessor_Types (I).all);
         end if;
      end loop;
   exception
   when Constraint_Error =>
      --  Facility name is defined but it contains no messages
      Print_Line (ZBMCompile_Facility, "E00021",
                  Argument0 => +Facility);
   end Create_Facility_Packages;

   ----------------------------
   -- Create_Key_Descriptors --
   ----------------------------

   procedure Create_Key_Descriptors
     (Handler             : in ZBMC_Handler_Type;
      Facility            : in Wide_String;
      Base_Locale         : in Wide_String;
      Max_Args            : out Natural;
      Key_Descriptors     : in out Key_Descriptor_Vector) is

      Catalog : constant Catalog_Type := Get_Catalog (Handler);

      function "<" (Left, Right : in Key_Descriptor_Type) return Boolean;
      --  Compare two key descriptors based on the key text.

      procedure Scan_Key (Catalog     : in Catalog_Type;
                          Facility    : in Wide_String;
                          Key         : in Wide_String;
                          Locales  : in Locale_Definitions_Map);
      --  Scan the messages generating a mapping from key to expected number
      --  of arguments to the base message.

      package Key_Sorting is new Key_Descriptor_Vectors.Generic_Sorting;
      --  Package to sort the key descriptor list.

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : in Key_Descriptor_Type) return Boolean is
         use Ada.Wide_Characters.Unicode;
         Left_Key : Wide_String := Get_Key (Catalog, Left.Index);
         Right_Key : Wide_String := Get_Key (Catalog, Right.Index);
      begin
         for I in Left_Key'Range loop
            Left_Key (I) := To_Upper_Case (Left_Key (I));
         end loop;
         for I in Right_Key'Range loop
            Right_Key (I) := To_Upper_Case (Right_Key (I));
         end loop;
         return Left_Key < Right_Key;
      end "<";

      --------------
      -- Scan_Key --
      --------------

      procedure Scan_Key (Catalog  : in Catalog_Type;
                          Facility : in Wide_String;
                          Key      : in Wide_String;
                          Locales  : in Locale_Definitions_Map) is

         pragma Unreferenced (Facility);
         N_Args : constant Natural := Message_Arg_Count (Base_Locale, Locales);

         use Locale_Definitions_Package;
         use Key_Descriptor_Vectors;

         New_Descriptor : Key_Descriptor_Type := (
                                   Index => Get_Key_Index (Catalog, Key),
                                   N_Args => N_Args,
                                   others => <>);
      begin
         Copy_Argument_Types (Base_Locale, Locales, N_Args,
                              New_Descriptor.Arg_Types);
         Key_Descriptors.Append (New_Descriptor);
         if N_Args > Max_Args then
            Max_Args := N_Args;
         end if;
      end Scan_Key;

   begin
      Max_Args := 0;
      Message_Iterate (Handler, Facility, Scan_Key'Access);
      Key_Sorting.Sort (Key_Descriptors);
   end Create_Key_Descriptors;

   -----------------------
   -- Message_Arg_Count --
   -----------------------

   function Message_Arg_Count (Base_Locale : in Wide_String;
                               Locales     : in Locale_Definitions_Map)
      return Natural
   is
      use Locale_Definitions_Package;

      Result    : Natural := 0;

      procedure Count_Args (Position : Cursor);

      procedure Count_Args (Position : Cursor) is
      begin
         Result := Natural'Max (Result,
               Natural (String_Vectors.Length (Element (Position).Arg_Types)));
      end Count_Args;

   begin
      if Has_Element (Locales.Find (Base_Locale)) then
         --  Base locale defined for the message, return the number of args
         --  for the base message.
         Result := Natural (String_Vectors.Length (Element (
                          Locales.Find (Base_Locale)).Arg_Types));
      else
         --  No base message, iterate over the defined locales returning
         --  the maximum number used for any locale.
         Locales.Iterate (Count_Args'Access);
      end if;
      return Result;
   end Message_Arg_Count;

   ---------------------------
   -- Write_Base_Message_Text --
   ---------------------------

   procedure Write_Base_Message_Text (File        : in File_Type;
                                      Catalog     : in Catalog_Type;
                                      Facility    : in Wide_String;
                                      Key         : in Wide_String;
                                      Base_Locale : in Wide_String;
                                      Block_Size  : in Positive) is
   begin
      Write_Commented_Text (File,
                            Get_Text (Catalog, Facility, Key,
                                      Make_Locale (Base_Locale)),
                            Block_Size);
   exception
      when No_Such_Message_Error | No_Such_Locale_Error =>
         Print_Line (File, ZBMBase_Facility, "10022");
   end Write_Base_Message_Text;

end ZBMCompile.Codegen.Accessors;
