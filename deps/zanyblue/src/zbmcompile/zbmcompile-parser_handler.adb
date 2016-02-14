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

with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Format_Errors;
with ZanyBlue.Text.Format_Message;

package body ZBMCompile.Parser_Handler is

   use ZanyBlue.Text.Format_Errors;
   use ZanyBlue.Text.Formatting;

   procedure Discard (Value : Wide_String);
   --  Simply discard the generated wide string value.

   procedure Add_Definition (Handler     : in out ZBMC_Handler_Type;
                             Catalog     : Catalog_Type;
                             Facility    : Wide_String;
                             Key         : Wide_String;
                             Locale      : Locale_Type;
                             File_Name   : Wide_String;
                             Line_Number : Natural;
                             Arg_Types   : String_Vectors.Vector);
   --  Add a message definition to the summary data structure used to support
   --  consistency checks.

   ---------
   -- "=" --
   ---------
   --
   --  Two of the same element are never added to the map, always return
   --  False?  Not sure if this is OK.
   --

   function "=" (Left, Right : Locale_Definitions_Map) return Boolean is
      pragma Unreferenced (Left);
      pragma Unreferenced (Right);
   begin
      return False;
   end "=";

   ---------
   -- "=" --
   ---------
   --
   --  Two of the same element are never added to the map, always return
   --  False?  Not sure if this is OK.
   --

   function "=" (Left, Right : Key_Definitions_Map) return Boolean is
      pragma Unreferenced (Left);
      pragma Unreferenced (Right);
   begin
      return False;
   end "=";

   --------------------
   -- Add_Definition --
   --------------------

   procedure Add_Definition (Handler     : in out ZBMC_Handler_Type;
                             Catalog     : Catalog_Type;
                             Facility    : Wide_String;
                             Key         : Wide_String;
                             Locale      : Locale_Type;
                             File_Name   : Wide_String;
                             Line_Number : Natural;
                             Arg_Types   : String_Vectors.Vector) is

      pragma Unreferenced (Catalog);
      pragma Unreferenced (File_Name);

      procedure Update_Key_Info (Facility : Wide_String;
                                 FD       : in out Facility_Descriptor_Type);

      procedure Update_Locale_Info (Key   : Wide_String;
                                    LI    : in out Locale_Definitions_Map);

      procedure Update_Key_Info (Facility : Wide_String;
                                 FD       : in out Facility_Descriptor_Type) is
         pragma Unreferenced (Facility);

         use Key_Definitions_Package;

         New_Item : Locale_Definitions_Map;
         Position : Cursor;
         Inserted : Boolean;

      begin
         FD.Keys.Insert (Key, New_Item, Position, Inserted);
         Update_Element (FD.Keys, Position, Update_Locale_Info'Access);
         FD.Locales.Include (Locale_Name (Locale));
      end Update_Key_Info;

      procedure Update_Locale_Info (Key   : Wide_String;
                                    LI    : in out Locale_Definitions_Map) is
         pragma Unreferenced (Key);

         use String_Vectors;
         use Locale_Definitions_Package;

         N_Arguments : constant Natural := Natural (Length (Arg_Types));
         New_Item    : Key_Definition := (Line_Number => Line_Number,
                                          others => <>);

      begin
         for I in 1 ..  N_Arguments loop
            Append (New_Item.Arg_Types, Element (Arg_Types, I - 1));
         end loop;
         LI.Include (Locale_Name (Locale), New_Item);
      end Update_Locale_Info;

      use Facility_Descriptor_Package;

      New_Item : constant Facility_Descriptor_Type := (others => <>);
      Position : Cursor;
      Inserted : Boolean;

   begin
      Handler.Message_Info.Insert (Facility, New_Item, Position, Inserted);
      Update_Element (Handler.Message_Info, Position, Update_Key_Info'Access);
   end Add_Definition;

   -------------------
   -- Add_Key_Value --
   -------------------

   overriding
   procedure Add_Key_Value (Handler       : in out ZBMC_Handler_Type;
                            Facility      : Wide_String;
                            Key           : Wide_String;
                            Value         : Wide_String;
                            Locale        : Locale_Type;
                            Source_Locale : Locale_Type;
                            File_Name     : Wide_String;
                            Line_Number   : Natural) is

      type Verify_Handler_Type is new Error_Handler_Type with
         record
            N_Error   : Natural := 0;
            Arg_Types : String_Vectors.Vector;
         end record;
      --  Messages defined via the .properties file are verified by formatting
      --  them with no arguments.  The call backs on this tagged type allow
      --  the reporting of formatting errors and the determination of the
      --  number of argument references in the message.

      overriding
      procedure Format_Not_Closed (V_Handler    : in out Verify_Handler_Type;
                                   Message      : Wide_String;
                                   Position     : Positive;
                                   Level        : Natural;
                                   Raise_Errors : Boolean);
      --  Report a format not closed error in a message.

      overriding
      procedure Illegal_Character (V_Handler    : in out Verify_Handler_Type;
                                   Message      : Wide_String;
                                   Position     : Positive;
                                   Ch           : Wide_Character;
                                   Level        : Natural;
                                   Raise_Errors : Boolean);
      --  Report an illegal format character error in a message.

      overriding
      procedure Missing_Argument (V_Handler    : in out Verify_Handler_Type;
                                  Message      : Wide_String;
                                  Position     : Natural;
                                  Type_Name    : Wide_String;
                                  Raise_Errors : Boolean);
      --  Report a missing argument reference, used to count the number of
      --  such references in a message.

      -----------------------
      -- Format_Not_Closed --
      -----------------------

      overriding
      procedure Format_Not_Closed (V_Handler    : in out Verify_Handler_Type;
                                   Message      : Wide_String;
                                   Position     : Positive;
                                   Level        : Natural;
                                   Raise_Errors : Boolean) is
         pragma Unreferenced (Message);
         pragma Unreferenced (Level);
         pragma Unreferenced (Raise_Errors);
      begin
         if not Handler.Unchecked then
            Print_Line (ZBMCompile_Facility, "E00011",
                        Argument0 => +File_Name,
                        Argument1 => +Line_Number,
                        Argument2 => +Key,
                        Argument3 => +Position);
            V_Handler.N_Error := V_Handler.N_Error + 1;
         end if;
      end Format_Not_Closed;

      -----------------------
      -- Illegal_Character --
      -----------------------

      overriding
      procedure Illegal_Character (V_Handler    : in out Verify_Handler_Type;
                                   Message      : Wide_String;
                                   Position     : Positive;
                                   Ch           : Wide_Character;
                                   Level        : Natural;
                                   Raise_Errors : Boolean) is

         pragma Unreferenced (Message);
         pragma Unreferenced (Level);
         pragma Unreferenced (Raise_Errors);

      begin
         if not Handler.Unchecked then
            Print_Line (ZBMCompile_Facility, "E00012",
                        Argument0 => +File_Name,
                        Argument1 => +Line_Number,
                        Argument2 => +Key,
                        Argument3 => +Ch,
                        Argument4 => +Position);
            V_Handler.N_Error := V_Handler.N_Error + 1;
         end if;
      end Illegal_Character;

      ----------------------
      -- Missing_Argument --
      ----------------------

      overriding
      procedure Missing_Argument (V_Handler    : in out Verify_Handler_Type;
                                  Message      : Wide_String;
                                  Position     : Natural;
                                  Type_Name    : Wide_String;
                                  Raise_Errors : Boolean) is
         pragma Unreferenced (Message);
         pragma Unreferenced (Raise_Errors);

         use String_Vectors;

         Type_Category : constant Wide_String :=
                             Type_Name_To_Category (Type_Name);
         Extra : constant Integer := Position + 1
                                      - Natural (Length (V_Handler.Arg_Types));

      begin
         if Extra > 0 then
            Append (V_Handler.Arg_Types, "", Count_Type (Extra));
         end if;
         if Element (V_Handler.Arg_Types, Position) = "" then
            Replace_Element (V_Handler.Arg_Types, Position, Type_Category);
         elsif Element (V_Handler.Arg_Types, Position) /= Type_Category then
            Print_Line (ZBMCompile_Facility, "E00027",
                            +File_Name, +Line_Number, +Position,
                            +Element (V_Handler.Arg_Types, Position),
                            +Type_Category);
            Replace_Element (V_Handler.Arg_Types, Position, "");
            V_Handler.N_Error := V_Handler.N_Error + 1;
         end if;
      end Missing_Argument;

      Catalog     : constant Catalog_Type := Handler.Get_Catalog;
      Verifier    : aliased Verify_Handler_Type;
      Arguments   : Argument_List;

   begin
      Add_Key_Value (Catalog_Handler_Type (Handler),
                     Facility, Key, Value, Locale, Source_Locale,
                     File_Name, Line_Number);
      Discard (Format_Message (Value, Arguments, null, Locale,
                               Raise_Errors => False,
                               Mark_Messages => False,
                               Mark_Arguments => False,
                               Error_Handler => Verifier'Access));
      Handler.Increment_Errors (By_Amount => Verifier.N_Error);
      Add_Definition (Handler, Catalog, Facility, Key, Locale,
                      File_Name, Line_Number, Verifier.Arg_Types);
   end Add_Key_Value;

   -------------
   -- Discard --
   -------------

   procedure Discard (Value : Wide_String) is
      pragma Unreferenced (Value);
   begin
      null;
   end Discard;

   -------------------
   -- Duplicate_Key --
   -------------------

   overriding
   procedure Duplicate_Key (Handler       : in out ZBMC_Handler_Type;
                            Facility      : Wide_String;
                            Key           : Wide_String;
                            Locale        : Locale_Type;
                            File_Name     : Wide_String;
                            Current_Line  : Natural;
                            Previous_Line : Natural) is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
      pragma Unreferenced (Locale);
   begin
      Print_Line (ZBMCompile_Facility, "E00004",
                  +File_Name, +Current_Line, +Key, +Previous_Line);
   end Duplicate_Key;

   -----------------------------
   -- Facility_Defines_Locale --
   -----------------------------

   function Facility_Defines_Locale (Handler  : ZBMC_Handler_Type;
                                     Facility : Wide_String;
                                     Locale   : Wide_String) return Boolean
   is

      Result         : Boolean := True;

      procedure Query_Facility (Facility_Name : Wide_String;
                                FI            : Facility_Descriptor_Type);
      --  Helper procedure to access the stored data on a facility.

      --------------------
      -- Query_Facility --
      --------------------

      procedure Query_Facility (Facility_Name : Wide_String;
                                FI            : Facility_Descriptor_Type) is
         pragma Unreferenced (Facility_Name);
         use Locale_Sets;
      begin
         Result := Has_Element (Find (FI.Locales, Locale));
      end Query_Facility;

      use Facility_Descriptor_Package;

      Position : constant Cursor := Handler.Message_Info.Find (Facility);

   begin
      if Position /= No_Element then
         Query_Element (Position, Query_Facility'Access);
      else
         Result := False;
      end if;
      return Result;
   exception
   when Constraint_Error | No_Such_Locale_Error | No_Such_Facility_Error =>
      return False;
   end Facility_Defines_Locale;

   -----------------------
   -- Invalid_Character --
   -----------------------

   overriding
   procedure Invalid_Character (Handler         : in out ZBMC_Handler_Type;
                                Facility        : Wide_String;
                                File_Name       : Wide_String;
                                Current_Line    : Natural;
                                Ch              : Character) is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
      pragma Unreferenced (Ch);
   begin
      Print_Line (ZBMCompile_Facility, "E00028",
                  Argument0 => +File_Name,
                  Argument1 => +Current_Line);
   end Invalid_Character;

   ------------------------
   -- Invalid_Definition --
   ------------------------

   overriding
   procedure Invalid_Definition (Handler         : in out ZBMC_Handler_Type;
                                 Facility        : Wide_String;
                                 Locale          : Locale_Type;
                                 File_Name       : Wide_String;
                                 Current_Line    : Natural;
                                 Additional_Info : String) is

      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
      pragma Unreferenced (Locale);

   begin
      Print_Line (ZBMCompile_Facility, "E00003",
                  Argument0 => +File_Name,
                  Argument1 => +Current_Line,
                  Argument2 => +Additional_Info);
   end Invalid_Definition;

   ---------------------
   -- Message_Iterate --
   ---------------------

   procedure Message_Iterate (
      Handler  : ZBMC_Handler_Type;
      Facility : Wide_String;
      Callback : not null access procedure
                                    (Catalog  : Catalog_Type;
                                     Facility : Wide_String;
                                     Key      : Wide_String;
                                     Locales  : Locale_Definitions_Map))
   is

      Catalog : constant Catalog_Type := Get_Catalog (Handler);

      procedure Read_Facility (Facility_Name : Wide_String;
                               Facility_Data : Facility_Descriptor_Type);
      --  Read the data associated with the facility and call the callback
      --  routine.

      procedure Read_Facility (Facility_Name : Wide_String;
                               Facility_Data : Facility_Descriptor_Type) is

         use Key_Definitions_Package;

         procedure Iterate_Keys (Position : Cursor);

         procedure Iterate_Keys (Position : Cursor) is
            Locales : constant Locale_Definitions_Map := Element (Position);
         begin
            if Natural (Locale_Definitions_Package.Length (Locales)) > 0 then
               Callback (Catalog, Facility_Name, Key (Position), Locales);
            end if;
         end Iterate_Keys;

      begin
         Facility_Data.Keys.Iterate (Iterate_Keys'Access);
      end Read_Facility;

      use Facility_Descriptor_Package;

      Position : constant Cursor := Handler.Message_Info.Find (Facility);

   begin
      Query_Element (Position, Read_Facility'Access);
   end Message_Iterate;

end ZBMCompile.Parser_Handler;
