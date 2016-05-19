--  -*- coding: utf-8 -*-
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;

with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;

package ZBMCompile.Parser_Handler is

   use Ada.Containers;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;

   type Key_Definition is
      record
         Line_Number : Positive := 1;
         Arg_Types   : String_Vectors.Vector;
      end record;
   --  Information about the definition of a key for a message.  A list of
   --  these records is maintained to cover the locales loaded.  This
   --  information is used to check the cross locale consistency of messages,
   --  i.e., does a localized message refer to an argument number beyond
   --  the numbers used by the base locale.

   package Locale_Definitions_Package is
      new Indefinite_Hashed_Maps (Key_Type        => Wide_String,
                                  Element_Type    => Key_Definition,
                                  Hash            => Wide_Hash,
                                  Equivalent_Keys => "=");
   subtype Locale_Definitions_Map is Locale_Definitions_Package.Map;
   function "=" (Left, Right : Locale_Definitions_Map) return Boolean;
   --  Associate a locale with the description of the key defined in it.

   package Key_Definitions_Package is
      new Indefinite_Hashed_Maps (Key_Type        => Wide_String,
                                  Element_Type    => Locale_Definitions_Map,
                                  Hash            => Wide_Hash,
                                  Equivalent_Keys => "=");
   subtype Key_Definitions_Map is Key_Definitions_Package.Map;
   function "=" (Left, Right : Key_Definitions_Map) return Boolean;
   --  Associate keys with their definitions in various locales

   package Locale_Sets is
      new Indefinite_Hashed_Sets (Element_Type        => Wide_String,
                                  Hash                => Wide_Hash,
                                  Equivalent_Elements => "=");
   subtype Locale_Set_Type is Locale_Sets.Set;
   --  Is a particular locale defined for a facility

   type Facility_Descriptor_Type is
      record
          Keys    : Key_Definitions_Map;
          Locales : Locale_Set_Type;
      end record;
   --  Description of the information gathered for a particular facility.
   --  This is used for both inter-locale consistency checks and the generation
   --  of accessor routines.

   package Facility_Descriptor_Package is
      new Indefinite_Hashed_Maps (Key_Type        => Wide_String,
                                  Element_Type    => Facility_Descriptor_Type,
                                  Hash            => Wide_Hash,
                                  Equivalent_Keys => "=");
   subtype Facility_Descriptor_Map is Facility_Descriptor_Package.Map;
   --  Associate facilities with the keys defined for it.

   type ZBMC_Handler_Type (Unchecked : Boolean) is
      new Catalog_Handler_Type with record
         Message_Info : Facility_Descriptor_Map;
      end record;
   --  The parser handler type used when parsing ".properties" files.

   overriding
   procedure Add_Key_Value (Handler       : in out ZBMC_Handler_Type;
                            Facility      : Wide_String;
                            Key           : Wide_String;
                            Value         : Wide_String;
                            Locale        : Locale_Type;
                            Source_Locale : Locale_Type;
                            File_Name     : Wide_String;
                            Line_Number   : Natural);
   --  Call back used to add messages to a catalog.  Simply verify the message
   --  and then invoke our parent's method to do the actually addition.

   overriding
   procedure Duplicate_Key (Handler       : in out ZBMC_Handler_Type;
                            Facility      : Wide_String;
                            Key           : Wide_String;
                            Locale        : Locale_Type;
                            File_Name     : Wide_String;
                            Current_Line  : Natural;
                            Previous_Line : Natural);
   --  Call back used to report a duplicate key error.

   overriding
   procedure Invalid_Character (Handler         : in out ZBMC_Handler_Type;
                                Facility        : Wide_String;
                                File_Name       : Wide_String;
                                Current_Line    : Natural;
                                Ch              : Character);
   --  Call back used to report an invalid character, non-ISO-646, in the
   --  source properties file.

   overriding
   procedure Invalid_Definition (Handler         : in out ZBMC_Handler_Type;
                                 Facility        : Wide_String;
                                 Locale          : Locale_Type;
                                 File_Name       : Wide_String;
                                 Current_Line    : Natural;
                                 Additional_Info : String);
   --  Call back used to report an invalid .properties file line error.

   procedure Message_Iterate (
      Handler  : ZBMC_Handler_Type;
      Facility : Wide_String;
      Callback : not null access procedure
                                    (Catalog  : Catalog_Type;
                                     Facility : Wide_String;
                                     Key      : Wide_String;
                                     Locales  : Locale_Definitions_Map));
   --  Iterate over the messages defined for a facility by message key.  The
   --  Locale_Info vector gives information on whether or not a key is defined
   --  for a particular locale index and, if defined, the number of arguments
   --  the message expects.

   function Facility_Defines_Locale (Handler  : ZBMC_Handler_Type;
                                     Facility : Wide_String;
                                     Locale   : Wide_String) return Boolean;
   --  Was the given locale loaded for the given facility?

end ZBMCompile.Parser_Handler;
