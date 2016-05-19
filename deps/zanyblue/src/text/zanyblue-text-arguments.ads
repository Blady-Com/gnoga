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

--
--  The definition of a class hierarchy to categorize arguments into a
--  type tree.  This hierarchy should be used by zbmcompile to generate
--  type aware accessor routines, where possible.  This is a future
--  enhancement.
--
--  Schematically, the type tree is
--
--                                               +-------- Character*
--                                               |
--                             +---- Character --+
--                             |                 |
--                             |                 +-------- Wide_Character*
--             +-- String -----+
--             |               |
--             |               +---- String*
--             |               |
--             |               |
--             |               +---- Wide_String*
--             |
--             |               +---- Integer ------------- Modular
--             |               |
--   Argument -+-- Number -----+
--             |               |
--             |               |           +-------------- Float
--             |               |           |
--             |               +---- Real -+
--             |                           |
--             |                           +-------------- Fixed
--             |
--             +-- Enumeration +---- Boolean*
--             |
--             +-- Time*
--             |
--             +-- Duration*
--             |
--             +-- Exception*
--             |
--             +-- Null*
--
--  The types marked with an asterisk are not defined in this package as
--  the types are implemented as concert class.
--

with Ada.Containers.Indefinite_Vectors;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Format_Errors;

package ZanyBlue.Text.Arguments is

   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Format_Errors;

   type Argument_List is tagged private;
   --  The List represent formatting arguments to a message, e.g.,
   --  an integer, a string, etc.  The list is initially empty with arguments
   --  added using the the Append methods below.

   function Format (List          : Argument_List;
                    Position      : Natural;
                    Message       : Wide_String;
                    Format_String : Wide_String;
                    Locale        : Locale_Type;
                    Raise_Errors  : Boolean;
                    Error_Handler : access Error_Handler_Type'Class
                                       := Standard_Error_Handler'Access)
      return Wide_String;
   --  Format an individual argument using the @Template@ to direct the
   --  conversion.

   function Length (List : Argument_List) return Natural;
   --  Return the length of an argument list, i.e., the number of arguments
   --  current on the list.

   procedure Clear (List : in out Argument_List);
   --  Remove any elements on the list restoring it to an empty list.

   type Argument_Type is abstract tagged null record;
   --  Base type for all arguments types.  Used to define the methods
   --  supported by all arguments: @To_String@, @Format@ and @Append@.

   function Format (Value     : Argument_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Format an argument value using the @Template@ to direct the convesion.

   procedure Append (List      : in out Argument_List;
                     Argument  : Argument_Type'Class);
   --  Append an argument to a list of arguments.

   subtype Any_Category_Type is Argument_Type;
   --  General type used as the unspecified type.

   type Number_Category_Type is
      abstract new Any_Category_Type with null record;
   --  General numeric types (integer, floating point, fixed point etc).

   overriding
   function Format (Value     : Number_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Real_Category_Type is
      abstract new Number_Category_Type with null record;
   --  General category for real numbers.

   overriding
   function Format (Value     : Real_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Float_Category_Type is
      abstract new Real_Category_Type with null record;
   --  Category for floating point numbers.

   overriding
   function Format (Value     : Float_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Fixed_Category_Type is
      abstract new Real_Category_Type with null record;
   --  Category for fixed point numbers.

   overriding
   function Format (Value     : Fixed_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Integer_Category_Type is
      abstract new Number_Category_Type with null record;
   --  General category for integer types.

   overriding
   function Format (Value     : Integer_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Modular_Category_Type is
      abstract new Integer_Category_Type with null record;
   --  General category for modular types (from integers).

   overriding
   function Format (Value     : Modular_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type String_Category_Type is
      abstract new Any_Category_Type with null record;
   --  General category for string types.

   overriding
   function Format (Value     : String_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Character_Category_Type is
      abstract new String_Category_Type with null record;
   --  General category for string types.

   overriding
   function Format (Value     : Character_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Enumeration_Category_Type is
      abstract new Any_Category_Type with null record;
   --  Category for enumeration types.

   overriding
   function Format (Value     : Enumeration_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Boolean_Category_Type is
      abstract new Enumeration_Category_Type with null record;
   --  Category for boolean types.

   overriding
   function Format (Value     : Boolean_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Calendar_Category_Type is
      abstract new Any_Category_Type with null record;
   --  Category for Calendar Time types.

   overriding
   function Format (Value     : Calendar_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Duration_Category_Type is
      abstract new Any_Category_Type with null record;
   --  Category for Ada duration types.

   overriding
   function Format (Value     : Duration_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   type Exception_Category_Type is
      abstract new Any_Category_Type with null record;
   --  Category for Ada duration types.

   overriding
   function Format (Value     : Exception_Category_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String is
      abstract;
   --  Abstract formatting function.

   Any_Format_Name       : aliased constant Wide_String := "any";
   Boolean_Format_Name   : aliased constant Wide_String := "boolean";
   Character_Format_Name : aliased constant Wide_String := "character";
   Date_Format_Name      : aliased constant Wide_String := "date";
   Datetime_Format_Name  : aliased constant Wide_String := "datetime";
   Duration_Format_Name  : aliased constant Wide_String := "duration";
   Enum_Format_Name      : aliased constant Wide_String := "enum";
   Exception_Format_Name : aliased constant Wide_String := "exception";
   Fixed_Format_Name     : aliased constant Wide_String := "fixed";
   Float_Format_Name     : aliased constant Wide_String := "float";
   Integer_Format_Name   : aliased constant Wide_String := "integer";
   Modular_Format_Name   : aliased constant Wide_String := "modular";
   Number_Format_Name    : aliased constant Wide_String := "number";
   Real_Format_Name      : aliased constant Wide_String := "real";
   String_Format_Name    : aliased constant Wide_String := "string";
   Time_Format_Name      : aliased constant Wide_String := "time";
   --  Names that can occur in format strings to specify an argument type

   Any_Category_Name         : aliased constant Wide_String :=
                                    "Argument_Type";
   Boolean_Category_Name     : aliased constant Wide_String :=
                                    "Boolean_Category_Type";
   Character_Category_Name   : aliased constant Wide_String :=
                                    "Character_Category_Type";
   Date_Category_Name        : aliased constant Wide_String :=
                                    "Calendar_Category_Type";
   Datetime_Category_Name    : aliased constant Wide_String :=
                                    "Calendar_Category_Type";
   Duration_Category_Name    : aliased constant Wide_String :=
                                    "Duration_Category_Type";
   Enumeration_Category_Name : aliased constant Wide_String :=
                                    "Enumeration_Category_Type";
   Exception_Category_Name   : aliased constant Wide_String :=
                                    "Exception_Category_Type";
   Fixed_Category_Name       : aliased constant Wide_String :=
                                    "Fixed_Category_Type";
   Float_Category_Name       : aliased constant Wide_String :=
                                    "Float_Category_Type";
   Integer_Category_Name     : aliased constant Wide_String :=
                                    "Integer_Category_Type";
   Modular_Category_Name     : aliased constant Wide_String :=
                                    "Modular_Category_Type";
   Number_Category_Name      : aliased constant Wide_String :=
                                    "Number_Category_Type";
   Real_Category_Name        : aliased constant Wide_String :=
                                    "Real_Category_Type";
   String_Category_Name      : aliased constant Wide_String :=
                                    "String_Category_Type";
   Time_Category_Name        : aliased constant Wide_String :=
                                    "Calendar_Category_Type";

   function Type_Name_Prefix (Format_String : Wide_String) return Wide_String;
   --  Return the longest prefix string of the Format_String that identifies
   --  a format type, e.g.,
   --
   --    Type_Name_Prefix ("date,EEE") => "date"
   --    Type_Name_Prefix ("date") => "date"
   --

   function Type_Name_To_Category (Type_Name : Wide_String) return Wide_String;
   --  Return the argument category type asscociated with a type name, e.g.,
   --  "date", "time" and "datetime" all map to "Calendar_Category_Type", etc.

   Empty_Argument_List : constant Argument_List;

private

   use Ada.Containers;

   package Argument_Vectors is
      new Indefinite_Vectors (Index_Type => Natural,
                              Element_Type => Argument_Type'Class);
   type Argument_List is tagged record
      Contents : Argument_Vectors.Vector;
   end record;

   Empty_Argument_List : constant Argument_List := (Contents => <>);

end ZanyBlue.Text.Arguments;
