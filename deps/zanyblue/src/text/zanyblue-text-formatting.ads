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

with Ada.Calendar;
with Ada.Exceptions;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Filter;
with ZanyBlue.Text.Times;
with ZanyBlue.Text.Floats;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Strings;
with ZanyBlue.Text.Integers;
with ZanyBlue.Text.Booleans;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Durations;
with ZanyBlue.Text.Characters;
with ZanyBlue.Text.Exceptions;
with ZanyBlue.Text.Long_Floats;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Null_Object;
with ZanyBlue.Text.Wide_Characters;
with ZanyBlue.Text.Wide_Wide_Characters;

pragma Elaborate_All (ZanyBlue.Text.Pseudo);
pragma Elaborate_All (ZanyBlue.Text.Catalogs);
pragma Elaborate_All (ZanyBlue.Text.Times);
pragma Elaborate_All (ZanyBlue.Text.Floats);
pragma Elaborate_All (ZanyBlue.Text.Locales);
pragma Elaborate_All (ZanyBlue.Text.Strings);
pragma Elaborate_All (ZanyBlue.Text.Integers);
pragma Elaborate_All (ZanyBlue.Text.Booleans);
pragma Elaborate_All (ZanyBlue.Text.Durations);
pragma Elaborate_All (ZanyBlue.Text.Characters);
pragma Elaborate_All (ZanyBlue.Text.Exceptions);
pragma Elaborate_All (ZanyBlue.Text.Long_Floats);
pragma Elaborate_All (ZanyBlue.Text.Arguments);
pragma Elaborate_All (ZanyBlue.Text.Null_Object);
pragma Elaborate_All (ZanyBlue.Text.Wide_Characters);
pragma Elaborate_All (ZanyBlue.Text.Wide_Wide_Characters);

package ZanyBlue.Text.Formatting is

   use Ada.Exceptions;
   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Filter;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Null_Object;

   --  The following "+" definitons simply make the "+" method visible
   --  to client using this package, simplifying the overal usage of the
   --  ZanyBlue.Text library.
   function "+"
     (String_Value : String)
      return ZanyBlue.Text.Strings.String_Argument_Type renames
     ZanyBlue.Text.Strings.Create;

--     function "+"
--       (Unbounded_String_Value : Unbounded_String)
--        return ZanyBlue.Text.Strings.String_Argument_Type renames
--       ZanyBlue.Text.Unbounded_Strings.Create;
--
--     function "+"
--       (Unbounded_Wide_String_Value : Unbounded_Wide_String)
--        return ZanyBlue.Text.Wide_Strings.Wide_String_Argument_Type renames
--       ZanyBlue.Text.Unbounded_Wide_Strings.Create;

--     function "+"
--       (Wide_String_Value : String)
--        return ZanyBlue.Text.Wide_Strings.Wide_String_Argument_Type renames
--       ZanyBlue.Text.Wide_Strings.Create;

   function "+"
     (Float_Value : Float)
      return ZanyBlue.Text.Floats.Float_Argument_Type renames
     ZanyBlue.Text.Floats.Create;

   function "+"
     (Float_Value : Long_Float)
      return ZanyBlue.Text.Long_Floats.Float_Argument_Type renames
     ZanyBlue.Text.Long_Floats.Create;

   function "+"
     (Integer_Value : Integer)
      return ZanyBlue.Text.Integers.Integer_Argument_Type renames
     ZanyBlue.Text.Integers.Create;

   function "+"
     (Time_Value : Ada.Calendar.Time)
      return ZanyBlue.Text.Times.Time_Argument_Type renames
     ZanyBlue.Text.Times.Create;

   function "+"
     (Duration_Value : Duration)
      return ZanyBlue.Text.Durations.Duration_Argument_Type renames
     ZanyBlue.Text.Durations.Create;

   function "+"
     (Character_Value : Character)
      return ZanyBlue.Text.Characters.Character_Argument_Type renames
     ZanyBlue.Text.Characters.Create;

   function "+"
     (Value : Exception_Occurrence)
      return ZanyBlue.Text.Exceptions.Exception_Argument_Type renames
     ZanyBlue.Text.Exceptions.Create;

   function "+"
     (Wide_Character_Value : Wide_Character)
      return ZanyBlue.Text.Wide_Characters
     .Wide_Character_Argument_Type renames
     ZanyBlue.Text.Wide_Characters.Create;

   function "+"
     (Wide_Wide_Character_Value : Wide_Wide_Character)
      return ZanyBlue.Text.Wide_Wide_Characters
     .Wide_Wide_Character_Argument_Type renames
     ZanyBlue.Text.Wide_Wide_Characters.Create;

   function "+"
     (Boolean_Value : Boolean)
      return ZanyBlue.Text.Booleans.Boolean_Argument_Type renames
     ZanyBlue.Text.Booleans.Create;

   function Standard_Catalog return Catalog_Type;
   --  Return the standard catalog managed by the ZanyBlue library to hold
   --  normal application messages.  Explicit reference to a catalog is not
   --  normally used, in this context, this standard catalog is used.

   procedure Enable_Exceptions;
   --  Enable exceptions for missing arguments, invalid formats, etc.

   procedure Disable_Exceptions;
   --  Disable exceptions for missing arguments, invalid formats, etc.

   function Exceptions_Enabled return Boolean;
   --  Are exceptions enabled for the catalog

   procedure Enable_Source_Locales;
   --  Enable usage of source locales for message argument formatting.

   procedure Disable_Source_Locales;
   --  Disable usage of source locales for message argument formatting.

--     procedure Enable_Wide_IO;
   --  Enable usage of Ada.Wide_Text_IO.Current_Output for Print/Print_Line
   --  when no explicit destination is defined.

--     procedure Disable_Wide_IO;
   --  Enable usage of Ada.Text_IO.Current_Output for Print/Print_Line
   --  when no explicit destination is defined.

   function Source_Locales_Enabled return Boolean;
   --  Are source locales for message argument formatting enabled.

   procedure Set_Filter
     (Filter  : Message_Filter_Access;
      Catalog : Catalog_Type := Standard_Catalog);
   --  Set the filter associated with the standard catalog, can be null.

   function Format
     (Facility  : String;
      Key       : String;
      Arguments : Argument_List;
      Locale    : Locale_Type  := Current_Locale;
      Catalog   : Catalog_Type := Standard_Catalog)
      return String;
   --  Format a message with arguments to a String for display or
   --  printing.

   function Format
     (Facility  : String;
      Key       : String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale;
      Catalog   : Catalog_Type        := Standard_Catalog)
      return String;
   --  Format a message with 5 possible arguments to a String for
   --  display or printing.

   function Format
     (Text      : String;
      Arguments : Argument_List;
      Locale    : Locale_Type := Current_Locale)
      return String;
   --  Format a message with arguments to a String for display or
   --  printing.

   function Format
     (Text      : String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale)
      return String;
   --  Format a message with 5 possible arguments to a String for
   --  display or printing.

--     function Format
--       (Facility  : String;
--        Key       : String;
--        Arguments : Argument_List;
--        Locale    : Locale_Type  := Current_Locale;
--        Catalog   : Catalog_Type := Standard_Catalog)
--        return String;
--     --  Format a message with arguments to a String for display or
--     --  printing.
--
--     function Format
--       (Facility  : String;
--        Key       : String;
--        Argument0 : Argument_Type'Class := Null_Argument;
--        Argument1 : Argument_Type'Class := Null_Argument;
--        Argument2 : Argument_Type'Class := Null_Argument;
--        Argument3 : Argument_Type'Class := Null_Argument;
--        Argument4 : Argument_Type'Class := Null_Argument;
--        Locale    : Locale_Type         := Current_Locale;
--        Catalog   : Catalog_Type        := Standard_Catalog)
--        return String;
--     --  Format a message with 5 possible arguments to a String for
--     --  display or printing.
--
--     function Format
--       (Text      : String;
--        Arguments : Argument_List;
--        Locale    : Locale_Type := Current_Locale)
--        return String;
--     --  Format a message with arguments to a String for display or
--     --  printing.
--
--     function Format
--       (Text      : String;
--        Argument0 : Argument_Type'Class := Null_Argument;
--        Argument1 : Argument_Type'Class := Null_Argument;
--        Argument2 : Argument_Type'Class := Null_Argument;
--        Argument3 : Argument_Type'Class := Null_Argument;
--        Argument4 : Argument_Type'Class := Null_Argument;
--        Locale    : Locale_Type         := Current_Locale)
--        return String;
--     --  Format a message with 5 possible arguments to a String for
--     --  display or printing.

   procedure Print
     (Facility  : String;
      Key       : String;
      Arguments : Argument_List;
      Locale    : Locale_Type  := Current_Locale;
      Catalog   : Catalog_Type := Standard_Catalog);
   --  Print a message with arguments.

   procedure Print_Line
     (Facility  : String;
      Key       : String;
      Arguments : Argument_List;
      Locale    : Locale_Type  := Current_Locale;
      Catalog   : Catalog_Type := Standard_Catalog);
   --  Print a message with arguments.

   procedure Print
     (Facility  : String;
      Key       : String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale;
      Catalog   : Catalog_Type        := Standard_Catalog);
   --  Print a message with upto 5 arguments.

   procedure Print_Line
     (Facility  : String;
      Key       : String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale;
      Catalog   : Catalog_Type        := Standard_Catalog);
   --  Print a message with upto 5 arguments.

--     procedure Print
--       (Destination : Ada.Wide_Text_IO.File_Type;
--        Facility    : String;
--        Key         : String;
--        Arguments   : Argument_List;
--        Locale      : Locale_Type  := Current_Locale;
--        Catalog     : Catalog_Type := Standard_Catalog);
   --  Print a message with arguments.

   procedure Print
     (Destination : UXStrings.Text_IO.File_Type;
      Facility    : String;
      Key         : String;
      Arguments   : Argument_List;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog);
   --  Print a message with arguments.

--     procedure Print_Line
--       (Destination : Ada.Wide_Text_IO.File_Type;
--        Facility    : String;
--        Key         : String;
--        Arguments   : Argument_List;
--        Locale      : Locale_Type  := Current_Locale;
--        Catalog     : Catalog_Type := Standard_Catalog);
   --  Print a message with arguments.

   procedure Print_Line
     (Destination : UXStrings.Text_IO.File_Type;
      Facility    : String;
      Key         : String;
      Arguments   : Argument_List;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog);
   --  Print a message with arguments.

--     procedure Print
--       (Destination : Ada.Wide_Text_IO.File_Type;
--        Facility    : String;
--        Key         : String;
--        Argument0   : Argument_Type'Class := Null_Argument;
--        Argument1   : Argument_Type'Class := Null_Argument;
--        Argument2   : Argument_Type'Class := Null_Argument;
--        Argument3   : Argument_Type'Class := Null_Argument;
--        Argument4   : Argument_Type'Class := Null_Argument;
--        Locale      : Locale_Type         := Current_Locale;
--        Catalog     : Catalog_Type        := Standard_Catalog);
   --  Print a message with upto 5 arguments.

   procedure Print
     (Destination : UXStrings.Text_IO.File_Type;
      Facility    : String;
      Key         : String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale;
      Catalog     : Catalog_Type        := Standard_Catalog);
   --  Print a message with upto 5 arguments.

--     procedure Print_Line
--       (Destination : Ada.Wide_Text_IO.File_Type;
--        Facility    : String;
--        Key         : String;
--        Argument0   : Argument_Type'Class := Null_Argument;
--        Argument1   : Argument_Type'Class := Null_Argument;
--        Argument2   : Argument_Type'Class := Null_Argument;
--        Argument3   : Argument_Type'Class := Null_Argument;
--        Argument4   : Argument_Type'Class := Null_Argument;
--        Locale      : Locale_Type         := Current_Locale;
--        Catalog     : Catalog_Type        := Standard_Catalog);
   --  Print a message with upto 5 arguments.

   procedure Print_Line
     (Destination : UXStrings.Text_IO.File_Type;
      Facility    : String;
      Key         : String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale;
      Catalog     : Catalog_Type        := Standard_Catalog);
   --  Print a message with upto 5 arguments.

   procedure Print
     (Text      : String;
      Arguments : Argument_List;
      Locale    : Locale_Type := Current_Locale);
   --  Print a message with arguments.

   procedure Print_Line
     (Text      : String;
      Arguments : Argument_List;
      Locale    : Locale_Type := Current_Locale);
   --  Print a message with arguments.

   procedure Print
     (Text      : String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale);
   --  Print a message with upto 5 arguments.

   procedure Print_Line
     (Text      : String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale);
   --  Print a message with upto 5 arguments.

--     procedure Print
--       (Destination : Ada.Wide_Text_IO.File_Type;
--        Text        : String;
--        Arguments   : Argument_List;
--        Locale      : Locale_Type := Current_Locale);
   --  Print a message with arguments.

   procedure Print
     (Destination : UXStrings.Text_IO.File_Type;
      Text        : String;
      Arguments   : Argument_List;
      Locale      : Locale_Type := Current_Locale);
   --  Print a message with arguments.

--     procedure Print_Line
--       (Destination : Ada.Wide_Text_IO.File_Type;
--        Text        : String;
--        Arguments   : Argument_List;
--        Locale      : Locale_Type := Current_Locale);
   --  Print a message with arguments.

   procedure Print_Line
     (Destination : UXStrings.Text_IO.File_Type;
      Text        : String;
      Arguments   : Argument_List;
      Locale      : Locale_Type := Current_Locale);
   --  Print a message with arguments.

--     procedure Print
--       (Destination : Ada.Wide_Text_IO.File_Type;
--        Text        : String;
--        Argument0   : Argument_Type'Class := Null_Argument;
--        Argument1   : Argument_Type'Class := Null_Argument;
--        Argument2   : Argument_Type'Class := Null_Argument;
--        Argument3   : Argument_Type'Class := Null_Argument;
--        Argument4   : Argument_Type'Class := Null_Argument;
--        Locale      : Locale_Type         := Current_Locale);
   --  Print a message with upto 5 arguments.

   procedure Print
     (Destination : UXStrings.Text_IO.File_Type;
      Text        : String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale);
   --  Print a message with upto 5 arguments.

--     procedure Print_Line
--       (Destination : Ada.Wide_Text_IO.File_Type;
--        Text        : String;
--        Argument0   : Argument_Type'Class := Null_Argument;
--        Argument1   : Argument_Type'Class := Null_Argument;
--        Argument2   : Argument_Type'Class := Null_Argument;
--        Argument3   : Argument_Type'Class := Null_Argument;
--        Argument4   : Argument_Type'Class := Null_Argument;
--        Locale      : Locale_Type         := Current_Locale);
   --  Print a message with upto 5 arguments.

   procedure Print_Line
     (Destination : UXStrings.Text_IO.File_Type;
      Text        : String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale);
   --  Print a message with upto 5 arguments.

   procedure Raise_Exception
     (E       : Ada.Exceptions.Exception_Id;
      Message : String);
   pragma No_Return (Raise_Exception);
   --  Raise an exception with a wide message, encoded as UTF-8

   procedure Raise_Exception
     (E         : Ada.Exceptions.Exception_Id;
      Facility  : String;
      Key       : String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale;
      Catalog   : Catalog_Type        := Standard_Catalog);
   pragma No_Return (Raise_Exception);
   --  Raise an exception with a wide message, encoded as UTF-8

   procedure Raise_Exception
     (E         : Ada.Exceptions.Exception_Id;
      Facility  : String;
      Key       : String;
      Arguments : Argument_List;
      Locale    : Locale_Type  := Current_Locale;
      Catalog   : Catalog_Type := Standard_Catalog);
   pragma No_Return (Raise_Exception);
   --  Raise an exception with a wide message, encoded as UTF-8

   procedure Raise_Exception
     (E         : Ada.Exceptions.Exception_Id;
      Text      : String;
      Argument0 : Argument_Type'Class;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale);
   pragma No_Return (Raise_Exception);
   --  Raise an exception with a wide message, encoded as UTF-8

   procedure Pseudo_Translate
     (Mapping : Pseudo_Map_Vector;
      Catalog : Catalog_Type := Standard_Catalog);
   --  Enable pseudo translations for a catalog.

--     procedure Write_Message
--       (Destination : Ada.Wide_Text_IO.File_Type;
--        Facility    : String;
--        Key         : String;
--        Arguments   : Argument_List;
--        With_NL     : Boolean;
--        Locale      : Locale_Type;
--        Catalog     : Catalog_Type);
   --  If not filtered, format the message and write to the underlying
   --  file.

   procedure Write_Message
     (Destination : UXStrings.Text_IO.File_Type;
      Facility    : String;
      Key         : String;
      Arguments   : Argument_List;
      With_NL     : Boolean;
      Locale      : Locale_Type;
      Catalog     : Catalog_Type);
   --  If not filtered, format the message and write to the underlying
   --  file.

end ZanyBlue.Text.Formatting;
