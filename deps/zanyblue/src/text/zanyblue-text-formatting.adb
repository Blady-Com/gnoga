--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2017, Michael Rohan <mrohan@zanyblue.com>
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

with ZanyBlue.Text.Format_Errors;
with ZanyBlue.Text.Format_Message;

package body ZanyBlue.Text.Formatting is

   use ZanyBlue.Text.Format_Errors;

   procedure Make_Arguments
     (Arguments : in out Argument_List;
      Argument0 :        Argument_Type'Class;
      Argument1 :        Argument_Type'Class;
      Argument2 :        Argument_Type'Class;
      Argument3 :        Argument_Type'Class;
      Argument4 :        Argument_Type'Class);
   --  Construct an Argument_List given a set of Argument_Type
   --  values.  To catch No_Such_Argument references, the list generated
   --  includes those arguments upto the first argument that is a
   --  Null_Argument.  It's assumed this null argument starts the argument
   --  defaulting in the original call.

   My_Catalog : constant Catalog_Type := Create;
   --  Catalog used by applications that don't manage their own catalogs.

   Use_Wide_IO : Boolean := True;
   --  Should Print/Print_Line without an explicit destination to use the
   --  Wide_Text_IO Current_Output or the Text_IO Current_Output.

   --------------------
   -- Format_Message --
   --------------------

   function Format_Message
     (Message        : Wide_String;
      Arguments      : Argument_List;
      Mapping        : Pseudo_Map_Access;
      Locale         : Locale_Type;
      Raise_Errors   : Boolean;
      Mark_Messages  : Boolean                         := True;
      Mark_Arguments : Boolean                         := True;
      Error_Handler  : access Error_Handler_Type'Class :=
        Standard_Error_Handler'Access)
      return Wide_String renames ZanyBlue.Text.Format_Message;
   --  Utility renaming to simplify calls.

   ------------------------
   -- Disable_Exceptions --
   ------------------------

   procedure Disable_Exceptions is
   begin
      Disable_Exceptions (Standard_Catalog);
   end Disable_Exceptions;

   ----------------------------
   -- Disable_Source_Locales --
   ----------------------------

   procedure Disable_Source_Locales is
   begin
      Disable_Source_Locales (Standard_Catalog);
   end Disable_Source_Locales;

   ---------------------
   -- Disable_Wide_IO --
   ---------------------

   procedure Disable_Wide_IO is
   begin
      Use_Wide_IO := False;
   end Disable_Wide_IO;

   -----------------------
   -- Enable_Exceptions --
   -----------------------

   procedure Enable_Exceptions is
   begin
      Enable_Exceptions (Standard_Catalog);
   end Enable_Exceptions;

   ---------------------------
   -- Enable_Source_Locales --
   ---------------------------

   procedure Enable_Source_Locales is
   begin
      Enable_Source_Locales (Standard_Catalog);
   end Enable_Source_Locales;

   --------------------
   -- Enable_Wide_IO --
   --------------------

   procedure Enable_Wide_IO is
   begin
      Use_Wide_IO := True;
   end Enable_Wide_IO;

   ------------------------
   -- Exceptions_Enabled --
   ------------------------

   function Exceptions_Enabled return Boolean is
   begin
      return Exceptions_Enabled (Standard_Catalog);
   end Exceptions_Enabled;

   ------------
   -- Format --
   ------------

   function Format
     (Facility  : Wide_String;
      Key       : Wide_String;
      Arguments : Argument_List;
      Locale    : Locale_Type  := Current_Locale;
      Catalog   : Catalog_Type := Standard_Catalog)
      return String
   is
   begin
      return
        Locale.Encode_To_String
          (Format
             (Facility, Key, Arguments, Locale => Locale, Catalog => Catalog));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Facility  : Wide_String;
      Key       : Wide_String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale;
      Catalog   : Catalog_Type        := Standard_Catalog)
      return String
   is
   begin
      return
        Locale.Encode_To_String
          (Format
             (Facility, Key, Argument0 => Argument0, Argument1 => Argument1,
              Argument2                => Argument2, Argument3 => Argument3,
              Argument4 => Argument4, Locale => Locale, Catalog => Catalog));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Text      : Wide_String;
      Arguments : Argument_List;
      Locale    : Locale_Type := Current_Locale)
      return String
   is
   begin
      return
        Locale.Encode_To_String (Format (Text, Arguments, Locale => Locale));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Text      : Wide_String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale)
      return String
   is
   begin
      return
        Locale.Encode_To_String
          (Format
             (Text, Argument0 => Argument0, Argument1 => Argument1,
              Argument2       => Argument2, Argument3 => Argument3,
              Argument4       => Argument4, Locale => Locale));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Facility  : Wide_String;
      Key       : Wide_String;
      Arguments : Argument_List;
      Locale    : Locale_Type  := Current_Locale;
      Catalog   : Catalog_Type := Standard_Catalog)
      return Wide_String
   is
      Effective_Locale : aliased Locale_Type;
      Text             : constant Wide_String :=
        Get_Text (Catalog, Facility, Key, Locale, Effective_Locale'Access);
   begin
      if Source_Locales_Enabled (Catalog) then
         Effective_Locale := Transfer_Locale_Data (Effective_Locale, Locale);
      else
         Effective_Locale := Locale;
      end if;
      return
        Format_Message
          (Text, Arguments, Get_Pseudo_Map (Catalog), Effective_Locale,
           Raise_Errors   => Exceptions_Enabled (Catalog),
           Mark_Messages  => Get_Mark_Messages (Catalog),
           Mark_Arguments => Get_Mark_Arguments (Catalog));
   exception
      when E : No_Such_Argument_Error =>
      --  Re-raise the No_Such_Argument_Error but include the facility and
      --  key that caused the error.
         raise No_Such_Argument_Error
           with Locale.Encode_To_String
             (Facility & ":" & Key & ":" &
              Wide_From_UTF8 (Exception_Message (E)));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Facility  : Wide_String;
      Key       : Wide_String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale;
      Catalog   : Catalog_Type        := Standard_Catalog)
      return Wide_String
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      return
        Format
          (Facility, Key, Arguments, Locale => Locale, Catalog => Catalog);
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Text      : Wide_String;
      Arguments : Argument_List;
      Locale    : Locale_Type := Current_Locale)
      return Wide_String
   is
   begin
      return Format_Message (Text, Arguments, null, Locale, True);
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Text      : Wide_String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale)
      return Wide_String
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      return Format (Text, Arguments, Locale => Locale);
   end Format;

   --------------------
   -- Make_Arguments --
   --------------------

   procedure Make_Arguments
     (Arguments : in out Argument_List;
      Argument0 :        Argument_Type'Class;
      Argument1 :        Argument_Type'Class;
      Argument2 :        Argument_Type'Class;
      Argument3 :        Argument_Type'Class;
      Argument4 :        Argument_Type'Class)
   is
   begin
      if Argument0 in Null_Argument_Type then
         return;
      end if;
      Append (Arguments, Argument0);
      if Argument1 in Null_Argument_Type then
         return;
      end if;
      Append (Arguments, Argument1);
      if Argument2 in Null_Argument_Type then
         return;
      end if;
      Append (Arguments, Argument2);
      if Argument3 in Null_Argument_Type then
         return;
      end if;
      Append (Arguments, Argument3);
      if Argument4 in Null_Argument_Type then
         return;
      end if;
      Append (Arguments, Argument4);
   end Make_Arguments;

   -----------
   -- Print --
   -----------

   procedure Print
     (Facility  : Wide_String;
      Key       : Wide_String;
      Arguments : Argument_List;
      Locale    : Locale_Type  := Current_Locale;
      Catalog   : Catalog_Type := Standard_Catalog)
   is
   begin
      if Use_Wide_IO then
         Print
           (Ada.Wide_Text_IO.Current_Output, Facility, Key, Arguments, Locale,
            Catalog);
      else
         Print
           (Ada.Text_IO.Current_Output, Facility, Key, Arguments, Locale,
            Catalog);
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Facility  : Wide_String;
      Key       : Wide_String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale;
      Catalog   : Catalog_Type        := Standard_Catalog)
   is
   begin
      if Use_Wide_IO then
         Print
           (Ada.Wide_Text_IO.Current_Output, Facility, Key, Argument0,
            Argument1, Argument2, Argument3, Argument4, Locale, Catalog);
      else
         Print
           (Ada.Text_IO.Current_Output, Facility, Key, Argument0, Argument1,
            Argument2, Argument3, Argument4, Locale, Catalog);
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Destination : Ada.Wide_Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Arguments   : Argument_List;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message
        (Destination, Facility, Key, Arguments, False, Locale, Catalog);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Destination : Ada.Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Arguments   : Argument_List;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message
        (Destination, Facility, Key, Arguments, False, Locale, Catalog);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Destination : Ada.Wide_Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale;
      Catalog     : Catalog_Type        := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      Print
        (Destination, Facility, Key, Arguments, Locale => Locale,
         Catalog                                       => Catalog);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Destination : Ada.Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale;
      Catalog     : Catalog_Type        := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      Print
        (Destination, Facility, Key, Arguments, Locale => Locale,
         Catalog                                       => Catalog);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Text      : Wide_String;
      Arguments : Argument_List;
      Locale    : Locale_Type := Current_Locale)
   is
   begin
      if Use_Wide_IO then
         Print (Ada.Wide_Text_IO.Current_Output, Text, Arguments, Locale);
      else
         Print (Ada.Text_IO.Current_Output, Text, Arguments, Locale);
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Text      : Wide_String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale)
   is
   begin
      if Use_Wide_IO then
         Print
           (Ada.Wide_Text_IO.Current_Output, Text, Argument0, Argument1,
            Argument2, Argument3, Argument4, Locale);
      else
         Print
           (Ada.Text_IO.Current_Output, Text, Argument0, Argument1, Argument2,
            Argument3, Argument4, Locale);
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Destination : Ada.Wide_Text_IO.File_Type;
      Text        : Wide_String;
      Arguments   : Argument_List;
      Locale      : Locale_Type := Current_Locale)
   is
   begin
      Print
        (Standard_Catalog, Destination, "", "", Locale, Arguments,
         Format_Message (Text, Arguments, null, Locale, True), False);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Destination : Ada.Text_IO.File_Type;
      Text        : Wide_String;
      Arguments   : Argument_List;
      Locale      : Locale_Type := Current_Locale)
   is
   begin
      Print
        (Standard_Catalog, Destination, "", "", Locale, Arguments,
         Format_Message (Text, Arguments, null, Locale, True), False);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Destination : Ada.Wide_Text_IO.File_Type;
      Text        : Wide_String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale)
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      Print (Destination, Text, Arguments, Locale);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Destination : Ada.Text_IO.File_Type;
      Text        : Wide_String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale)
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      Print (Destination, Text, Arguments, Locale);
   end Print;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Facility  : Wide_String;
      Key       : Wide_String;
      Arguments : Argument_List;
      Locale    : Locale_Type  := Current_Locale;
      Catalog   : Catalog_Type := Standard_Catalog)
   is
   begin
      if Use_Wide_IO then
         Print_Line
           (Ada.Wide_Text_IO.Current_Output, Facility, Key, Arguments, Locale,
            Catalog);
      else
         Print_Line
           (Ada.Text_IO.Current_Output, Facility, Key, Arguments, Locale,
            Catalog);
      end if;
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Facility  : Wide_String;
      Key       : Wide_String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale;
      Catalog   : Catalog_Type        := Standard_Catalog)
   is
   begin
      if Use_Wide_IO then
         Print_Line
           (Ada.Wide_Text_IO.Current_Output, Facility, Key, Argument0,
            Argument1, Argument2, Argument3, Argument4, Locale, Catalog);
      else
         Print_Line
           (Ada.Text_IO.Current_Output, Facility, Key, Argument0, Argument1,
            Argument2, Argument3, Argument4, Locale, Catalog);
      end if;
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Destination : Ada.Wide_Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Arguments   : Argument_List;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message
        (Destination, Facility, Key, Arguments, True, Locale, Catalog);
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Destination : Ada.Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Arguments   : Argument_List;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message
        (Destination, Facility, Key, Arguments, True, Locale, Catalog);
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Destination : Ada.Wide_Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale;
      Catalog     : Catalog_Type        := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      Write_Message
        (Destination, Facility, Key, Arguments, True, Locale, Catalog);
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Destination : Ada.Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale;
      Catalog     : Catalog_Type        := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      Write_Message
        (Destination, Facility, Key, Arguments, True, Locale, Catalog);
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Text      : Wide_String;
      Arguments : Argument_List;
      Locale    : Locale_Type := Current_Locale)
   is
   begin
      if Use_Wide_IO then
         Print_Line (Ada.Wide_Text_IO.Current_Output, Text, Arguments, Locale);
      else
         Print_Line (Ada.Text_IO.Current_Output, Text, Arguments, Locale);
      end if;
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Text      : Wide_String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale)
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      if Use_Wide_IO then
         Print_Line (Ada.Wide_Text_IO.Current_Output, Text, Arguments, Locale);
      else
         Print_Line (Ada.Text_IO.Current_Output, Text, Arguments, Locale);
      end if;
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Destination : Ada.Wide_Text_IO.File_Type;
      Text        : Wide_String;
      Arguments   : Argument_List;
      Locale      : Locale_Type := Current_Locale)
   is
   begin
      Print
        (Standard_Catalog, Destination, "", "", Locale, Arguments,
         Format_Message (Text, Arguments, null, Locale, True), True);
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Destination : Ada.Text_IO.File_Type;
      Text        : Wide_String;
      Arguments   : Argument_List;
      Locale      : Locale_Type := Current_Locale)
   is
   begin
      Print
        (Standard_Catalog, Destination, "", "", Locale, Arguments,
         Format_Message (Text, Arguments, null, Locale, True), True);
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Destination : Ada.Wide_Text_IO.File_Type;
      Text        : Wide_String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale)
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      Print_Line (Destination, Text, Arguments, Locale);
   end Print_Line;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line
     (Destination : Ada.Text_IO.File_Type;
      Text        : Wide_String;
      Argument0   : Argument_Type'Class := Null_Argument;
      Argument1   : Argument_Type'Class := Null_Argument;
      Argument2   : Argument_Type'Class := Null_Argument;
      Argument3   : Argument_Type'Class := Null_Argument;
      Argument4   : Argument_Type'Class := Null_Argument;
      Locale      : Locale_Type         := Current_Locale)
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      Print_Line (Destination, Text, Arguments, Locale);
   end Print_Line;

   ----------------------
   -- Pseudo_Translate --
   ----------------------

   procedure Pseudo_Translate
     (Mapping : Pseudo_Map_Vector;
      Catalog : Catalog_Type := Standard_Catalog)
   is
   begin
      Enable_Pseudo_Translations (Catalog, Mapping);
   end Pseudo_Translate;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E       : Ada.Exceptions.Exception_Id;
      Message : Wide_String)
   is
   begin
      Raise_Exception
        (E, Message => Current_Locale.Encode_To_String (Message));
   end Raise_Exception;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E         : Ada.Exceptions.Exception_Id;
      Facility  : Wide_String;
      Key       : Wide_String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale;
      Catalog   : Catalog_Type        := Standard_Catalog)
   is
      Message : constant String :=
        Format
          (Facility, Key, Argument0 => Argument0, Argument1 => Argument1,
           Argument2                => Argument2, Argument3 => Argument3,
           Argument4 => Argument4, Locale => Locale, Catalog => Catalog);
   begin
      Raise_Exception (E, Message => Message);
   end Raise_Exception;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E         : Ada.Exceptions.Exception_Id;
      Facility  : Wide_String;
      Key       : Wide_String;
      Arguments : Argument_List;
      Locale    : Locale_Type  := Current_Locale;
      Catalog   : Catalog_Type := Standard_Catalog)
   is
      Message : constant String :=
        Format
          (Facility, Key, Arguments, Locale => Locale, Catalog => Catalog);
   begin
      Raise_Exception (E, Message => Message);
   end Raise_Exception;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E         : Ada.Exceptions.Exception_Id;
      Text      : Wide_String;
      Argument0 : Argument_Type'Class;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument;
      Locale    : Locale_Type         := Current_Locale)
   is
      Arguments : Argument_List;
   begin
      Make_Arguments
        (Arguments, Argument0, Argument1, Argument2, Argument3, Argument4);
      Raise_Exception
        (E, Message => String'(Format (Text, Arguments, Locale => Locale)));
   end Raise_Exception;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Filter  : Message_Filter_Access;
      Catalog : Catalog_Type := Standard_Catalog)
   is
   begin
      Set_Filter (Catalog, Filter);
   end Set_Filter;

   ----------------------------
   -- Source_Locales_Enabled --
   ----------------------------

   function Source_Locales_Enabled return Boolean is
   begin
      return Source_Locales_Enabled (Standard_Catalog);
   end Source_Locales_Enabled;

   ----------------------
   -- Standard_Catalog --
   ----------------------

   function Standard_Catalog return Catalog_Type is
   begin
      return My_Catalog;
   end Standard_Catalog;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message
     (Destination : Ada.Wide_Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Arguments   : Argument_List;
      With_NL     : Boolean;
      Locale      : Locale_Type;
      Catalog     : Catalog_Type)
   is
   begin
      if not Is_Filtered (Catalog, Facility, Key) then
         Print
           (Catalog, Destination, Facility, Key, Locale, Arguments,
            Format (Facility, Key, Arguments, Locale, Catalog), With_NL);
      end if;
   end Write_Message;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message
     (Destination : Ada.Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Arguments   : Argument_List;
      With_NL     : Boolean;
      Locale      : Locale_Type;
      Catalog     : Catalog_Type)
   is
   begin
      if not Is_Filtered (Catalog, Facility, Key) then
         Print
           (Catalog, Destination, Facility, Key, Locale, Arguments,
            Format (Facility, Key, Arguments, Locale, Catalog), With_NL);
      end if;
   end Write_Message;

end ZanyBlue.Text.Formatting;
