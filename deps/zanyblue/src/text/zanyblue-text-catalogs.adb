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

with Ada.Containers.Indefinite_Vectors;
with ZanyBlue.OS;
with ZanyBlue.Text.Message_Maps;
with ZanyBlue.Text.Indexed_Strings;

package body ZanyBlue.Text.Catalogs is

   use Ada.Containers;
   use ZanyBlue.OS;
   use ZanyBlue.Text.Message_Maps;
   use ZanyBlue.Text.Indexed_Strings;

   type Catalog_Value is record
      Facilities     : Indexed_Strings_Type;
      Keys           : Indexed_Strings_Type;
      Locales        : Indexed_Strings_Type;
      Messages       : Message_Map_Type;
      Logical_Size   : Natural               := 0;
      Pseudo_Map     : Pseudo_Map_Access;
      Mark_Messages  : Boolean               := True;
      Mark_Arguments : Boolean               := True;
      Single_Pool    : Boolean               := False;
      Raise_Errors   : Boolean               := True;
      Source_Locales : Boolean               := True;
      Printer        : Printer_Access        := Standard_Printer;
      Filter         : Message_Filter_Access := null;
   end record;

   function Map_To_Triple
     (Catalog  : Catalog_Type;
      Facility : Wide_String;
      Key      : Wide_String;
      Locale   : Locale_Type;
      Create   : Boolean)
      return Message_Triple;

   ---------
   -- Add --
   ---------

   procedure Add
     (Catalog  : Catalog_Type;
      Facility : Wide_String;
      Key      : Wide_String;
      Message  : Wide_String;
      Locale   : Locale_Type)
   is
   begin
      Add (Catalog, Facility, Key, Message, Locale, Locale);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Catalog       : Catalog_Type;
      Facility      : Wide_String;
      Key           : Wide_String;
      Message       : Wide_String;
      Locale        : Locale_Type;
      Source_Locale : Locale_Type)
   is

      Triple : constant Message_Triple :=
        Map_To_Triple (Catalog, Facility, Key, Locale, True);

   begin
      Add_Locale (Catalog, Source_Locale);
      Catalog.C.Logical_Size := Catalog.C.Logical_Size + Message'Length;
      Catalog.C.Messages.Add
        (Triple, Message, Get_Locale_Index (Catalog, Source_Locale));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Catalog  : Catalog_Type;
      Facility : Wide_String;
      Key      : Wide_String;
      Pool     : Static_Message_Pool_Type;
      First    : Positive;
      Last     : Natural;
      Locale   : Locale_Type)
   is
   begin
      Add (Catalog, Facility, Key, Pool, First, Last, Locale, Locale);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Catalog       : Catalog_Type;
      Facility      : Wide_String;
      Key           : Wide_String;
      Pool          : Static_Message_Pool_Type;
      First         : Positive;
      Last          : Natural;
      Locale        : Locale_Type;
      Source_Locale : Locale_Type)
   is

      New_Message : Message_Definition;
      Triple      : Message_Triple;

   begin
      Add_Locale (Catalog, Source_Locale);
      --  Validate the message is actually within the argument pool
      if First < Pool.all'First or else Last > Pool.all'Last then
         raise Invalid_Static_Message_Error;
      end if;
      if Catalog.C.Single_Pool then
         --  Application requested all message be stored in a single
         --  pool: simply add static message to the dynamic pool area.
         --  This is normally used by "zbmcompile" when generating code.
         Add
           (Catalog, Facility, Key, Pool.all (First .. Last), Locale,
            Source_Locale);
         return;
      end if;
      Triple := Map_To_Triple (Catalog, Facility, Key, Locale, True);
      New_Message.Pool         := Pool;
      New_Message.First        := First;
      New_Message.Last         := Last;
      New_Message.Locale_Index := Get_Locale_Index (Catalog, Source_Locale);
      Catalog.C.Messages.Add (Triple, New_Message);
   end Add;

   ------------------
   -- Add_Facility --
   ------------------

   procedure Add_Facility
     (Catalog  : Catalog_Type;
      Facility : Wide_String)
   is
      Index : Facility_Index_Type;             --  Throwaway value
      pragma Warnings (Off, Index);
   begin
      Add_Facility (Catalog, Facility, Index);
   end Add_Facility;

   ------------------
   -- Add_Facility --
   ------------------

   procedure Add_Facility
     (Catalog  :     Catalog_Type;
      Facility :     Wide_String;
      Index    : out Facility_Index_Type)
   is
   begin
      Catalog.C.Facilities.Add (Facility, Index);
   end Add_Facility;

   -------------
   -- Add_Key --
   -------------

   procedure Add_Key
     (Catalog :     Catalog_Type;
      Key     :     Wide_String;
      Index   : out Key_Index_Type)
   is
   begin
      Catalog.C.Keys.Add (Key, Index);
   end Add_Key;

   -------------------
   -- Add_Key_Value --
   -------------------

   overriding procedure Add_Key_Value
     (Handler       : in out Catalog_Handler_Type;
      Facility      :        Wide_String;
      Key           :        Wide_String;
      Value         :        Wide_String;
      Locale        :        Locale_Type;
      Source_Locale :        Locale_Type;
      File_Name     :        Wide_String;
      Line          :        Natural)
   is
      pragma Unreferenced (File_Name);
      pragma Unreferenced (Line);
   begin
      Add (Handler.Catalog, Facility, Key, Value, Locale, Source_Locale);
   end Add_Key_Value;

   ----------------
   -- Add_Locale --
   ----------------

   procedure Add_Locale
     (Catalog : Catalog_Type;
      Locale  : Locale_Type)
   is
      Index : Locale_Index_Type;             --  Throwaway value
      pragma Warnings (Off, Index);
   begin
      Add_Locale (Catalog, Locale, Index);
   end Add_Locale;

   ----------------
   -- Add_Locale --
   ----------------

   procedure Add_Locale
     (Catalog :     Catalog_Type;
      Locale  :     Locale_Type;
      Index   : out Locale_Index_Type)
   is
   begin
      Catalog.C.Locales.Add (Locale_Name (Locale), Index);
   end Add_Locale;

   ------------
   -- Create --
   ------------

   function Create return Catalog_Type is
      Result : constant Catalog_Type := (C => new Catalog_Value);
   begin
      return Result;
   end Create;

   ------------------------
   -- Disable_Exceptions --
   ------------------------

   procedure Disable_Exceptions (Catalog : Catalog_Type) is
   begin
      Catalog.C.Raise_Errors := False;
   end Disable_Exceptions;

   ----------------------------
   -- Disable_Source_Locales --
   ----------------------------

   procedure Disable_Source_Locales (Catalog : Catalog_Type) is
   begin
      Catalog.C.Source_Locales := False;
   end Disable_Source_Locales;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Catalog   : Catalog_Type;
      File_Name : Wide_String := "") is separate;

   -------------------
   -- Duplicate_Key --
   -------------------

   overriding procedure Duplicate_Key
     (Handler       : in out Catalog_Handler_Type;
      Facility      :        Wide_String;
      Key           :        Wide_String;
      Locale        :        Locale_Type;
      File_Name     :        Wide_String;
      Current_Line  :        Natural;
      Previous_Line :        Natural)
   is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
      pragma Unreferenced (Locale);
   begin
      raise Duplicate_Key_Error
        with Wide_To_UTF8 (File_Name & ":" & Key) & ":" &
        Natural'Image (Current_Line) & ":" & Natural'Image (Previous_Line);
   end Duplicate_Key;

   -----------------------
   -- Enable_Exceptions --
   -----------------------

   procedure Enable_Exceptions (Catalog : Catalog_Type) is
   begin
      Catalog.C.Raise_Errors := True;
   end Enable_Exceptions;

   --------------------------------
   -- Enable_Pseudo_Translations --
   --------------------------------

   procedure Enable_Pseudo_Translations
     (Catalog        : Catalog_Type;
      Mapping        : Pseudo_Map_Vector;
      Mark_Messages  : Boolean := True;
      Mark_Arguments : Boolean := True)
   is
   begin
      Catalog.C.Pseudo_Map := new Pseudo_Map_Type;
      Catalog.C.Pseudo_Map.Add_Mapping (Mapping);
      Catalog.C.Mark_Messages  := Mark_Messages;
      Catalog.C.Mark_Arguments := Mark_Arguments;
   end Enable_Pseudo_Translations;

   ---------------------------
   -- Enable_Source_Locales --
   ---------------------------

   procedure Enable_Source_Locales (Catalog : Catalog_Type) is
   begin
      Catalog.C.Source_Locales := True;
   end Enable_Source_Locales;

   ------------------------
   -- Exceptions_Enabled --
   ------------------------

   function Exceptions_Enabled
     (Catalog : Catalog_Type)
      return Boolean
   is
   begin
      return Catalog.C.Raise_Errors;
   end Exceptions_Enabled;

   -----------------
   -- Get_Catalog --
   -----------------

   function Get_Catalog
     (Handler : Catalog_Handler_Type)
      return Catalog_Type
   is
   begin
      return Handler.Catalog;
   end Get_Catalog;

   ------------------
   -- Get_Facility --
   ------------------

   function Get_Facility
     (Catalog : Catalog_Type;
      Index   : Facility_Index_Type)
      return Wide_String
   is
   begin
      return Catalog.C.Facilities.Get (Positive (Index));
   exception
      when No_Such_Item =>
         raise No_Such_Facility_Error with Facility_Index_Type'Image (Index);
   end Get_Facility;

   ------------------------
   -- Get_Facility_Index --
   ------------------------

   function Get_Facility_Index
     (Catalog : Catalog_Type;
      Name    : Wide_String)
      return Facility_Index_Type
   is
      Index : Positive;
   begin
      Index :=
        Catalog.C.Facilities.Get (Name, No_Such_Facility_Error'Identity);
      return Facility_Index_Type (Index);
   end Get_Facility_Index;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Catalog : Catalog_Type;
      Index   : Key_Index_Type)
      return Wide_String
   is
   begin
      return Catalog.C.Keys.Get (Positive (Index));
   exception
      when No_Such_Item =>
         raise No_Such_Key_Error with Key_Index_Type'Image (Index);
   end Get_Key;

   -------------------
   -- Get_Key_Index --
   -------------------

   function Get_Key_Index
     (Catalog : Catalog_Type;
      Name    : Wide_String)
      return Key_Index_Type
   is
      Index : Positive;
   begin
      Index := Catalog.C.Keys.Get (Name, No_Such_Key_Error'Identity);
      return Key_Index_Type (Index);
   end Get_Key_Index;

   ----------------
   -- Get_Locale --
   ----------------

   function Get_Locale
     (Catalog : Catalog_Type;
      Index   : Locale_Index_Type)
      return Locale_Type
   is
   begin
      return Make_Locale (Catalog.C.Locales.Get (Positive (Index)));
   exception
      when No_Such_Item =>
         raise No_Such_Locale_Error with Locale_Index_Type'Image (Index);
   end Get_Locale;

   ----------------------
   -- Get_Locale_Index --
   ----------------------

   function Get_Locale_Index
     (Catalog : Catalog_Type;
      Name    : Wide_String)
      return Locale_Index_Type
   is
      Index : Positive;
   begin
      Index := Catalog.C.Locales.Get (Name, No_Such_Locale_Error'Identity);
      return Locale_Index_Type (Index);
   end Get_Locale_Index;

   ----------------------
   -- Get_Locale_Index --
   ----------------------

   function Get_Locale_Index
     (Catalog : Catalog_Type;
      Locale  : Locale_Type)
      return Locale_Index_Type
   is
   begin
      return Get_Locale_Index (Catalog, Locale_Name (Locale));
   end Get_Locale_Index;

   ---------------------
   -- Get_Locale_Name --
   ---------------------

   function Get_Locale_Name
     (Catalog : Catalog_Type;
      Index   : Locale_Index_Type)
      return Wide_String
   is
   begin
      return Locale_Name (Get_Locale (Catalog, Index));
   end Get_Locale_Name;

   ------------------------
   -- Get_Mark_Arguments --
   ------------------------

   function Get_Mark_Arguments
     (Catalog : Catalog_Type)
      return Boolean
   is
   begin
      return Catalog.C.Mark_Arguments;
   end Get_Mark_Arguments;

   -----------------------
   -- Get_Mark_Messages --
   -----------------------

   function Get_Mark_Messages
     (Catalog : Catalog_Type)
      return Boolean
   is
   begin
      return Catalog.C.Mark_Messages;
   end Get_Mark_Messages;

   --------------
   -- Get_Pool --
   --------------

   function Get_Pool
     (Catalog : Catalog_Type)
      return Wide_String
   is
   begin
      if not Catalog.C.Single_Pool then
         raise Multiple_Pools_Error;
      end if;
      return Catalog.C.Messages.Get_Pool;
   end Get_Pool;

   --------------------
   -- Get_Pseudo_Map --
   --------------------

   function Get_Pseudo_Map
     (Catalog : Catalog_Type)
      return Pseudo_Map_Access
   is
   begin
      return Catalog.C.Pseudo_Map;
   end Get_Pseudo_Map;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Catalog        : Catalog_Type;
      Facility       : Wide_String;
      Key            : Wide_String;
      Locale         : Locale_Type;
      Message_Locale : access Locale_Type := null)
      return Wide_String
   is

      Message        : Message_Definition;
      Language       : Language_Type;
      Script         : Script_Type;
      Territory      : Territory_Type;
      Base_Territory : Territory_Type;
      Triple         : Message_Triple;

   begin
      Get_Locale_Codes (Locale, Language, Script, Territory);
      Base_Territory := Territory;
      Locale_Loop :
      for I in 1 .. Maximum_Locale_Parents loop
         Map_Locale_Triple :
         begin
            Triple :=
              Map_To_Triple
                (Catalog, Facility, Key,
                 Make_Locale (Language, Script, Territory), False);
            Catalog.C.Messages.Get (Triple, Message);
            if Message_Locale /= null then
               Message_Locale.all :=
                 Get_Locale (Catalog, Message.Locale_Index);
            end if;
            return Catalog.C.Messages.Text (Message);
         exception
            when No_Such_Locale_Error | Constraint_Error =>
               exit Locale_Loop when Language = Empty_Language;
               Parent_Codes
                 (Language, Script, Territory,
                  Base_Territory => Base_Territory);
         end Map_Locale_Triple;
      end loop Locale_Loop;
      raise No_Such_Message_Error with Wide_To_UTF8 (Facility & "/" & Key);
   end Get_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Catalog        : Catalog_Type;
      Facility_Index : Facility_Index_Type;
      Key_Index      : Key_Index_Type;
      Locale_Index   : Locale_Index_Type)
      return Wide_String
   is

      Message : Message_Definition;
      Triple  : constant Message_Triple :=
        (Facility_Index => Facility_Index, Key_Index => Key_Index,
         Locale_Index   => Locale_Index);

   begin
      Catalog.C.Messages.Get (Triple, Message);
      return Catalog.C.Messages.Text (Message);
   exception
      when Constraint_Error =>
         raise No_Such_Message_Error
           with Facility_Index_Type'Image (Facility_Index) & "/" &
           Key_Index_Type'Image (Key_Index);
   end Get_Text;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Catalog         : Catalog_Type;
      Messages        : ZBMCompile_List;
      Pool            : Static_Message_Pool_Type;
      Facilities      : Constant_String_List;
      Keys            : Constant_String_List;
      Locales         : Constant_String_List;
      Package_Name    : Wide_String := "";
      Pool_Length     : Natural     := 0;
      Expected_Length : Natural     := 0)
   is
      F, K, L, EL : Positive;
   begin
      if Pool_Length /= Expected_Length then
         raise Pool_Size_Mismatch_Error
           with Wide_To_UTF8 (Package_Name) & Natural'Image (Pool_Length) &
           " /=" & Natural'Image (Expected_Length);
      end if;
      Reserve (Catalog, Messages => Messages'Length);
      for I in Messages'Range loop
         F  := Positive (Messages (I).Facility_Index);
         K  := Positive (Messages (I).Key_Index);
         L  := Positive (Messages (I).Locale_Index);
         EL := Positive (Messages (I).Source_Locale_Index);
         Add
           (Catalog, Facilities (F).all, Keys (K).all, Pool,
            Messages (I).First, Messages (I).Last,
            Make_Locale (Locales (L).all), Make_Locale (Locales (EL).all));
      end loop;
   end Initialize;

   -----------------------
   -- Invalid_Character --
   -----------------------

   overriding procedure Invalid_Character
     (Handler      : in out Catalog_Handler_Type;
      Facility     :        Wide_String;
      File_Name    :        Wide_String;
      Current_Line :        Natural;
      Ch           :        Character)
   is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
   begin
      raise Unicode_Escape_Error
        with Wide_To_UTF8 (File_Name) & ":" & Natural'Image (Current_Line) &
        ":" & "Invalid character: " & Ch;
   end Invalid_Character;

   ------------------------
   -- Invalid_Definition --
   ------------------------

   overriding procedure Invalid_Definition
     (Handler         : in out Catalog_Handler_Type;
      Facility        :        Wide_String;
      Locale          :        Locale_Type;
      File_Name       :        Wide_String;
      Current_Line    :        Natural;
      Additional_Info :        String)
   is
      pragma Unreferenced (Handler);
      pragma Unreferenced (Facility);
      pragma Unreferenced (Locale);
   begin
      raise Unicode_Escape_Error
        with Wide_To_UTF8 (File_Name) & ":" & Natural'Image (Current_Line) &
        ":" & Additional_Info;
   end Invalid_Definition;

   -----------------
   -- Is_Filtered --
   -----------------

   function Is_Filtered
     (Catalog  : Catalog_Type;
      Facility : Wide_String;
      Key      : Wide_String)
      return Boolean
   is
   begin
      return
        Catalog.C.Filter /= null
        and then Catalog.C.Filter.Is_Filtered (Facility, Key);
   end Is_Filtered;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (Catalog : Catalog_Type)
      return Boolean
   is
   begin
      return Catalog.C /= null;
   end Is_Valid;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Catalog : Catalog_Type;
      Handler : not null access procedure
        (Facility      : Facility_Index_Type;
         Key           : Key_Index_Type;
         Locale        : Locale_Index_Type;
         Source_Locale : Locale_Index_Type;
         First         : Positive;
         Last          : Natural;
         Count         : Natural))
   is
   begin
      Catalog.C.Messages.Iterate (Handler);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Catalog : Catalog_Type;
      Handler : not null access procedure
        (Facility      : Facility_Index_Type;
         Key           : Key_Index_Type;
         Locale        : Locale_Index_Type;
         Source_Locale : Locale_Index_Type;
         Message       : Wide_String;
         Count         : Natural))
   is
   begin
      Catalog.C.Messages.Iterate (Handler);
   end Iterate;

   -------------------
   -- Load_Facility --
   -------------------

   procedure Load_Facility
     (Facility           :        Wide_String;
      Source_Name        :        Wide_String;
      N_Locales          :    out Natural;
      N_Messages         :    out Natural;
      Handler            : in out Catalog_Handler_Type'Class;
      Directory          :        Wide_String := ".";
      Extension          :        Wide_String := Default_Extension;
      Base_Locale_Only   :        Boolean     := False;
      Locale_Prefix      :        Wide_String := "";
      Source_Root_Locale :        Locale_Type := Root_Locale) is separate;

   -------------------
   -- Load_Facility --
   -------------------

   procedure Load_Facility
     (Catalog            :     Catalog_Type;
      Facility           :     Wide_String;
      Source_Name        :     Wide_String;
      N_Locales          : out Natural;
      N_Messages         : out Natural;
      Directory          :     Wide_String := ".";
      Extension          :     Wide_String := Default_Extension;
      Base_Locale_Only   :     Boolean     := False;
      Locale_Prefix      :     Wide_String := "";
      Source_Root_Locale :     Locale_Type := Root_Locale)
   is

      Handler : Catalog_Handler_Type;

   begin
      Handler.Catalog := Catalog;
      Load_Facility
        (Facility, Source_Name, N_Locales, N_Messages, Handler,
         Directory          => Directory, Extension => Extension,
         Base_Locale_Only => Base_Locale_Only, Locale_Prefix => Locale_Prefix,
         Source_Root_Locale => Source_Root_Locale);
   end Load_Facility;

   -------------------
   -- Load_Facility --
   -------------------

   procedure Load_Facility
     (Catalog            :     Catalog_Type;
      Facility           :     Wide_String;
      N_Locales          : out Natural;
      N_Messages         : out Natural;
      Directory          :     Wide_String := ".";
      Extension          :     Wide_String := Default_Extension;
      Source_Root_Locale :     Locale_Type := Root_Locale)
   is
   begin
      Load_Facility
        (Catalog, Facility, Facility, N_Locales, N_Messages,
         Directory          => Directory, Extension => Extension,
         Source_Root_Locale => Source_Root_Locale);
   end Load_Facility;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
     (Catalog       : Catalog_Type;
      File_Name     : Wide_String;
      Facility      : Wide_String;
      Locale        : Locale_Type;
      Source_Locale : Locale_Type := Root_Locale)
      return Natural
   is

      Handler : Catalog_Handler_Type;

   begin
      Add_Locale (Catalog, Source_Locale);
      Handler.Set_Catalog (Catalog);
      Parse (Handler, File_Name, Facility, Locale, Source_Locale);
      return Handler.Get_N_Messages;
   end Load_File;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (File_Name     :        Wide_String;
      Facility      :        Wide_String;
      Locale        :        Locale_Type;
      Handler       : in out Catalog_Handler_Type'Class;
      Source_Locale :        Locale_Type := Root_Locale)
   is
   begin
      Parse (Handler, File_Name, Facility, Locale, Source_Locale);
   end Load_File;

   -----------------------
   -- Logical_Pool_Size --
   -----------------------

   function Logical_Pool_Size
     (Catalog : Catalog_Type)
      return Natural
   is
   begin
      return Catalog.C.Logical_Size;
   end Logical_Pool_Size;

   -------------------
   -- Map_To_Triple --
   -------------------

   function Map_To_Triple
     (Catalog  : Catalog_Type;
      Facility : Wide_String;
      Key      : Wide_String;
      Locale   : Locale_Type;
      Create   : Boolean)
      return Message_Triple
   is
      Result : Message_Triple;
   begin
      if Create then
         Add_Facility (Catalog, Facility, Result.Facility_Index);
         Add_Key (Catalog, Key, Result.Key_Index);
         Add_Locale (Catalog, Locale, Result.Locale_Index);
      else
         Result.Facility_Index := Get_Facility_Index (Catalog, Facility);
         Result.Key_Index      := Get_Key_Index (Catalog, Key);
         Result.Locale_Index   := Get_Locale_Index (Catalog, Locale);
      end if;
      return Result;
   end Map_To_Triple;

   --------------------------
   -- Number_Of_Facilities --
   --------------------------

   function Number_Of_Facilities
     (Catalog : Catalog_Type)
      return Natural
   is
   begin
      return Catalog.C.Facilities.Length;
   end Number_Of_Facilities;

   --------------------
   -- Number_Of_Keys --
   --------------------

   function Number_Of_Keys
     (Catalog : Catalog_Type)
      return Natural
   is
   begin
      return Catalog.C.Keys.Length;
   end Number_Of_Keys;

   -----------------------
   -- Number_Of_Locales --
   -----------------------

   function Number_Of_Locales
     (Catalog : Catalog_Type)
      return Natural
   is
   begin
      return Catalog.C.Locales.Length;
   end Number_Of_Locales;

   ------------------------
   -- Number_Of_Messages --
   ------------------------

   function Number_Of_Messages
     (Catalog : Catalog_Type)
      return Natural
   is
   begin
      return Catalog.C.Messages.Length;
   end Number_Of_Messages;

   ---------------
   -- Pool_Size --
   ---------------

   function Pool_Size
     (Catalog : Catalog_Type)
      return Natural
   is
   begin
      return Catalog.C.Messages.Pool_Size;
   end Pool_Size;

   -----------
   -- Print --
   -----------

   procedure Print
     (Catalog     : Catalog_Type;
      Destination : Ada.Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Locale      : Locale_Type;
      Arguments   : ZanyBlue.Text.Arguments.Argument_List;
      Message     : Wide_String;
      With_NL     : Boolean)
   is
   begin
      Catalog.C.Printer.Print
        (Destination, Facility, Key, Locale, Arguments, Message, With_NL);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Catalog     : Catalog_Type;
      Destination : Ada.Wide_Text_IO.File_Type;
      Facility    : Wide_String;
      Key         : Wide_String;
      Locale      : Locale_Type;
      Arguments   : ZanyBlue.Text.Arguments.Argument_List;
      Message     : Wide_String;
      With_NL     : Boolean)
   is
   begin
      Catalog.C.Printer.Print
        (Destination, Facility, Key, Locale, Arguments, Message, With_NL);
   end Print;

   -------------------
   -- Query_Message --
   -------------------

   procedure Query_Message
     (Catalog        :     Catalog_Type;
      Facility_Index :     Facility_Index_Type;
      Key_Index      :     Key_Index_Type;
      Locale_Index   :     Locale_Index_Type;
      First          : out Positive;
      Last           : out Natural)
   is

      Triple : constant Message_Triple :=
        (Facility_Index => Facility_Index, Key_Index => Key_Index,
         Locale_Index   => Locale_Index);
      Message : Message_Definition;

   begin
      if not Catalog.C.Single_Pool then
         raise Multiple_Pools_Error;
      end if;
      Catalog.C.Messages.Get (Triple, Message);
      First := Message.First;
      Last  := Message.Last;
   exception
      when Constraint_Error =>
         raise No_Such_Message_Error
           with Facility_Index_Type'Image (Facility_Index) & "/" &
           Key_Index_Type'Image (Key_Index);
   end Query_Message;

   -------------
   -- Reserve --
   -------------

   procedure Reserve
     (Catalog   : Catalog_Type;
      Pool_Size : Natural := 0;
      Messages  : Natural := 0)
   is
      pragma Unreferenced (Pool_Size);
   begin
      --  There doesn't appear to be an API to reserve space for an
      --  Unbounded String?  Skipping the Pool_Size adjustment.
      if Messages > 0 then
         Catalog.C.Messages.Adjust_Size (Messages);
      end if;
   end Reserve;

   -----------------
   -- Set_Catalog --
   -----------------

   procedure Set_Catalog
     (Handler : in out Catalog_Handler_Type;
      Catalog :        Catalog_Type)
   is
   begin
      Handler.Catalog := Catalog;
   end Set_Catalog;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Catalog : Catalog_Type;
      Filter  : Message_Filter_Access)
   is
   begin
      Catalog.C.Filter := Filter;
   end Set_Filter;

   -----------------
   -- Set_Printer --
   -----------------

   procedure Set_Printer
     (Catalog : Catalog_Type;
      Printer : Printer_Access)
   is
   begin
      Catalog.C.Printer := Printer;
   end Set_Printer;

   ----------------------------
   -- Source_Locales_Enabled --
   ----------------------------

   function Source_Locales_Enabled
     (Catalog : Catalog_Type)
      return Boolean
   is
   begin
      return Catalog.C.Source_Locales;
   end Source_Locales_Enabled;

   ---------------------
   -- Use_Single_Pool --
   ---------------------

   procedure Use_Single_Pool (Catalog : Catalog_Type) is
   begin
      Catalog.C.Single_Pool := True;
   end Use_Single_Pool;

end ZanyBlue.Text.Catalogs;
