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

with Ada.Environment_Variables;
with ZanyBlue.OS;
with ZanyBlue.Text.Utils;
pragma Elaborate_All (ZanyBlue.OS);
pragma Elaborate_All (ZanyBlue.Text.Utils);

package body ZanyBlue.Text.Locales is

   use ZanyBlue.Text.Utils;

   Max_Tag_Length : constant :=
     Max_Language_Length + Max_Script_Length + Max_Territory_Length;
   --  Maximum length of the internal locale name, it's simply a concatenation
   --  of the language, script and territory names, uppercased.

   subtype Tag_Type is String;
   --  Internal locale identification: Lang + Script + Terr padded with
   --  spaces, e.g., "EN LATNUS " for "en_Latn_US".

   type String_Index_Type is new Positive range 1 .. 12_000;
   --  Strings are accessed via a simple table giving start and ending indexes
   --  of the string within the "global" string pool.  Each string is stored
   --  within locale structures as a index into this table.  Loading all
   --  available CLDR locales generates a pool with ~6500 strings.  This
   --  range should be sufficient for definitions for quite some time.

   type String_Address_Type is record
      First : Positive;
      Last  : Natural;
   end record;
   --  Start and end of a string with the pool

   type String_Addresses_Type is
     array (String_Index_Type range <>) of String_Address_Type;
   --  The collection of strings defined for all locales accessed by index.

   type Month_Names_Type is array (Month_Type) of String_Index_Type;
   --  List of strings (by index) for month names in a locale.

   type Day_Names_Type is array (Day_Type) of String_Index_Type;
   --  List of strings (by index) for day names in a locale.

   type Day_Period_Names_Type is array (Day_Period_Type) of String_Index_Type;
   --  List of strings (by index) for day period names in a locale.

   type Day_Period_Map_Type is array (Hour_Type) of Day_Period_Type;
   --  Mapping from hour number to day period name.

   type Era_Names_Type is array (Era_Type) of String_Index_Type;
   --  List of strings (by index) for era names in a locale.

   type Date_Time_Styles_Type is
     array (Date_Time_Style_Type) of String_Index_Type;
   --  List of strings (by index) for various time/date formats in a locale.

   type Numeric_Items_Type is array (Numeric_Item_Type) of String_Index_Type;
   --  List of strings (by index) for various numeric items in a locale.

   type Numeric_Format_Type is array (Numeric_Style_Type) of String_Index_Type;
   --  List of strings (by index) for various numeric formats in a locale.

   type Locale_Traits_Type is record
      Tag                : Tag_Type;
      Level              : Level_Type;
      Name               : String_Index_Type;
      Text_Layout        : Text_Layout_Type;
      Short_Month_Names  : Month_Names_Type;
      Full_Month_Names   : Month_Names_Type;
      Short_Day_Names    : Day_Names_Type;
      Full_Day_Names     : Day_Names_Type;
      Day_Period_Names   : Day_Period_Names_Type;
      Exact_Day_Periods  : Day_Period_Map_Type;
      Within_Day_Periods : Day_Period_Map_Type;
      Era_Names          : Era_Names_Type;
      Date_Formats       : Date_Time_Styles_Type;
      Time_Formats       : Date_Time_Styles_Type;
      Date_Time_Formats  : Date_Time_Styles_Type;
      Numeric_Items      : Numeric_Items_Type;
      Numeric_Formats    : Numeric_Format_Type;
   end record;
   --  Collection of strings used for each locale.

   type Trait_Array_Type is
     array (Trait_Index_Type range <>) of Locale_Traits_Type;
   --  List of predefined traits

   Current_Locale_Value : Locale_Type;
   --  Current locale initialized by the environment (variable or underlying
   --  OS defintion).

   function Locale_Data return Trait_Array_Type;
   --  The definition of the various attributes for each known locale.

   function Pool return String;
   --  Pool of accumulated string data for the known locales.

   function String_Addresses return String_Addresses_Type;
   --  Definition of strings within the pool referenced by the Locale_Data.

   procedure Environment_Initialize;
   --  Initialize the current locale based on the ZB_LANG/LANG environment
   --  variables.

   procedure Decompose_Name
     (Name      :     String;
      Language  : out Language_Type;
      Script    : out Script_Type;
      Territory : out Territory_Type;
      Encoding  : out Encoding_Type);
   --  Decompose a locale name, e.g., "en", "en_Latn_US.UTF8", etc. into it's
   --  component language, script and territory values.

   function Find_Traits
     (Language  : String;
      Script    : String;
      Territory : String)
      return Trait_Index_Type;
   --  Locate the traits entry "matching" the given locale data (matching
   --  attempts locale resolution, i.e., "fr_FR" => "fr", etc.

   function Latin_Digit
     (Ch   : Unicode_Character;
      Zero : Unicode_Character)
      return Unicode_Character;
   --  Convert a localized digit character to the corresponding Latin (ASCII)
   --  digit.  The Zero character gives the zero character for the localized
   --  digits.  E.g., for Arabic Latin_Digit ('٣', '٠') => '3'

   procedure Lookup_Traits
     (Language  :     String;
      Script    :     String;
      Territory :     String;
      Index     : out Trait_Index_Type;
      Found     : out Boolean);
   --  Binary search lookup of a traits by tag value.

   function To_String
     (Index : String_Index_Type)
      return String;
   --  Convert a string index of a pooled string to a string value.

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left, Right : Locale_Type)
      return Boolean
   is
   begin
      return
        Left.Language_Code = Right.Language_Code
        and then Left.Script_Code = Right.Script_Code
        and then Left.Territory_Code = Right.Territory_Code;
   end "=";

   ------------
   -- Codecs --
   ------------

   function Codecs
     (Locale : Locale_Type)
      return Codecs_Type
   is
   begin
      return Locale.Codecs;
   end Codecs;

   --------------------
   -- Current_Locale --
   --------------------

   function Current_Locale return Locale_Type is
   begin
      return Current_Locale_Value;
   end Current_Locale;

   -----------------
   -- Date_Format --
   -----------------

   function Date_Format
     (Locale : Locale_Type;
      Style  : Date_Time_Style_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Date_Formats (Style));
   end Date_Format;

   ----------------------
   -- Date_Time_Format --
   ----------------------

   function Date_Time_Format
     (Locale : Locale_Type;
      Style  : Date_Time_Style_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Date_Time_Formats (Style));
   end Date_Time_Format;

   -------------------------
   -- Day_Period_For_Time --
   -------------------------

   function Day_Period_For_Time
     (Locale : Locale_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type)
      return Day_Period_Type
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      if Minute = 0 and then Second = 0 then
         return Locale_Data (Index).Exact_Day_Periods (Hour);
      else
         return Locale_Data (Index).Within_Day_Periods (Hour);
      end if;
   end Day_Period_For_Time;

   ---------------------
   -- Day_Period_Name --
   ---------------------

   function Day_Period_Name
     (Locale     : Locale_Type;
      Day_Period : Day_Period_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Day_Period_Names (Day_Period));
   end Day_Period_Name;

   --------------------
   -- Decompose_Name --
   --------------------

   procedure Decompose_Name
     (Name      :     String;
      Language  : out Language_Type;
      Script    : out Script_Type;
      Territory : out Territory_Type;
      Encoding  : out Encoding_Type)
   is

      Separator : Unicode_Character := '_';
      First     : Positive          := Name.First;
      Last      : Natural           := 0;

      procedure Get_Separated_Item
        (Result :    out String;
         From   : in out Positive;
         Last   :        Natural);

      procedure Get_Separated_Item
        (Result :    out String;
         From   : in out Positive;
         Last   :        Natural)
      is

         First    : constant Positive := From;
         Position : Natural           := First;

      begin
         Find_Separator :
         loop
            Position := Position + 1;
            exit Find_Separator when Position >= Last
              or else Name (Position) = Separator;
         end loop Find_Separator;
         From := Position + 1;
         if Position >= Last then
            Position := Last;
         elsif Name (Position) = Separator then
            Position := Position - 1;
         end if;
         Result := Head (Name.Slice (First, Position), Result.Length);
      end Get_Separated_Item;

   begin
      --  Get any encoding info, e.g., "en_US.utf8"
      Encoding := Empty_Encoding;
      Last     := Index (Name, ".", First);
      if Last = 0 then
         Last := Name.Last;
      else
         Last := Last - 1;
         Encoding.Replace_Slice
           (1, Natural'Min (Name.Last - Last - 1, Encoding.Last),
            Name.Slice
              (Last + 2,
               Natural'Min (Name.Last - Last - 1, Encoding.Last) + Last + 1));
      end if;
      --  If strings contains dashes, assume it's the separator, e.g., "en-us"
      if Index (Name.Slice (First, Last), "-", First) /= 0 then
         Separator := '-';
      end if;
      Language  := Empty_Language;
      Script    := Empty_Script;
      Territory := Empty_Territory;
      Get_Separated_Item (Language, First, Last);
      Get_Separated_Item (Script, First, Last);
      Get_Separated_Item (Territory, First, Last);
      if Script (Script.Last) = ' ' then
         --  Fix up, the script is really the territory
         Territory := Script;
         Script    := Empty_Script;
      end if;
   end Decompose_Name;

   -----------------------
   -- Delocalize_Digits --
   -----------------------

   function Delocalize_Digits
     (Locale : Locale_Type;
      Value  : String)
      return String
   is
      Digit_Str : constant String :=
        Numeric_Item (Locale, Decimal_Digits_String);
      Zero   : constant Unicode_Character := Digit_Str (Digit_Str.First);
      Result : String                     := Value;
      Offset : Integer;
   begin
      for I in Value loop
         Offset :=
           Unicode_Character'Pos (Value (I)) - Unicode_Character'Pos (Zero);
         if Offset >= 0 and then Offset <= 9 then
            Result.Replace_Unicode (I, Latin_Digit (Result (I), Zero));
         end if;
      end loop;
      return Result;
   end Delocalize_Digits;

   ----------------------
   -- Encode_To_String --
   ----------------------

   function Encode_To_String
     (Locale : Locale_Type;
      Value  : String)
      return String
   is
   begin
      return Locale.Codecs.Encode (Value);
   end Encode_To_String;

   --------------
   -- Encoding --
   --------------

   function Encoding
     (Locale : Locale_Type)
      return String
   is
   begin
      return Non_Blank_Prefix (Locale.Encoding_Code);
   end Encoding;

   -----------------------------
   -- Encoding_Implementation --
   -----------------------------

   function Encoding_Implementation
     (Locale : Locale_Type)
      return String
   is
   begin
      return Locale.Codecs.Name;
   end Encoding_Implementation;

   ----------------------------
   -- Environment_Initialize --
   ----------------------------

   procedure Environment_Initialize is

      use Ada.Environment_Variables;

      ZBLang : constant String := "ZB_LANG";

   begin
      if Exists (To_Latin_1 (ZBLang)) then
         Set_Locale (From_Latin_1 (Value (To_Latin_1 (ZBLang))));
      else
         Set_Locale (ZanyBlue.OS.OS_Locale_Name);
      end if;
   end Environment_Initialize;

   --------------
   -- Era_Name --
   --------------

   function Era_Name
     (Locale : Locale_Type;
      Era    : Era_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Era_Names (Era));
   end Era_Name;

   -----------------
   -- Find_Traits --
   -----------------

   function Find_Traits
     (Language  : String;
      Script    : String;
      Territory : String)
      return Trait_Index_Type
   is
      Result : Trait_Index_Type := 1;
      Found  : Boolean          := False;
   begin
      Lookup_Traits (Language, Script, Territory, Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits (Language, Script, "", Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits (Language, "", Territory, Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits (Language, "", "", Result, Found);
      if Found then
         return Result;
      end if;
      Lookup_Traits ("", "", "", Result, Found);
      return Result;
   end Find_Traits;

   -------------------
   -- Full_Day_Name --
   -------------------

   function Full_Day_Name
     (Locale : Locale_Type;
      Day    : Day_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Full_Day_Names (Day));
   end Full_Day_Name;

   ---------------------
   -- Full_Month_Name --
   ---------------------

   function Full_Month_Name
     (Locale : Locale_Type;
      Month  : Month_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Full_Month_Names (Month));
   end Full_Month_Name;

   ----------------------
   -- Get_Locale_Codes --
   ----------------------

   procedure Get_Locale_Codes
     (Locale    :        Locale_Type;
      Language  : in out Language_Type;
      Script    : in out Script_Type;
      Territory : in out Territory_Type)
   is
   begin
      Language  := Locale.Language_Code;
      Script    := Locale.Script_Code;
      Territory := Locale.Territory_Code;
   end Get_Locale_Codes;

   ----------
   -- Hash --
   ----------

   function Hash
     (Key : Locale_Type)
      return Ada.Containers.Hash_Type
   is
   begin
      return Wide_Hash (Locale_Name (Key));
   end Hash;

   -----------------------
   -- Is_Locale_Defined --
   -----------------------

   function Is_Locale_Defined
     (Language  : String;
      Script    : String;
      Territory : String)
      return Boolean
   is
      Index : Trait_Index_Type;
      Found : Boolean;
   begin
      Lookup_Traits (Language, Script, Territory, Index, Found);
      return Found;
   end Is_Locale_Defined;

   --------------------
   -- Is_Root_Locale --
   --------------------

   function Is_Root_Locale
     (Locale : Locale_Type)
      return Boolean
   is
   begin
      return
        Locale.Language_Code = Empty_Language
        and then Locale.Script_Code = Empty_Script
        and then Locale.Territory_Code = Empty_Territory;
   end Is_Root_Locale;

   --------------
   -- Language --
   --------------

   function Language
     (Locale : Locale_Type)
      return String
   is
   begin
      return Non_Blank_Prefix (Locale.Language_Code);
   end Language;

   -----------------
   -- Latin_Digit --
   -----------------

   function Latin_Digit
     (Ch   : Unicode_Character;
      Zero : Unicode_Character)
      return Unicode_Character
   is
      Result : Unicode_Character := Ch;
   begin
      --  Quick check, might already a Latin digit
      if Zero /= '0' then
         Result :=
           Unicode_Character'Val
             (Unicode_Character'Pos ('0') +
              (Unicode_Character'Pos (Ch) - Unicode_Character'Pos (Zero)));
      end if;
      return Result;
   end Latin_Digit;

   -----------------
   -- Locale_Data --
   -----------------

   function Locale_Data return Trait_Array_Type is separate;

   -------------------
   -- Locale_Digits --
   -------------------

   function Locale_Digits
     (Locale    : Locale_Type;
      Lowercase : Boolean)
      return String
   is
      Result : String := 16 * '0';
   begin
      Result.Replace_Slice
        (1, 10, Head (Numeric_Item (Locale, Decimal_Digits_String), 10));
      if Lowercase then
         Result.Replace_Slice (11, 16, "abcdef");
      else
         Result.Replace_Slice (11, 16, "ABCDEF");
      end if;
      return Result;
   end Locale_Digits;

   ------------------
   -- Locale_Level --
   ------------------

   function Locale_Level
     (Locale : Locale_Type)
      return Level_Type
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return Locale_Data (Index).Level;
   end Locale_Level;

   -----------------
   -- Locale_Name --
   -----------------

   function Locale_Name
     (Language  : Language_Type;
      Script    : Script_Type;
      Territory : Territory_Type)
      return String
   is

      procedure Append
        (Result   : in out String;
         Position : in out Natural;
         Value    :        Unicode_Character);

      procedure Append
        (Result     : in out String;
         Position   : in out Natural;
         Value      :        String;
         Include_UC :        Boolean := True);

      procedure Append
        (Result   : in out String;
         Position : in out Natural;
         Value    :        Unicode_Character)
      is
      begin
         Position := Position + 1;
         Append (Result, Value);
      end Append;

      procedure Append
        (Result     : in out String;
         Position   : in out Natural;
         Value      :        String;
         Include_UC :        Boolean := True)
      is
      begin
         if Value (Value.First) = ' ' then
            return;
         end if;
         if Include_UC then
            Append (Result, Position, '_');
         end if;
         for I in Value loop
            if Value (I) /= ' ' then
               Append (Result, Position, Value (I));
            end if;
         end loop;
      end Append;

      Result   : String;
      Position : Natural := 0;

   begin
      if Language (Language.First) /= ' ' then
         Append (Result, Position, Language, Include_UC => False);
         Append (Result, Position, Script);
         Append (Result, Position, Territory);
      end if;
      return Result;
   end Locale_Name;

   -----------------
   -- Locale_Name --
   -----------------

   function Locale_Name
     (Locale : Locale_Type)
      return String
   is
      Result : constant String :=
        Locale_Name
          (Locale.Language_Code, Locale.Script_Code, Locale.Territory_Code);
      Encoding_Name : constant String := Encoding (Locale);
   begin
      if Encoding_Name.Length > 0 then
         return Result & "." & Encoding_Implementation (Locale);
      else
         return Result;
      end if;
   end Locale_Name;

   -------------------
   -- Lookup_Traits --
   -------------------

   procedure Lookup_Traits
     (Language  :     String;
      Script    :     String;
      Territory :     String;
      Index     : out Trait_Index_Type;
      Found     : out Boolean)
   is

      Key : Tag_Type :=
        Head (Language, Max_Language_Length) &
        Head (Script, Max_Script_Length) &
        Head (Territory, Max_Territory_Length);

      Left      : Trait_Index_Type := Locale_Data'First;
      Right     : Trait_Index_Type := Locale_Data'Last + 1;
      Center    : Trait_Index_Type;
      Candidate : Tag_Type;

   begin
      ASCII_Uppercase (Key);
      Found := False;
      if Key < Locale_Data (Left).Tag then
         return;
      end if;
      loop
         Center    := Left + (Right - Left) / 2;
         Candidate := Locale_Data (Center).Tag;
         if Key = Candidate then
            Index := Center;
            Found := True;
            return;
         end if;

         if Right - Left <= 1 then
            return;
         elsif Key < Candidate then
            Right := Center;
         else
            Left := Center;
         end if;
      end loop;
   end Lookup_Traits;

   -------------------------
   -- Make_Encoded_Locale --
   -------------------------

   function Make_Encoded_Locale
     (Language  : String;
      Territory : String;
      Encoding  : String)
      return Locale_Type
   is
   begin
      return Make_Locale (Language, "", Territory, Encoding);
   end Make_Encoded_Locale;

   -----------------
   -- Make_Locale --
   -----------------

   function Make_Locale
     (Locale_String : String)
      return Locale_Type
   is

      Language  : Language_Type;
      Script    : Script_Type;
      Territory : Territory_Type;
      Encoding  : Encoding_Type;

   begin
      Decompose_Name (Locale_String, Language, Script, Territory, Encoding);
      return Make_Locale (Language, Script, Territory, Encoding);
   end Make_Locale;

   -----------------
   -- Make_Locale --
   -----------------

   function Make_Locale
     (Language  : String;
      Territory : String)
      return Locale_Type
   is
   begin
      return Make_Locale (Language, "", Territory, "");
   end Make_Locale;

   -----------------
   -- Make_Locale --
   -----------------

   function Make_Locale
     (Language  : String;
      Script    : String;
      Territory : String)
      return Locale_Type
   is
   begin
      return Make_Locale (Language, Script, Territory, "");
   end Make_Locale;

   -----------------
   -- Make_Locale --
   -----------------

   function Make_Locale
     (Language  : String;
      Script    : String;
      Territory : String;
      Encoding  : String)
      return Locale_Type
   is
   begin
      return Result : Locale_Type do
         Result.Language_Code  := Head (Language, Max_Language_Length);
         Result.Script_Code    := Head (Script, Max_Script_Length);
         Result.Territory_Code := Head (Territory, Max_Territory_Length);
         Result.Encoding_Code  := Head (Encoding, Max_Encoding_Length);
         ASCII_Lowercase (Result.Language_Code);
         ASCII_Capitalize (Result.Script_Code);
         ASCII_Uppercase (Result.Territory_Code);
         ASCII_Uppercase (Result.Encoding_Code);
         Result.Traits_Index :=
           Find_Traits
             (Result.Language_Code, Result.Script_Code, Result.Territory_Code);
         Result.Codecs := Make_Codecs (Non_Blank_Prefix (Encoding));
      end return;
   end Make_Locale;

   ------------------------
   -- Make_Locale_Narrow --
   ------------------------

   function Make_Locale_Narrow
     (Locale_String : String)
      return Locale_Type
   is
   begin
      return Make_Locale (Locale_String);
   end Make_Locale_Narrow;

   -------------------------------
   -- Number_Of_Defined_Locales --
   -------------------------------

   function Number_Of_Defined_Locales return Positive is
   begin
      return Locale_Data'Length;
   end Number_Of_Defined_Locales;

   --------------------
   -- Numeric_Format --
   --------------------

   function Numeric_Format
     (Locale : Locale_Type;
      Style  : Numeric_Style_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Numeric_Formats (Style));
   end Numeric_Format;

   ------------------
   -- Numeric_Item --
   ------------------

   function Numeric_Item
     (Locale : Locale_Type;
      Item   : Numeric_Item_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Numeric_Items (Item));
   end Numeric_Item;

   ------------------
   -- Parent_Codes --
   ------------------

   procedure Parent_Codes
     (Language       : in out Language_Type;
      Script         : in out Script_Type;
      Territory      : in out Territory_Type;
      Base_Territory :        Territory_Type := Empty_Territory)
   is

      Language_P    : constant Boolean := Language (1) /= ' ';
      Script_P      : constant Boolean := Script (1) /= ' ';
      Territory_P   : constant Boolean := Territory (1) /= ' ';
      B_Territory_P : constant Boolean := Base_Territory (1) /= ' ';

   begin
      if Language_P and then Script_P and then Territory_P then
         Territory := Empty_Territory;
         return;
      end if;

      if Language_P and then Script_P and then not Territory_P
        and then B_Territory_P
      then
         Script    := Empty_Script;
         Territory := Base_Territory;
         return;
      end if;

      if Language_P and then Script_P and then not Territory_P
        and then not B_Territory_P
      then
         Script    := Empty_Script;
         Territory := Empty_Territory;
         return;
      end if;

      if Language_P and then not Script_P and then Territory_P then
         Script    := Empty_Script;
         Territory := Empty_Territory;
         return;
      end if;

      Language  := Empty_Language;
      Script    := Empty_Script;
      Territory := Empty_Territory;
   end Parent_Codes;

   ----------
   -- Pool --
   ----------

   function Pool return String is separate;

   ------------
   -- Script --
   ------------

   function Script
     (Locale : Locale_Type)
      return String
   is
   begin
      return Non_Blank_Prefix (Locale.Script_Code);
   end Script;

   ----------------
   -- Set_Locale --
   ----------------

   procedure Set_Locale (Locale : Locale_Type) is
   begin
      Current_Locale_Value := Locale;
   end Set_Locale;

   ----------------
   -- Set_Locale --
   ----------------

--     procedure Set_Locale (Name : String) is
--     begin
--        Set_Locale (Name);
--     end Set_Locale;

   ----------------
   -- Set_Locale --
   ----------------

   procedure Set_Locale (Wide_Name : String) is
   begin
      Set_Locale (Make_Locale (Wide_Name));
   end Set_Locale;

   ----------------
   -- Set_Traits --
   ----------------

--     procedure Set_Traits
--       (Locale : in out Locale_Type;
--        Name   :        String)
--     is
--     begin
--        Set_Traits (Locale, Name);
--     end Set_Traits;

   ----------------
   -- Set_Traits --
   ----------------

   procedure Set_Traits
     (Locale    : in out Locale_Type;
      Wide_Name :        String)
   is
      Language  : Language_Type;
      Script    : Script_Type;
      Territory : Territory_Type;
      Encoding  : Encoding_Type;
   begin
      Decompose_Name (Wide_Name, Language, Script, Territory, Encoding);
      Locale.Traits_Index := Find_Traits (Language, Script, Territory);
   end Set_Traits;

   --------------------
   -- Short_Day_Name --
   --------------------

   function Short_Day_Name
     (Locale : Locale_Type;
      Day    : Day_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Short_Day_Names (Day));
   end Short_Day_Name;

   ----------------------
   -- Short_Month_Name --
   ----------------------

   function Short_Month_Name
     (Locale : Locale_Type;
      Month  : Month_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Short_Month_Names (Month));
   end Short_Month_Name;

   ----------------------
   -- String_Addresses --
   ----------------------

   function String_Addresses return String_Addresses_Type is separate;

   ---------------
   -- Territory --
   ---------------

   function Territory
     (Locale : Locale_Type)
      return String
   is
   begin
      return Non_Blank_Prefix (Locale.Territory_Code);
   end Territory;

   -----------------
   -- Text_Layout --
   -----------------

   function Text_Layout
     (Locale : Locale_Type)
      return Text_Layout_Type
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return Locale_Data (Index).Text_Layout;
   end Text_Layout;

   -----------------
   -- Time_Format --
   -----------------

   function Time_Format
     (Locale : Locale_Type;
      Style  : Date_Time_Style_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Time_Formats (Style));
   end Time_Format;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Index : String_Index_Type)
      return String
   is
      Address : constant String_Address_Type := String_Addresses (Index);
   begin
      return Pool.Slice (Address.First, Address.Last);
   end To_String;

   -----------------
   -- Traits_Name --
   -----------------

   function Traits_Name
     (Locale : Locale_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return To_String (Locale_Data (Index).Name);
   end Traits_Name;

   ----------------
   -- Traits_Tag --
   ----------------

   function Traits_Tag
     (Locale : Locale_Type)
      return String
   is
      Index : constant Trait_Index_Type := Locale.Traits_Index;
   begin
      return Locale_Data (Index).Tag;
   end Traits_Tag;

   --------------------------
   -- Transfer_Locale_Data --
   --------------------------

   function Transfer_Locale_Data
     (Source_Locale : Locale_Type;
      Extra_Data    : Locale_Type)
      return Locale_Type
   is
      S_L, E_L : Language_Type;
      S_S, E_S : Script_Type;
      S_T, E_T : Territory_Type;
   begin
      Get_Locale_Codes (Source_Locale, S_L, S_S, S_T);
      Get_Locale_Codes (Extra_Data, E_L, E_S, E_T);
      if S_L = Empty_Language then
         S_L := E_L;
      end if;
      if S_L /= Empty_Language and then S_S = Empty_Script then
         S_S := E_S;
      end if;
      if S_L /= Empty_Language and then S_T = Empty_Territory then
         S_T := E_T;
      end if;
      return Make_Locale (S_L, S_S, S_T);
   end Transfer_Locale_Data;

begin  --  ZanyBlue.Text.Locales
   Environment_Initialize;
end ZanyBlue.Text.Locales;
