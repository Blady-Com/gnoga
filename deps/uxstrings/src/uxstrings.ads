with Ada.Strings;      use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;

package UXStrings is

   -- ISO/IEC 8859-1
   subtype Latin_1_Character is Character;
   subtype Latin_1_Character_Array is String;

   -- Unicode Basic Multilingual Plane
   -- Could be also named UCS_2_Character (Universal Coded Character Set)?
   subtype BMP_Character is Wide_Character;
   subtype BPM_Character_Array is Wide_String;

   -- Unicode planes
   -- Could be also named UCS_4_Character?
   subtype Unicode_Character is Wide_Wide_Character;
   subtype Unicode_Character_Array is Wide_Wide_String;

   type UTF_8_Character is new Character;
   type UTF_8_Character_Array is array (Positive range <>) of UTF_8_Character;

   type Encoding_Scheme is (UTF_16BE, UTF_16LE);
   type UTF_16_Character is new Wide_Character;
   type UTF_16_Character_Array is array (Positive range <>) of UTF_16_Character;

   subtype Char_Type is Latin_1_Character;
   subtype Wide_Char_Type is BMP_Character;
   subtype Wide_Wide_Char_Type is Unicode_Character;
   type UXString is tagged private with
      Constant_Indexing => Element,
      Variable_Indexing => Reference,
      Iterable          => (First => First, Next => Next, Has_Element => Has_Element, Element => Element);

   Null_UXString : constant UXString;

   function Length (Source : UXString) return Natural;

   function Element (Source : UXString; Index : Positive) return Char_Type;
   function Element (Source : UXString; Index : Positive) return Wide_Char_Type;
   function Element (Source : UXString; Index : Positive) return Wide_Wide_Char_Type;

   -- Possible ambigous uses with literal expressions
   type Character_Reference (Char : not null access Char_Type) is limited private with
      Implicit_Dereference => Char;
   function Reference (Source : aliased in out UXString; Index : Positive) return Character_Reference;
   type Wide_Character_Reference (Wide_Char : not null access Wide_Char_Type) is limited private with
      Implicit_Dereference => Wide_Char;
   function Reference (Source : aliased in out UXString; Index : Positive) return Wide_Character_Reference;
   type Wide_Wide_Character_Reference (Wide_Wide_Char : not null access Wide_Wide_Char_Type) is limited private with
      Implicit_Dereference => Wide_Wide_Char;
   function Reference (Source : aliased in out UXString; Index : Positive) return Wide_Wide_Character_Reference;

   function First (Source : UXString) return Positive;
   function Next (Source : UXString; Index : Positive) return Positive;
   function Has_Element (Source : UXString; Index : Positive) return Boolean;

   function Is_Latin_1 (Source : UXString; Index : Positive) return Boolean;
   function Is_Latin_1 (Source : UXString) return Boolean;
   function To_Latin_1
     (Source : UXString; Index : Positive; Substitute : in Latin_1_Character := ' ') return Latin_1_Character;
   function To_Latin_1 (Source : UXString; Substitute : in Latin_1_Character := ' ') return Latin_1_Character_Array;
   function From_Latin_1 (Str : Latin_1_Character_Array) return UXString;

   function Is_BMP (Source : UXString; Index : Positive) return Boolean;
   function Is_BMP (Source : UXString) return Boolean;
   function To_BPM (Source : UXString; Index : Positive; Substitute : in BMP_Character := ' ') return BMP_Character;
   function To_BPM (Source : UXString; Substitute : in BMP_Character := ' ') return BPM_Character_Array;
   function From_BMP (Str : BPM_Character_Array) return UXString;

   function Is_Unicode (Source : UXString; Index : Positive) return Boolean;
   function Is_Unicode (Source : UXString) return Boolean;
   function To_Unicode (Source : UXString; Index : Positive) return Unicode_Character;
   function To_Unicode (Source : UXString) return Unicode_Character_Array;
   function From_Unicode (Str : Unicode_Character_Array) return UXString;

   function To_UTF_8 (Source : UXString; Output_BOM : Boolean := False) return UTF_8_Character_Array;
   function From_UTF8 (Str : UTF_8_Character_Array) return UXString;

   function To_UTF_16
     (Source : UXString; Output_Scheme : Encoding_Scheme; Output_BOM : Boolean := False) return UTF_16_Character_Array;
   function From_UTF_16 (Str : UTF_16_Character_Array; Input_Scheme : Encoding_Scheme) return UXString;

   -- Possible ambigous uses with literal expressions
   procedure Set (Target : out UXString; Latin_1_Source : Latin_1_Character_Array);
   procedure Set (Target : out UXString; BMP_Source : BPM_Character_Array);
   procedure Set (Target : out UXString; Unicode_Source : Unicode_Character_Array);

   procedure Append (Source : in out UXString; New_Item : UXString);

   procedure Append (Source : in out UXString; New_Latin_1_Item : Latin_1_Character_Array);
   procedure Append (Source : in out UXString; New_BPM_Item : BPM_Character_Array);
   procedure Append (Source : in out UXString; New_Unicode_Item : Unicode_Character_Array);

   procedure Append (Source : in out UXString; New_Item : Latin_1_Character);
   procedure Append (Source : in out UXString; New_Wide_Item : BMP_Character);
   procedure Append (Source : in out UXString; New_Wide_Wide_Item : Unicode_Character);

   function "&" (Left : UXString; Right : UXString) return UXString;

   -- Only Latin-1 forms to avoid ambigous literal expressions
   function "&" (Left : UXString; Right : Latin_1_Character_Array) return UXString;
   function "&" (Left : Latin_1_Character_Array; Right : UXString) return UXString;
   function "&" (Left : UXString; Right : Latin_1_Character) return UXString;
   function "&" (Left : Latin_1_Character; Right : UXString) return UXString;

   -- Possible ambigous uses with literal expressions
   procedure Replace_Element (Source : in out UXString; Index : Positive; By : Latin_1_Character);
   procedure Replace_Element (Source : in out UXString; Index : Positive; By : BMP_Character);
   procedure Replace_Element (Source : in out UXString; Index : Positive; By : Unicode_Character);

   function Slice (Source : UXString; Low : Positive; High : Natural) return UXString;
   procedure Slice (Source : UXString; Target : out UXString; Low : Positive; High : Natural);

   -- Only Latin-1 forms to avoid ambigous literal expressions
   function "=" (Left : UXString; Right : UXString) return Boolean;
   function "=" (Left : UXString; Right : Latin_1_Character_Array) return Boolean;
   function "=" (Left : Latin_1_Character_Array; Right : UXString) return Boolean;
   function "<" (Left : UXString; Right : UXString) return Boolean;
   function "<" (Left : UXString; Right : Latin_1_Character_Array) return Boolean;
   function "<" (Left : Latin_1_Character_Array; Right : UXString) return Boolean;
   function "<=" (Left : UXString; Right : UXString) return Boolean;
   function "<=" (Left : UXString; Right : Latin_1_Character_Array) return Boolean;
   function "<=" (Left : Latin_1_Character_Array; Right : UXString) return Boolean;
   function ">" (Left : UXString; Right : UXString) return Boolean;
   function ">" (Left : UXString; Right : Latin_1_Character_Array) return Boolean;
   function ">" (Left : Latin_1_Character_Array; Right : UXString) return Boolean;
   function ">=" (Left : UXString; Right : UXString) return Boolean;
   function ">=" (Left : UXString; Right : Latin_1_Character_Array) return Boolean;
   function ">=" (Left : Latin_1_Character_Array; Right : UXString) return Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   -- Could be defined also for Wide_Character_Mapping and Wide_Wide_Character_Mapping? Other smarter forms?
   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;
   function Index
     (Source : UXString; Pattern : UXString; Going : Direction := Forward; Mapping : Maps.Character_Mapping_Function)
      return Natural;
   function Index
     (Source : UXString; Set : Maps.Character_Set; Test : Membership := Inside; Going : Direction := Forward)
      return Natural;
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural;
   function Index
     (Source : UXString; Set : Maps.Character_Set; From : Positive; Test : Membership := Inside;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank (Source : UXString; Going : Direction := Forward) return Natural;
   function Index_Non_Blank (Source : UXString; From : Positive; Going : Direction := Forward) return Natural;

   function Count
     (Source : UXString; Pattern : UXString; Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;
   function Count (Source : UXString; Pattern : UXString; Mapping : Maps.Character_Mapping_Function) return Natural;
   function Count (Source : UXString; Set : Maps.Character_Set) return Natural;

   procedure Find_Token
     (Source :     UXString; Set : Maps.Character_Set; From : Positive; Test : Membership; First : out Positive;
      Last   : out Natural);
   procedure Find_Token
     (Source : UXString; Set : Maps.Character_Set; Test : Membership; First : out Positive; Last : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate (Source : UXString; Mapping : Maps.Character_Mapping) return UXString;
   procedure Translate (Source : in out UXString; Mapping : Maps.Character_Mapping);
   function Translate (Source : UXString; Mapping : Maps.Character_Mapping_Function) return UXString;
   procedure Translate (Source : in out UXString; Mapping : Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice (Source : UXString; Low : Positive; High : Natural; By : UXString) return UXString;
   procedure Replace_Slice (Source : in out UXString; Low : Positive; High : Natural; By : UXString);
   function Insert (Source : UXString; Before : Positive; New_Item : UXString) return UXString;
   procedure Insert (Source : in out UXString; Before : Positive; New_Item : UXString);
   function Overwrite (Source : UXString; Position : Positive; New_Item : UXString) return UXString;
   procedure Overwrite (Source : in out UXString; Position : Positive; New_Item : UXString);
   function Delete (Source : UXString; From : Positive; Through : Natural) return UXString;
   procedure Delete (Source : in out UXString; From : Positive; Through : Natural);
   function Trim (Source : UXString; Side : Trim_End) return UXString;
   procedure Trim (Source : in out UXString; Side : Trim_End);
   function Trim (Source : UXString; Left : Maps.Character_Set; Right : Maps.Character_Set) return UXString;
   procedure Trim (Source : in out UXString; Left : Maps.Character_Set; Right : Maps.Character_Set);
   function Head (Source : UXString; Count : Natural; Pad : Latin_1_Character := Space) return UXString;
   procedure Head (Source : in out UXString; Count : Natural; Pad : Latin_1_Character := Space);
   function Tail (Source : UXString; Count : Natural; Pad : Latin_1_Character := Space) return UXString;
   procedure Tail (Source : in out UXString; Count : Natural; Pad : Latin_1_Character := Space);

   -- Only Latin-1 forms to avoid ambigous literal expressions
   function "*" (Left : Natural; Right : Latin_1_Character) return UXString;
   function "*" (Left : Natural; Right : Latin_1_Character_Array) return UXString;
   function "*" (Left : Natural; Right : UXString) return UXString;

private
   type Character_Reference (Char : not null access Char_Type) is null record;
   type Wide_Character_Reference (Wide_Char : not null access Wide_Char_Type) is null record;
   type Wide_Wide_Character_Reference (Wide_Wide_Char : not null access Wide_Wide_Char_Type) is null record;

   type UXString is tagged null record;
   Null_UXString : constant UXString := (others => <>);

end UXStrings;
