with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Wide_Wide_Maps; use Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.UTF_Encoding;
private with Ada.Finalization;
private with Ada.Streams;

package UXStrings is

   type Encoding_Scheme is (Latin_1, UTF_8, UTF_16BE, UTF_16LE);
   subtype UTF_16_Encoding_Scheme is Encoding_Scheme range UTF_16BE .. UTF_16LE;

   -- ISO/IEC 8859-1
   subtype Latin_1_Character is Character;
   subtype Latin_1_Character_Array is String;

   -- Unicode Basic Multilingual Plane
   -- Could be also named UCS_2_Character (Universal Coded Character Set)?
   subtype BMP_Character is Wide_Character;
   subtype BMP_Character_Array is Wide_String;

   -- Unicode planes
   -- Could be also named UCS_4_Character?
   subtype Unicode_Character is Wide_Wide_Character;
   subtype Unicode_Character_Array is Wide_Wide_String;

   -- UTF encoding
   subtype UTF_8_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   subtype UTF_16_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;

   type UXString is tagged private with
      Constant_Indexing => Element,
      Iterable          => (First => First, Next => Next, Has_Element => Has_Element, Element => Element),
      String_Literal    => From_Unicode;

   Null_UXString : constant UXString;

   function Length (Source : UXString) return Natural;

   function First (Source : UXString) return Positive;
   function Next (Source : UXString; Index : Positive) return Positive;
   procedure Next (Source : UXString; Index : in out Positive);
   function Has_Element (Source : UXString; Index : Positive) return Boolean;
   function Element (Source : UXString; Index : Positive) return Unicode_Character;
   function Last (Source : UXString) return Natural;

   function Is_Latin_1 (Source : UXString; Index : Positive) return Boolean;
   function Is_Latin_1 (Source : UXString) return Boolean;
   function Get_Latin_1
     (Source : UXString; Index : Positive; Substitute : in Latin_1_Character := '¿') return Latin_1_Character;
   function To_Latin_1 (Source : UXString; Substitute : in Latin_1_Character := '¿') return Latin_1_Character_Array;
   function From_Latin_1 (Item : Latin_1_Character) return UXString;
   function From_Latin_1 (Source : Latin_1_Character_Array) return UXString;

   function Is_BMP (Source : UXString; Index : Positive) return Boolean;
   function Is_BMP (Source : UXString) return Boolean;
   function Get_BMP (Source : UXString; Index : Positive; Substitute : in BMP_Character := '¿') return BMP_Character;
   function To_BMP (Source : UXString; Substitute : in BMP_Character := '¿') return BMP_Character_Array;
   function From_BMP (Item : BMP_Character) return UXString;
   function From_BMP (Source : BMP_Character_Array) return UXString;

   function Is_Unicode (Source : UXString; Index : Positive) return Boolean;
   function Is_Unicode (Source : UXString) return Boolean;
   function Get_Unicode (Source : UXString; Index : Positive) return Unicode_Character;
   function To_Unicode (Source : UXString) return Unicode_Character_Array;
   function From_Unicode (Item : Unicode_Character) return UXString;
   function From_Unicode (Source : Unicode_Character_Array) return UXString;

   function To_UTF_8 (Source : UXString; Output_BOM : Boolean := False) return UTF_8_Character_Array;
   function From_UTF_8 (Source : UTF_8_Character_Array) return UXString;

   function To_UTF_16
     (Source : UXString; Output_Scheme : UTF_16_Encoding_Scheme; Output_BOM : Boolean := False)
      return UTF_16_Character_Array;
   function From_UTF_16 (Source : UTF_16_Character_Array; Input_Scheme : UTF_16_Encoding_Scheme) return UXString;

   procedure Set (Target : out UXString; Unicode_Source : Unicode_Character_Array);

   procedure Append (Source : in out UXString; New_Item : UXString);
   procedure Append (Source : in out UXString; New_Item : Unicode_Character);

   procedure Prepend (Source : in out UXString; New_Item : UXString);
   procedure Prepend (Source : in out UXString; New_Item : Unicode_Character);

   function "&" (Left : UXString; Right : UXString) return UXString;
   function "&" (Left : UXString; Right : Unicode_Character) return UXString;
   function "&" (Left : Unicode_Character; Right : UXString) return UXString;

   procedure Replace_Latin_1 (Source : in out UXString; Index : Positive; By : Latin_1_Character);
   procedure Replace_BMP (Source : in out UXString; Index : Positive; By : BMP_Character);
   procedure Replace_Unicode (Source : in out UXString; Index : Positive; By : Unicode_Character);

   function Slice (Source : UXString; Low : Positive; High : Natural) return UXString;
   procedure Slice (Source : UXString; Target : out UXString; Low : Positive; High : Natural);

   function "=" (Left : UXString; Right : UXString) return Boolean;
   function "<" (Left : UXString; Right : UXString) return Boolean;
   function "<=" (Left : UXString; Right : UXString) return Boolean;
   function ">" (Left : UXString; Right : UXString) return Boolean;
   function ">=" (Left : UXString; Right : UXString) return Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership := Inside; Going : Direction := Forward)
      return Natural;
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership := Inside;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank (Source : UXString; Going : Direction := Forward) return Natural;
   function Index_Non_Blank (Source : UXString; From : Positive; Going : Direction := Forward) return Natural;

   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   function Count (Source : UXString; Set : Wide_Wide_Character_Set) return Natural;

   procedure Find_Token
     (Source :     UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership; First : out Positive;
      Last   : out Natural);
   procedure Find_Token
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership; First : out Positive; Last : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping) return UXString;
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping);
   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString;
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping_Function);

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
   function Trim (Source : UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set) return UXString;
   procedure Trim (Source : in out UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set);
   function Head (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   procedure Head (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   function Tail (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   procedure Tail (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);

   function "*" (Left : Natural; Right : UXString) return UXString;
   function "*" (Left : Natural; Right : Unicode_Character) return UXString;

private

   type UTF_8_Characters_Access is access UTF_8_Character_Array;
   type UXString is new Ada.Finalization.Controlled with record
      Chars : UTF_8_Characters_Access := new UTF_8_Character_Array (2 .. 1);
   end record;

   procedure Adjust (Object : in out UXString);
   procedure Finalize (Object : in out UXString);

   procedure Bounded_Move (Source : in out UXString; Target : out UXString; Max : Natural; Last : out Natural);

   procedure UXString_Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out UXString);
   for UXString'Read use UXString_Read;

   procedure UXString_Write (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : UXString);
   for UXString'Write use UXString_Write;

   Null_UXString : constant UXString := (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array (2 .. 1));

end UXStrings;
