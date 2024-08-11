-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings3.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString implementation.
-- NOTES                        : Ada 2022
--
-- COPYRIGHT                    : (c) Pascal Pignard 2024
-- LICENCE                      : CeCILL-C (https://cecill.info)
-- CONTACT                      : http://blady.chez.com
-------------------------------------------------------------------------------

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings; use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Wide_Wide_Characters.Handling;          use Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Conversions;                 use Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
with GNAT.UTF_32;

with UXStrings.Lists;

package body UXStrings is

   use Ada.Strings.Wide_Wide_Unbounded;

   -- Encoding Scheme cross correspondance

   To_UTF_Encoding : constant array (Encoding_Scheme) of Ada.Strings.UTF_Encoding.Encoding_Scheme :=
     (Ada.Strings.UTF_Encoding.UTF_8, Ada.Strings.UTF_Encoding.UTF_8, Ada.Strings.UTF_Encoding.UTF_8,
      Ada.Strings.UTF_Encoding.UTF_16BE, Ada.Strings.UTF_Encoding.UTF_16LE);

   -- Stream management

   -------------------
   -- UXString_Read --
   -------------------

   procedure UXString_Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out UXString) is
   begin
      pragma Compile_Time_Warning (Standard.True, "UXString_Read unimplemented");
      raise Program_Error with "Unimplemented procedure UXString_Read";
   end UXString_Read;

   --------------------
   -- UXString_Write --
   --------------------

   procedure UXString_Write (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : UXString) is
   begin
      UTF_8_Character_Array'Write (Stream, To_UTF_8 (Item));
   end UXString_Write;

   ------------------
   -- Bounded_Move --
   ------------------

   procedure Bounded_Move (Source : in out UXString; Target : out UXString; Max : Natural; Last : out Natural) is
   begin
      Last   := Natural'Min (Target.Last, Max);
      Target := Source.Slice (1, Last);
      Delete (Source, 1, Last);
   end Bounded_Move;

   -- UXStrings API implementation

   ------------
   -- Length --
   ------------

   function Length (Source : UXString) return Natural is
   begin
      return Length (Source.Chars);
   end Length;

   -----------
   -- First --
   -----------

   function First (Source : UXString) return Positive is
   begin
      return 1;
   end First;

   ----------
   -- Next --
   ----------

   function Next (Source : UXString; Index : Positive) return Positive is
   begin
      return Index + 1;
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Source : UXString; Index : in out Positive) is
   begin
      Index := Next (Source, Index);
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Source : UXString; Index : Positive) return Boolean is
   begin
      return Index <= Length (Source);
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (Source : UXString; Index : Positive) return Unicode_Character renames Get_Unicode;

   ----------
   -- Last --
   ----------

   function Last (Source : UXString) return Natural is
   begin
      return Length (Source);
   end Last;

   ---------------------------
   -- Character_Set_Version --
   ---------------------------

   function Character_Set_Version return UXString is
   begin
      return From_ASCII (Character_Set_Version);
   end Character_Set_Version;

   --------------
   -- Is_ASCII --
   --------------

   function Is_ASCII (Source : UXString; Index : Positive) return Boolean is
   begin
      return Unicode_Character'Pos (Source (Index)) < 16#80#;
   end Is_ASCII;

   --------------
   -- Is_ASCII --
   --------------

   function Is_ASCII (Source : UXString) return Boolean is
   begin
      return (for all Item of Source => Unicode_Character'Pos (Item) < 16#80#);
   end Is_ASCII;

   ---------------
   -- To_ASCII --
   ---------------

   function To_ASCII
     (Item : unicode_character; Substitute : in ASCII_Character := Q_L) return ASCII_Character
   is
      Pos : constant Natural := Unicode_Character'Pos (Item);
   begin
      if Pos > 16#7F# then
         return Substitute;
      else
         return ASCII_Character'Val (Pos);
      end if;
   end To_ASCII;

  ---------------
   -- Get_ASCII --
   ---------------

   function Get_ASCII
     (Source : UXString; Index : Positive; Substitute : in ASCII_Character := Q_L) return ASCII_Character
   is
   begin
      return to_ascii (Source (Index), substitute);
   end Get_ASCII;

   --------------
   -- To_ASCII --
   --------------

   function To_ASCII (Source : UXString; Substitute : in ASCII_Character := Q_L) return ASCII_Character_Array is
   begin
      return [for E of Source => to_ascii (E , substitute)];
   end To_ASCII;

   ----------------
   -- From_ASCII --
   ----------------

   function From_ASCII (Item : ASCII_Character) return UXString is
   begin
      return From_Unicode ((1 => To_Wide_Wide_Character (Item)));
   end From_ASCII;

   ----------------
   -- From_ASCII --
   ----------------

   function From_ASCII (Source : ASCII_Character_Array) return UXString is
   begin
      return From_Unicode (To_Wide_Wide_String (Source));
   end From_ASCII;

   ----------------
   -- Is_Latin_1 --
   ----------------

   function Is_Latin_1 (Source : UXString; Index : Positive) return Boolean is
   begin
      return Unicode_Character'Pos (Source (Index)) < 16#1_00#;
   end Is_Latin_1;

   ----------------
   -- Is_Latin_1 --
   ----------------

   function Is_Latin_1 (Source : UXString) return Boolean is
   begin
      return (for all Item of Source => Unicode_Character'Pos (Item) < 16#1_00#);
   end Is_Latin_1;

   -----------------
   -- Get_Latin_1 --
   -----------------

   function Get_Latin_1
     (Source : UXString; Index : Positive; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character
   is
   begin
      return To_Character (Source (Index), Substitute);
   end Get_Latin_1;

   ----------------
   -- To_Latin_1 --
   ----------------

   function To_Latin_1 (Source : UXString; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character_Array
   is
   begin
      return To_String (To_Wide_Wide_String (Source.Chars), Substitute);
   end To_Latin_1;

   ------------------
   -- From_Latin_1 --
   ------------------

   function From_Latin_1 (Item : Latin_1_Character) return UXString is
   begin
      return From_Unicode ((1 => To_Wide_Wide_Character (Item)));
   end From_Latin_1;

   ------------------
   -- From_Latin_1 --
   ------------------

   function From_Latin_1 (Source : Latin_1_Character_Array) return UXString is
   begin
      return From_Unicode (To_Wide_Wide_String (Source));
   end From_Latin_1;

   ------------
   -- Is_BMP --
   ------------

   function Is_BMP (Source : UXString; Index : Positive) return Boolean is
   begin
      return Unicode_Character'Pos (Source (Index)) < 16#1_0000#;
   end Is_BMP;

   ------------
   -- Is_BMP --
   ------------

   function Is_BMP (Source : UXString) return Boolean is
   begin
      return (for all Item of Source => Unicode_Character'Pos (Item) < 16#1_0000#);
   end Is_BMP;

   -------------
   -- Get_BMP --
   -------------

   function Get_BMP (Source : UXString; Index : Positive; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character
   is
   begin
      return To_Wide_Character (Source (Index), Substitute);
   end Get_BMP;

   ------------
   -- To_BMP --
   ------------

   function To_BMP (Source : UXString; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character_Array is
   begin
      return To_Wide_String (To_Wide_Wide_String (Source.Chars), Substitute);
   end To_BMP;

   --------------
   -- From_BMP --
   --------------

   function From_BMP (Item : BMP_Character) return UXString is
   begin
      return From_Unicode ((1 => To_Wide_Wide_Character (Item)));
   end From_BMP;

   --------------
   -- From_BMP --
   --------------

   function From_BMP (Source : BMP_Character_Array) return UXString is
   begin
      return From_Unicode (To_Wide_Wide_String (Source));
   end From_BMP;

   ----------------
   -- Is_Unicode --
   ----------------

   function Is_Unicode (Source : UXString; Index : Positive) return Boolean is
   begin
      return True;
   end Is_Unicode;

   ----------------
   -- Is_Unicode --
   ----------------

   function Is_Unicode (Source : UXString) return Boolean is
   begin
      return True;
   end Is_Unicode;

   -----------------
   -- Get_Unicode --
   -----------------

   function Get_Unicode (Source : UXString; Index : Positive) return Unicode_Character is
   begin
      return Element (Source.Chars, Index);
   end Get_Unicode;

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (Source : UXString) return Unicode_Character_Array is
   begin
      return To_Wide_Wide_String (Source.Chars);
   end To_Unicode;

   ------------------
   -- From_Unicode --
   ------------------

   function From_Unicode (Item : Unicode_Character) return UXString is
   begin
      return From_Unicode ((1 => Item));
   end From_Unicode;

   ------------------
   -- From_Unicode --
   ------------------

   function From_Unicode (Source : Unicode_Character_Array) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := To_Unbounded_Wide_Wide_String (Source);
      end return;
   end From_Unicode;

   --------------
   -- To_UTF_8 --
   --------------

   function To_UTF_8 (Source : UXString; Output_BOM : Boolean := False) return UTF_8_Character_Array is
   begin
      return Encode (To_Wide_Wide_String (Source.Chars), Output_BOM);
   end To_UTF_8;

   ----------------
   -- From_UTF_8 --
   ----------------

   function From_UTF_8 (Source : UTF_8_Character_Array) return UXString is
   begin
      return From_Unicode (Decode (Source));
   end From_UTF_8;

   ---------------
   -- To_UTF_16 --
   ---------------

   function To_UTF_16
     (Source : UXString; Output_Scheme : UTF_16_Encoding_Scheme; Output_BOM : Boolean := False)
      return UTF_16_Character_Array
   is
   begin
      return
        UTF_16_Character_Array'
          (Encode (To_Wide_Wide_String (Source.Chars), To_UTF_Encoding (Output_Scheme), Output_BOM));
   end To_UTF_16;

   -----------------
   -- From_UTF_16 --
   -----------------

   function From_UTF_16 (Source : UTF_16_Character_Array; Input_Scheme : UTF_16_Encoding_Scheme) return UXString is
   begin
      return From_Unicode (Decode (Source, To_UTF_Encoding (Input_Scheme)));
   end From_UTF_16;

   ---------
   -- Set --
   ---------

   procedure Set (Target : out UXString; Source : Unicode_Character_Array) is
   begin
      Target := From_Unicode (Source);
   end Set;

   ------------
   -- Append --
   ------------

   procedure Append (Source : in out UXString; New_Item : UXString) is
   begin
      Append (Source.Chars, New_Item.Chars);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Source : in out UXString; New_Item : Unicode_Character) is
   begin
      Append (Source.Chars, New_Item);
   end Append;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Source : in out UXString; New_Item : UXString) is
   begin
      Source.Chars := New_Item.Chars & Source.Chars;
   end Prepend;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Source : in out UXString; New_Item : Unicode_Character) is
   begin
      Source.Chars := New_Item & Source.Chars;
   end Prepend;

   ---------
   -- "&" --
   ---------

   function "&" (Left : UXString; Right : UXString) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := Left.Chars & Right.Chars;
      end return;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : UXString; Right : Unicode_Character) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := Left.Chars & Right;
      end return;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Unicode_Character; Right : UXString) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := Left & Right.Chars;
      end return;
   end "&";

   -------------------
   -- Replace_ASCII --
   -------------------

   procedure Replace_ASCII (Source : in out UXString; Index : Positive; By : ASCII_Character) is
   begin
      Source := Slice (Source, 1, Index - 1) & From_ASCII (By) & Slice (Source, Index + 1, Length (Source));
   end Replace_ASCII;

   ---------------------
   -- Replace_Latin_1 --
   ---------------------

   procedure Replace_Latin_1 (Source : in out UXString; Index : Positive; By : Latin_1_Character) is
   begin
      Source := Slice (Source, 1, Index - 1) & From_Latin_1 (By) & Slice (Source, Index + 1, Length (Source));
   end Replace_Latin_1;

   -----------------
   -- Replace_BMP --
   -------*---------

   procedure Replace_BMP (Source : in out UXString; Index : Positive; By : BMP_Character) is
   begin
      Source := Slice (Source, 1, Index - 1) & From_BMP (By) & Slice (Source, Index + 1, Length (Source));
   end Replace_BMP;

   ---------------------
   -- Replace_Unicode --
   ---------------------

   procedure Replace_Unicode (Source : in out UXString; Index : Positive; By : Unicode_Character) is
   begin
      Source := Slice (Source, 1, Index - 1) & From_Unicode (By) & Slice (Source, Index + 1, Length (Source));
   end Replace_Unicode;

   -----------
   -- Slice --
   -----------

   function Slice (Source : UXString; Low : Positive; High : Integer) return UXString is
   begin
      return From_Unicode (Slice (Source.Chars, Low, High));
   end Slice;

   -----------
   -- Slice --
   -----------

   procedure Slice (Source : UXString; Target : out UXString; Low : Integer; High : Natural) is
   begin
      Target := Slice (Source, Low, High);
   end Slice;

   ---------
   -- "=" --
   ---------

   function "=" (Left : UXString; Right : UXString) return Boolean is
   begin
      return Left.Chars = Right.Chars;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left : UXString; Right : UXString) return Boolean is
   begin
      return Left.Chars < Right.Chars;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left : UXString; Right : UXString) return Boolean is
   begin
      return Left.Chars <= Right.Chars;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left : UXString; Right : UXString) return Boolean is
   begin
      return Left.Chars > Right.Chars;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left : UXString; Right : UXString) return Boolean is
   begin
      return Left.Chars >= Right.Chars;
   end ">=";

   -----------
   -- Index --
   -----------

   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural
   is
   begin
      if Going = Forward then
         return Index (Source, Pattern, Source.First, Forward, Mapping);
      else
         return Index (Source, Pattern, Source.Last, Backward, Mapping);
      end if;
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural
   is
   begin
      if Going = Forward then
         return Index (Source, Pattern, Source.First, Forward, Mapping);
      else
         return Index (Source, Pattern, Source.Last, Backward, Mapping);
      end if;
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership := Inside; Going : Direction := Forward)
      return Natural
   is
   begin
      if Going = Forward then
         return Index (Source, Set, Source.First, Test, Forward);
      else
         return Index (Source, Set, Source.Last, Test, Backward);
      end if;
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural
   is
   begin
      return Index (Source.Chars, To_Wide_Wide_String (Pattern.Chars), From, Going, Mapping);
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural
   is
   begin
      return Index (Source.Chars, To_Wide_Wide_String (Pattern.Chars), From, Going, Mapping);
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
   begin
      return Index (Source.Chars, Set, From, Test, Going);
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank (Source : UXString; Going : Direction := Forward) return Natural is
   begin
      return Index (Source, To_Set (Wide_Wide_Space), Outside, Going);
   end Index_Non_Blank;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank (Source : UXString; From : Positive; Going : Direction := Forward) return Natural is
   begin
      return Index (Source, To_Set (Wide_Wide_Space), From, Outside, Going);
   end Index_Non_Blank;

   -----------
   -- Count --
   -----------

   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity) return Natural
   is
   begin
      return Count (Source.Chars, To_Wide_Wide_String (Pattern.Chars), Mapping);
   end Count;

   -----------
   -- Count --
   -----------

   function Count (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return Natural
   is
   begin
      return Count (Source.Chars, To_Wide_Wide_String (Pattern.Chars), Mapping);
   end Count;

   -----------
   -- Count --
   -----------

   function Count (Source : UXString; Set : Wide_Wide_Character_Set) return Natural is
   begin
      return Count (Source.Chars, Set);
   end Count;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source :     UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership; First : out Positive;
      Last   : out Natural)
   is
   begin
      Find_Token (Source.Chars, Set, From, Test, First, Last);
   end Find_Token;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership; First : out Positive; Last : out Natural)
   is
   begin
      Find_Token (Source, Set, Source.First, Test, First, Last);
   end Find_Token;

   ---------------
   -- Translate --
   ---------------

   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := Translate (Source.Chars, Mapping);
      end return;
   end Translate;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping) is
   begin
      Source := Translate (Source, Mapping);
   end Translate;

   ---------------
   -- Translate --
   ---------------

   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := Translate (Source.Chars, Mapping);
      end return;
   end Translate;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping_Function) is
   begin
      Source := Translate (Source, Mapping);
   end Translate;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice (Source : UXString; Low : Positive; High : Natural; By : UXString) return UXString is
   begin
      if Low <= High then
         return Source.Slice (Source.First, Low - 1) & By & Source.Slice (High + 1, Source.Last);
      else
         return Insert (Source, Low, By);
      end if;
   end Replace_Slice;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice (Source : in out UXString; Low : Positive; High : Natural; By : UXString) is
   begin
      Source := Replace_Slice (Source, Low, High, By);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert (Source : UXString; Before : Positive; New_Item : UXString) return UXString is
   begin
      return Source.Slice (Source.First, Before - 1) & New_Item & Source.Slice (Before, Source.Last);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (Source : in out UXString; Before : Positive; New_Item : UXString) is
   begin
      Source := Insert (Source, Before, New_Item);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite (Source : UXString; Position : Positive; New_Item : UXString) return UXString is
   begin
      return
        Replace_Slice
          (Source, Position, Position + Natural'Min (Source.Length - Position + 1, New_Item.Length) - 1, New_Item);
   end Overwrite;

   ---------------
   -- Overwrite --
   ---------------

   procedure Overwrite (Source : in out UXString; Position : Positive; New_Item : UXString) is
   begin
      Source := Overwrite (Source, Position, New_Item);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete (Source : UXString; From : Positive; Through : Natural) return UXString is
   begin
      if From <= Through then
         return Replace_Slice (Source, From, Through, Null_UXString);
      else
         return Source;
      end if;
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (Source : in out UXString; From : Positive; Through : Natural) is
   begin
      Source := Delete (Source, From, Through);
   end Delete;

   ----------
   -- Trim --
   ----------

   function Trim (Source : UXString; Side : Trim_End) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := Trim (Source.Chars, Side);
      end return;
   end Trim;

   ----------
   -- Trim --
   ----------

   procedure Trim (Source : in out UXString; Side : Trim_End) is
   begin
      Source := Trim (Source, Side);
   end Trim;

   ----------
   -- Trim --
   ----------

   function Trim (Source : UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := Trim (Source.Chars, Left, Right);
      end return;
   end Trim;

   ----------
   -- Trim --
   ----------

   procedure Trim (Source : in out UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set) is
   begin
      Source := Trim (Source, Left, Right);
   end Trim;

   ----------
   -- Head --
   ----------

   function Head (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString is
      Len : constant Natural := Source.Length;
   begin
      if Count > Len then
         return Source & (Count - Len) * Pad;
      else
         return Source.Slice (Source.First, Count);
      end if;
   end Head;

   ----------
   -- Head --
   ----------

   procedure Head (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) is
   begin
      Source := Head (Source, Count, Pad);
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString is
      Len : constant Natural := Source.Length;
   begin
      if Count > Len then
         return Source & (Count - Len) * Pad;
      else
         return Source.Slice (Len - Count + 1, Len);
      end if;
   end Tail;

   ----------
   -- Tail --
   ----------

   procedure Tail (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) is
   begin
      Source := Tail (Source, Count, Pad);
   end Tail;

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : UXString) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := Left * Right.Chars;
      end return;
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Unicode_Character) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := Left * Right;
      end return;
   end "*";

   ----------------------------
   -- Equal_Case_Insensitive --
   ----------------------------

   function Equal_Case_Insensitive (Left, Right : UXString) return Boolean is
   begin
      return To_Lower (Left) = To_Lower (Right);
   end Equal_Case_Insensitive;

   ---------------------------
   -- Less_Case_Insensitive --
   ---------------------------

   function Less_Case_Insensitive (Left, Right : UXString) return Boolean is
   begin
      return To_Lower (Left) < To_Lower (Right);
   end Less_Case_Insensitive;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Item : UXString) return UXString is
   begin
      return From_Unicode (To_Lower (To_Wide_Wide_String (Item.Chars)));
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (Item : UXString) return UXString is
   begin
      return From_Unicode (To_Upper (To_Wide_Wide_String (Item.Chars)));
   end To_Upper;

   --------------
   -- To_Basic --
   --------------

   function To_Basic (Item : Wide_Wide_Character) return Wide_Wide_Character is
     (Wide_Wide_Character'Val (GNAT.UTF_32.UTF_32_To_Basic (Wide_Wide_Character'Pos (Item))));

   function To_Basic (Item : Wide_Wide_String) return Wide_Wide_String is
      Result : Wide_Wide_String (Item'Range);
   begin
      for J in Result'Range loop
         Result (J) := To_Basic (Item (J));
      end loop;
      return Result;
   end To_Basic;

   function To_Basic (Item : UXString) return UXString is
   begin
      return From_Unicode (To_Basic (To_Wide_Wide_String (Item.Chars)));
   end To_Basic;

   --------------
   -- Contains --
   --------------

   function Contains (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return Boolean
   is
   begin
      if Sensitivity = Sensitive then
         return Source.Index (Pattern) > 0;
      else
         return Source.Index (Pattern, Forward, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Lower_Case_Map) > 0;
      end if;
   end Contains;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With
     (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return Boolean
   is
   begin
      if Sensitivity = Sensitive then
         return Source.Index (Pattern) = Source.Last - Pattern.Length + 1;
      else
         return
           Source.Index (Pattern, Forward, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Lower_Case_Map) =
           Source.Last - Pattern.Length + 1;
      end if;
   end Ends_With;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With
     (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return Boolean
   is
   begin
      if Sensitivity = Sensitive then
         return Source.Index (Pattern) = 1;
      else
         return Source.Index (Pattern, Forward, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Lower_Case_Map) = 1;
      end if;
   end Starts_With;

   --------------
   -- Is_Lower --
   --------------

   function Is_Lower (Source : UXString) return Boolean is
   begin
      return Source = Source.To_Lower;
   end Is_Lower;

   --------------
   -- Is_Upper --
   --------------

   function Is_Upper (Source : UXString) return Boolean is
   begin
      return Source = Source.To_Upper;
   end Is_Upper;

   --------------
   -- Is_Basic --
   --------------

   function Is_Basic (Source : UXString) return Boolean is
   begin
      return Source = Source.To_Basic;
   end Is_Basic;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Source : UXString) return Boolean is
   begin
      return Source = Null_UXString;
   end Is_Empty;

   ------------
   -- Remove --
   ------------

   function Remove
     (Source : UXString; Pattern : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive) return UXString
   is
   begin
      return Source.Remove (From_Unicode (Pattern), Sensitivity);
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove (Source : in out UXString; Pattern : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive)
   is
   begin
      Source := Remove (Source, Pattern, Sensitivity);
   end Remove;

   ------------
   -- Remove --
   ------------

   function Remove (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return UXString
   is
      Result : UXString;
      Ind1   : Positive := Source.First;
      Ind2   : Natural  := Ind1;
   begin
      while Ind1 <= Source.Last and Ind2 > 0 loop
         if Sensitivity = Sensitive then
            Ind2 := Source.Index (Pattern, Ind1);
         else
            Ind2 :=
              Source.Index (Pattern, Ind1, Forward, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Lower_Case_Map);
         end if;
         if Ind2 > 0 then
            Result.Append (Source.Slice (Ind1, Ind2 - 1));
            Ind1 := Ind2 + Pattern.Length;
         end if;
      end loop;
      Result.Append (Source.Slice (Ind1, Source.Last));
      return Result;
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove (Source : in out UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) is
   begin
      Source := Remove (Source, Pattern, Sensitivity);
   end Remove;

   -------------
   -- Replace --
   -------------

   function Replace
     (Source : UXString; Before, After : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive) return UXString
   is
   begin
      return Source.Replace (From_Unicode (Before), From_Unicode (After), Sensitivity);
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Source : in out UXString; Before, After : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive)
   is
   begin
      Source := Replace (Source, Before, After, Sensitivity);
   end Replace;

   -------------
   -- Replace --
   -------------

   function Replace
     (Source : UXString; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive) return UXString
   is
      Result : UXString;
      Ind1   : Positive := Source.First;
      Ind2   : Natural  := Ind1;
   begin
      while Ind1 <= Source.Last and Ind2 > 0 loop
         if Sensitivity = Sensitive then
            Ind2 := Source.Index (Before, Ind1);
         else
            Ind2 := Source.Index (Before, Ind1, Forward, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Lower_Case_Map);
         end if;
         if Ind2 > 0 then
            Result.Append (Source.Slice (Ind1, Ind2 - 1));
            Result.Append (After);
            Ind1 := Ind2 + Before.Length;
         end if;
      end loop;
      Result.Append (Source.Slice (Ind1, Source.Last));
      return Result;
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace (Source : in out UXString; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive)
   is
   begin
      Source := Replace (Source, Before, After, Sensitivity);
   end Replace;

   -----------
   -- Split --
   -----------

   function Split
     (Source : UXString; Separator : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive;
     Keep_Empty_Parts : Boolean := True)
      return UXStrings.Lists.UXString_List
   is
   begin
      return Split (Source, From_Unicode (Separator), Sensitivity, Keep_Empty_Parts);
   end Split;

   -----------
   -- Split --
   -----------

   function Split
     (Source : UXString; Separator : UXString; Sensitivity : Case_Sensitivity := Sensitive;
     Keep_Empty_Parts : Boolean := True) return UXStrings.Lists.UXString_List
   is
      Result : UXStrings.Lists.UXString_List;
      Ind1   : Positive := Source.First;
      Ind2   : Natural  := Ind1;
   begin
      while Ind1 <= Source.Last and Ind2 > 0 loop
         if Sensitivity = Sensitive then
            Ind2 := Source.Index (Separator, Ind1);
         else
            Ind2 :=
              Source.Index (Separator, Ind1, Forward, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Lower_Case_Map);
         end if;
         if Ind2 > 0 then
            if Ind1 < Ind2 - 1 or Keep_Empty_Parts then
               Result.Append (Source.Slice (Ind1, Ind2 - 1));
            end if;
            Ind1 := Ind2 + Separator.Length;
         end if;
      end loop;
      if Ind1 <Source.Last or Keep_Empty_Parts then
         Result.Append (Source.Slice (Ind1, Source.Last));
      end if;
      return Result;
   end Split;

   -----------
   -- Split --
   -----------

   function Split
     (Source : UXString; Separator : Wide_Wide_Character_Set; Test : Membership := Inside;
     Keep_Empty_Parts : Boolean := True) return UXStrings.Lists.UXString_List
   is
      Result : UXStrings.Lists.UXString_List;
      Ind1   : Positive := Source.First;
      Ind2   : Natural  := Ind1;
   begin
      while Ind1 <= Source.Last and Ind2 > 0 loop
         Ind2 := Source.Index (Separator, Ind1, Test);
         if Ind2 > 0 then
            if Ind1 < Ind2 - 1 or Keep_Empty_Parts then
               Result.Append (Source.Slice (Ind1, Ind2 - 1));
            end if;
            Ind1 := Ind2 + 1;
         end if;
      end loop;
            if Ind1 <Source.Last or Keep_Empty_Parts then
         Result.Append (Source.Slice (Ind1, Source.Last));
         end if;
      return Result;
   end Split;

end UXStrings;
