-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings2.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString implementation.
-- NOTES                        : Ada 2022
--
-- COPYRIGHT                    : (c) Pascal Pignard 2024
-- LICENCE                      : CeCILL-C (https://cecill.info)
-- CONTACT                      : http://blady.chez.com
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;                          use Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Fixed;                use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings; use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Characters.Conversions;                 use Ada.Characters.Conversions;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Characters.Handling;          use Ada.Wide_Wide_Characters.Handling;
with GNAT.UTF_32;
with Strings_Edit.UTF8;                          use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Handling;                 use Strings_Edit.UTF8.Handling;

package body UXStrings is

   -- Missing subroutines in Strings_Edit as Wide_Wide_XX isn't Ada 95

   function To_UTF8 (Value : Wide_Wide_Character) return String is
      Result  : String (1 .. 4);
      Pointer : Integer := Result'First;
   begin
      Put (Result, Pointer, UTF8_Code_Point (Wide_Wide_Character'Pos (Value)));
      return Result (Result'First .. Pointer - 1);
   end To_UTF8;

   function To_UTF8 (Value : Wide_Wide_String) return String is
      Result  : String (1 .. Value'Length * 3);
      Pointer : Integer := Result'First;
   begin
      for Item in Value'Range loop
         Put (Result, Pointer, UTF8_Code_Point (Wide_Wide_Character'Pos (Value (Item))));
      end loop;
      return Result (Result'First .. Pointer - 1);
   end To_UTF8;

   function To_Wide_Wide_String (Value : String) return Wide_Wide_String is
      Result : Wide_Wide_String (1 .. Value'Length);
      To     : Integer := 1;
      From   : Integer := Value'First;
      Code   : UTF8_Code_Point;
   begin
      while From <= Value'Last loop
         Get (Value, From, Code);
         Result (To) := Wide_Wide_Character'Val (Code);
         To          := To + 1;
      end loop;
      return Result (Result'First .. To - 1);
   end To_Wide_Wide_String;

   function To_ASCII (Value : String; Substitute : Character) return String is
      Result  : String (1 .. Value'Length);
      Pointer : Integer := Value'First;
      Index   : Integer := Result'First;
      Item    : UTF8_Code_Point;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Item);
         if Item > 16#7F# then
            Result (Index) := Substitute;
         else
            Result (Index) := Character'Val (Item);
         end if;
         Index := Index + 1;
      end loop;
      return Result (Result'First .. Index - 1);
   end To_ASCII;

   -- Encoding Scheme cross correspondance

   ---------------------
   -- To_UTF_Encoding --
   ---------------------

   function To_UTF_Encoding (Scheme : Encoding_Scheme) return Ada.Strings.UTF_Encoding.Encoding_Scheme is
      Convert : constant array (Encoding_Scheme) of Ada.Strings.UTF_Encoding.Encoding_Scheme :=
        (Ada.Strings.UTF_Encoding.UTF_8, Ada.Strings.UTF_Encoding.UTF_8, Ada.Strings.UTF_Encoding.UTF_8,
         Ada.Strings.UTF_Encoding.UTF_16BE, Ada.Strings.UTF_Encoding.UTF_16LE);
   begin
      return Convert (Scheme);
   end To_UTF_Encoding;

   -- Memory management

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation (UTF_8_Character_Array, UTF_8_Characters_Access);

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out UXString) is
   begin
      if Object.Chars /= null then
         Object.Chars := new UTF_8_Character_Array'(Object.Chars.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out UXString) is
   begin
      if Object.Chars /= null then
         if Object.Finalized'Valid and then not Object.Finalized then
            Free (Object.Chars);
            Object.Finalized := True;
         end if;
      end if;
   end Finalize;

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
      UTF_8_Character_Array'Write (Stream, Item.Chars.all);
   end UXString_Write;

   ------------------
   -- Bounded_Move --
   ------------------

   procedure Bounded_Move (Source : in out UXString; Target : out UXString; Max : Natural; Last : out Natural) is
      Item    : UTF8_Code_Point;
      Pointer : Integer := Source.Chars'First;
      Count   : Natural := 0;
   begin
      if Source.Full_ASCII then
         Count := Natural'Min (Source.Chars'Length, Max);
      else
         while Pointer <= Source.Chars'First + Max - 1 and Pointer <= Source.Chars'Last loop
            Get (Source.Chars.all, Pointer, Item);
            Count := Count + 1;
         end loop;
      end if;
      Target := Source.Slice (1, Count);
      Delete (Source, 1, Count);
      Last := Target.Chars.all'Length;
   end Bounded_Move;

   -- UXStrings API implementation

   ------------
   -- Length --
   ------------

   function Length (Source : UXString) return Natural is
   begin
      if Source.Full_ASCII then
         return Source.Chars'Length;
      else
         return Length (Source.Chars.all);
      end if;
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
      return Source.Full_ASCII or else Unicode_Character'Pos (Source (Index)) < 16#80#;
   end Is_ASCII;

   --------------
   -- Is_ASCII --
   --------------

   function Is_ASCII (Source : UXString) return Boolean is
   begin
      return Source.Full_ASCII;
   end Is_ASCII;

   ---------------
   -- Get_ASCII --
   ---------------

   function Get_ASCII
     (Source : UXString; Index : Positive; Substitute : in ASCII_Character := Q_L) return ASCII_Character
   is
      Item    : UTF8_Code_Point;
      Pointer : Integer := Source.Chars'First;
   begin
      if Source.Full_ASCII then
         return Source.Chars (Pointer + Index - 1);
      else
         Skip (Source.Chars.all, Pointer, Index - 1);
         Get (Source.Chars.all, Pointer, Item);
         if Item > 16#7F# then
            return Substitute;
         else
            return ASCII_Character'Val (Item);
         end if;
      end if;
   end Get_ASCII;

   --------------
   -- To_ASCII --
   --------------

   function To_ASCII (Source : UXString; Substitute : in ASCII_Character := Q_L) return ASCII_Character_Array is
   begin
      if Source.Full_ASCII then
         return Source.Chars.all;
      else
         return To_ASCII (Source.Chars.all, Substitute);
      end if;
   end To_ASCII;

   ----------------
   -- From_ASCII --
   ----------------

   function From_ASCII (Item : ASCII_Character) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'((1 => Item)), Full_ASCII => True,
         others                                 => <>);
   end From_ASCII;

   ----------------
   -- From_ASCII --
   ----------------

   function From_ASCII (Source : ASCII_Character_Array) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(Source), Full_ASCII => True,
         others                                 => <>);
   end From_ASCII;

   ----------------
   -- Is_Latin_1 --
   ----------------

   function Is_Latin_1 (Source : UXString; Index : Positive) return Boolean is
   begin
      return Source.Full_ASCII or else Unicode_Character'Pos (Source (Index)) < 16#1_00#;
   end Is_Latin_1;

   ----------------
   -- Is_Latin_1 --
   ----------------

   function Is_Latin_1 (Source : UXString) return Boolean is
   begin
      return Source.Full_ASCII or else (for all Item of Source => Unicode_Character'Pos (Item) < 16#1_00#);
   end Is_Latin_1;

   -----------------
   -- Get_Latin_1 --
   -----------------

   function Get_Latin_1
     (Source : UXString; Index : Positive; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character
   is
      Item    : UTF8_Code_Point;
      Pointer : Integer := Source.Chars'First;
   begin
      if Source.Full_ASCII then
         return Source.Chars (Pointer + Index - 1);
      else
         Skip (Source.Chars.all, Pointer, Index - 1);
         Get (Source.Chars.all, Pointer, Item);
         if Item > 16#FF# then
            return Substitute;
         else
            return Latin_1_Character'Val (Item);
         end if;
      end if;
   end Get_Latin_1;

   ----------------
   -- To_Latin_1 --
   ----------------

   function To_Latin_1 (Source : UXString; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character_Array
   is
   begin
      if Source.Full_ASCII then
         return Source.Chars.all;
      else
         return To_String (Source.Chars.all, Substitute);
      end if;
   end To_Latin_1;

   ------------------
   -- From_Latin_1 --
   ------------------

   function From_Latin_1 (Item : Latin_1_Character) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(To_UTF8 (Item)),
         Full_ASCII                             => Latin_1_Character'Pos (Item) < 16#80#, others => <>);
   end From_Latin_1;

   ------------------
   -- From_Latin_1 --
   ------------------

   function From_Latin_1 (Source : Latin_1_Character_Array) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(To_UTF8 (Source)),
         Full_ASCII                             => (for all Item of Source => Item in ASCII_Character), others => <>);
   end From_Latin_1;

   ------------
   -- Is_BMP --
   ------------

   function Is_BMP (Source : UXString; Index : Positive) return Boolean is
   begin
      return Source.Full_ASCII or else Unicode_Character'Pos (Source (Index)) < 16#1_0000#;
   end Is_BMP;

   ------------
   -- Is_BMP --
   ------------

   function Is_BMP (Source : UXString) return Boolean is
   begin
      return Source.Full_ASCII or else (for all Item of Source => Unicode_Character'Pos (Item) < 16#1_0000#);
   end Is_BMP;

   -------------
   -- Get_BMP --
   -------------

   function Get_BMP (Source : UXString; Index : Positive; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character
   is
      Item    : UTF8_Code_Point;
      Pointer : Integer := Source.Chars'First;
   begin
      if Source.Full_ASCII then
         return To_Wide_Character (Source.Chars (Pointer + Index - 1));
      else
         Skip (Source.Chars.all, Pointer, Index - 1);
         Get (Source.Chars.all, Pointer, Item);
         if Item > 16#FFFF# then
            return Substitute;
         else
            return BMP_Character'Val (Item);
         end if;
      end if;
   end Get_BMP;

   ------------
   -- To_BMP --
   ------------

   function To_BMP (Source : UXString; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character_Array is
   begin
      if Source.Full_ASCII then
         return Ada.Characters.Conversions.To_Wide_String (Source.Chars.all);
      else
         return To_Wide_String (Source.Chars.all, Substitute);
      end if;
   end To_BMP;

   --------------
   -- From_BMP --
   --------------

   function From_BMP (Item : BMP_Character) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(To_UTF8 (Item)),
         Full_ASCII                             => BMP_Character'Pos (Item) < 16#80#, others => <>);
   end From_BMP;

   --------------
   -- From_BMP --
   --------------

   function From_BMP (Source : BMP_Character_Array) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(To_UTF8 (Source)),
         Full_ASCII => (for all Item of Source => BMP_Character'Pos (Item) < 16#80#), others => <>);
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
      Item    : UTF8_Code_Point;
      Pointer : Integer := Source.Chars'First;
   begin
      if Source.Full_ASCII then
         return To_Wide_Wide_Character (Source.Chars (Pointer + Index - 1));
      else
         Skip (Source.Chars.all, Pointer, Index - 1);
         Get (Source.Chars.all, Pointer, Item);
         return Unicode_Character'Val (Item);
      end if;
   end Get_Unicode;

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (Source : UXString) return Unicode_Character_Array is
   begin
      if Source.Full_ASCII then
         return Ada.Characters.Conversions.To_Wide_Wide_String (Source.Chars.all);
      else
         return To_Wide_Wide_String (Source.Chars.all);
      end if;
   end To_Unicode;

   ------------------
   -- From_Unicode --
   ------------------

   function From_Unicode (Item : Unicode_Character) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(To_UTF8 (Item)),
         Full_ASCII                             => Unicode_Character'Pos (Item) < 16#80#, others => <>);
   end From_Unicode;

   ------------------
   -- From_Unicode --
   ------------------

   function From_Unicode (Source : Unicode_Character_Array) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(To_UTF8 (Source)),
         Full_ASCII => (for all Item of Source => Unicode_Character'Pos (Item) < 16#80#), others => <>);
   end From_Unicode;

   --------------
   -- To_UTF_8 --
   --------------

   function To_UTF_8 (Source : UXString; Output_BOM : Boolean := False) return UTF_8_Character_Array is
   begin
      if not Output_BOM then
         return Source.Chars.all;
      else
         return UTF_8_Character_Array (Ada.Strings.UTF_Encoding.BOM_8) & Source.Chars.all;
      end if;
   end To_UTF_8;

   ----------------
   -- From_UTF_8 --
   ----------------

   function From_UTF_8 (Source : UTF_8_Character_Array) return UXString is
      Start : constant Natural :=
        (if Index (Source, Ada.Strings.UTF_Encoding.BOM_8) = Source'First then
           Source'First + Ada.Strings.UTF_Encoding.BOM_8'Length
         else Source'First);
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(Source (Start .. Source'Last)),
         Full_ASCII => (for all Item of Source (Start .. Source'Last) => Item in ASCII_Character), others => <>);
   end From_UTF_8;

   ---------------
   -- To_UTF_16 --
   ---------------

   function To_UTF_16
     (Source : UXString; Output_Scheme : UTF_16_Encoding_Scheme; Output_BOM : Boolean := False)
      return UTF_16_Character_Array
   is
   begin
      if Source.Full_ASCII then
         return Ada.Strings.UTF_Encoding.Strings.Encode (Source.Chars.all, To_UTF_Encoding (Output_Scheme), Output_BOM);
      else
         return
           Ada.Strings.UTF_Encoding.Conversions.Convert
             (Source.Chars.all, Ada.Strings.UTF_Encoding.UTF_8, To_UTF_Encoding (Output_Scheme), Output_BOM);
      end if;
   end To_UTF_16;

   -----------------
   -- From_UTF_16 --
   -----------------

   function From_UTF_16 (Source : UTF_16_Character_Array; Input_Scheme : UTF_16_Encoding_Scheme) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with
         Chars =>
           new UTF_8_Character_Array'
             (Ada.Strings.UTF_Encoding.Conversions.Convert
                (Source, To_UTF_Encoding (Input_Scheme), Ada.Strings.UTF_Encoding.UTF_8)),
         Full_ASCII => (for all Item of Source => Item in ASCII_Character), others => <>);
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
      Saved_Access : UTF_8_Characters_Access := Source.Chars;
   begin
      Source.Chars      := new UTF_8_Character_Array'(Source.Chars.all & New_Item.Chars.all);
      Source.Full_ASCII := Source.Full_ASCII and New_Item.Full_ASCII;
      if Saved_Access /= null then
         Free (Saved_Access);
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Source : in out UXString; New_Item : Unicode_Character) is
      Saved_Access : UTF_8_Characters_Access := Source.Chars;
   begin
      Source.Chars      := new UTF_8_Character_Array'(Source.Chars.all & To_UTF8 (New_Item));
      Source.Full_ASCII := Source.Full_ASCII and Unicode_Character'Pos (New_Item) < 16#80#;
      if Saved_Access /= null then
         Free (Saved_Access);
      end if;
   end Append;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Source : in out UXString; New_Item : UXString) is
      Saved_Access : UTF_8_Characters_Access := Source.Chars;
   begin
      Source.Chars      := new UTF_8_Character_Array'(New_Item.Chars.all & Source.Chars.all);
      Source.Full_ASCII := Source.Full_ASCII and New_Item.Full_ASCII;
      if Saved_Access /= null then
         Free (Saved_Access);
      end if;
   end Prepend;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Source : in out UXString; New_Item : Unicode_Character) is
      Saved_Access : UTF_8_Characters_Access := Source.Chars;
   begin
      Source.Chars      := new UTF_8_Character_Array'(To_UTF8 (New_Item) & Source.Chars.all);
      Source.Full_ASCII := Source.Full_ASCII and Unicode_Character'Pos (New_Item) < 16#80#;
      if Saved_Access /= null then
         Free (Saved_Access);
      end if;
   end Prepend;

   ---------
   -- "&" --
   ---------

   function "&" (Left : UXString; Right : UXString) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(Left.Chars.all & Right.Chars.all),
         Full_ASCII                             => Left.Full_ASCII and Right.Full_ASCII, others => <>);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : UXString; Right : Unicode_Character) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(Left.Chars.all & To_UTF8 (Right)),
         Full_ASCII => Left.Full_ASCII and Unicode_Character'Pos (Right) < 16#80#, others => <>);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Unicode_Character; Right : UXString) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(To_UTF8 (Left) & Right.Chars.all),
         Full_ASCII => Unicode_Character'Pos (Left) < 16#80# and Right.Full_ASCII, others => <>);
   end "&";

   -------------------
   -- Replace_ASCII --
   -------------------

   procedure Replace_ASCII (Source : in out UXString; Index : Positive; By : ASCII_Character) is
   begin
      if Source.Full_ASCII then
         Source.Chars (Source.Chars'First + Index - 1) := By;
      else
         Source := Slice (Source, 1, Index - 1) & From_ASCII (By) & Slice (Source, Index + 1, Length (Source));
      end if;
   end Replace_ASCII;

   ---------------------
   -- Replace_Latin_1 --
   ---------------------

   procedure Replace_Latin_1 (Source : in out UXString; Index : Positive; By : Latin_1_Character) is
   begin
      if Source.Full_ASCII and By in ASCII_Character then
         Source.Chars (Source.Chars'First + Index - 1) := By;
      else
         Source := Slice (Source, 1, Index - 1) & From_Latin_1 (By) & Slice (Source, Index + 1, Length (Source));
      end if;
   end Replace_Latin_1;

   -----------------
   -- Replace_BMP --
   -------*---------

   procedure Replace_BMP (Source : in out UXString; Index : Positive; By : BMP_Character) is
   begin
      if Source.Full_ASCII and BMP_Character'Pos (By) < 16#80# then
         Source.Chars (Source.Chars'First + Index - 1) := To_Character (By);
      else
         Source := Slice (Source, 1, Index - 1) & From_BMP (By) & Slice (Source, Index + 1, Length (Source));
      end if;
   end Replace_BMP;

   ---------------------
   -- Replace_Unicode --
   ---------------------

   procedure Replace_Unicode (Source : in out UXString; Index : Positive; By : Unicode_Character) is
   begin
      if Source.Full_ASCII and Unicode_Character'Pos (By) < 16#80# then
         Source.Chars (Source.Chars'First + Index - 1) := To_Character (By);
      else
         Source := Slice (Source, 1, Index - 1) & From_Unicode (By) & Slice (Source, Index + 1, Length (Source));
      end if;
   end Replace_Unicode;

   -----------
   -- Slice --
   -----------

   function Slice (Source : UXString; Low : Positive; High : Integer) return UXString is
      Pointer1 : Integer := Source.Chars'First;
      Pointer2 : Integer;
   begin
      if Source.Full_ASCII then
         return
           (Ada.Finalization.Controlled with
            Chars =>
              new UTF_8_Character_Array'(Source.Chars (Source.Chars'First + Low - 1 .. Source.Chars'First + High - 1)),
            Full_ASCII =>
              (for all Item of Source.Chars (Source.Chars'First + Low - 1 .. Source.Chars'First + High - 1) =>
                 Item in ASCII_Character),
            others => <>);
      else
         if Low <= High then
            Skip (Source.Chars.all, Pointer1, Low - 1);
            Pointer2 := Pointer1;
            Skip (Source.Chars.all, Pointer2, High - Low + 1);
            return
              (Ada.Finalization.Controlled with
               Chars      => new UTF_8_Character_Array'(Source.Chars (Pointer1 .. Pointer2 - 1)),
               Full_ASCII => (for all Item of Source.Chars (Pointer1 .. Pointer2 - 1) => Item in ASCII_Character),
               others     => <>);
         else
            return Null_UXString;
         end if;
      end if;
   end Slice;

   -----------
   -- Slice --
   -----------

   procedure Slice (Source : UXString; Target : out UXString; Low : Positive; High : Integer) is
   begin
      Target := Slice (Source, Low, High);
   end Slice;

   ---------
   -- "=" --
   ---------

   function "=" (Left : UXString; Right : UXString) return Boolean is
   begin
      return Left.Chars.all = Right.Chars.all;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left : UXString; Right : UXString) return Boolean is
   begin
      return Left.Chars.all < Right.Chars.all;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left : UXString; Right : UXString) return Boolean is
   begin
      return Left.Chars.all <= Right.Chars.all;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left : UXString; Right : UXString) return Boolean is
   begin
      return Left.Chars.all > Right.Chars.all;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left : UXString; Right : UXString) return Boolean is
   begin
      return Left.Chars.all >= Right.Chars.all;
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
      return Index (Decode (Source.Chars.all), Decode (Pattern.Chars.all), From, Going, Mapping);
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural
   is
   begin
      return Index (Decode (Source.Chars.all), Decode (Pattern.Chars.all), From, Going, Mapping);
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
   begin
      return Index (Decode (Source.Chars.all), Set, From, Test, Going);
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
      return Count (Decode (Source.Chars.all), Decode (Pattern.Chars.all), Mapping);
   end Count;

   -----------
   -- Count --
   -----------

   function Count (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return Natural
   is
   begin
      return Count (Decode (Source.Chars.all), Decode (Pattern.Chars.all), Mapping);
   end Count;

   -----------
   -- Count --
   -----------

   function Count (Source : UXString; Set : Wide_Wide_Character_Set) return Natural is
   begin
      return Count (Decode (Source.Chars.all), Set);
   end Count;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source :     UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership; First : out Positive;
      Last   : out Natural)
   is
   begin
      Find_Token (Decode (Source.Chars.all), Set, From, Test, First, Last);

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
      return From_UTF_8 (Encode (Translate (Decode (Source.Chars.all), Mapping)));
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
      return From_UTF_8 (Encode (Translate (Decode (Source.Chars.all), Mapping)));
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
      if Source.Full_ASCII and By.Full_ASCII then
         if Low <= High then
            return
              (Ada.Finalization.Controlled with
               Chars =>
                 new UTF_8_Character_Array'
                   (Source.Chars (Source.Chars'First .. Source.Chars'First + Low - 2) & By.Chars.all &
                    Source.Chars (Source.Chars'First + High .. Source.Chars'Last)),
               Full_ASCII => True, others => <>);
         else
            return
              (Ada.Finalization.Controlled with
               Chars =>
                 new UTF_8_Character_Array'
                   (Source.Chars (Source.Chars'First .. Source.Chars'First + Low - 2) & By.Chars.all &
                    Source.Chars (Source.Chars'First + Low - 1 .. Source.Chars'Last)),
               Full_ASCII => True, others => <>);
         end if;
      else
         if Low <= High then
            return Source.Slice (Source.First, Low - 1) & By & Source.Slice (High + 1, Source.Last);
         else
            return Insert (Source, Low, By);
         end if;
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
      if Source.Full_ASCII and New_Item.Full_ASCII then
         return
           (Ada.Finalization.Controlled with
            Chars =>
              new UTF_8_Character_Array'
                (Source.Chars (Source.Chars'First .. Source.Chars'First + Before - 2) & New_Item.Chars.all &
                 Source.Chars (Source.Chars'First + Before - 1 .. Source.Chars'Last)),
            Full_ASCII => True, others => <>);
      else
         return Source.Slice (Source.First, Before - 1) & New_Item & Source.Slice (Before, Source.Last);
      end if;
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
      if Source.Full_ASCII and New_Item.Full_ASCII then
         return
           (Ada.Finalization.Controlled with
            Chars =>
              new UTF_8_Character_Array'
                (Source.Chars (Source.Chars'First .. Source.Chars'First + Position - 2) & New_Item.Chars.all &
                 Source.Chars (Source.Chars'First + Position + New_Item.Chars'Length - 1 .. Source.Chars'Last)),
            Full_ASCII => True, others => <>);
      else
         return
           Replace_Slice
             (Source, Position, Position + Natural'Min (Source.Length - Position + 1, New_Item.Length) - 1, New_Item);
      end if;
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
         if Source.Full_ASCII then
            return
              (Ada.Finalization.Controlled with
               Chars =>
                 new UTF_8_Character_Array'
                   (Source.Chars (Source.Chars'First .. Source.Chars'First + From - 2) &
                    Source.Chars (Source.Chars'First + Through .. Source.Chars'Last)),
               Full_ASCII => True, others => <>);
         else
            return Replace_Slice (Source, From, Through, Null_UXString);
         end if;
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
      if Source.Full_ASCII then
         return
           (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(Trim (Source.Chars.all, Side)),
            Full_ASCII                             => True, others => <>);
      else
         return From_UTF_8 (Encode (Trim (Decode (Source.Chars.all), Side)));
      end if;
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
      return From_UTF_8 (Encode (Trim (Decode (Source.Chars.all), Left, Right)));
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
         return
           (Ada.Finalization.Controlled with
            Chars      => new UTF_8_Character_Array'(Source.Chars.all & (Count - Len) * (To_UTF8 (Pad))),
            Full_ASCII => Source.Full_ASCII and Unicode_Character'Pos (Pad) < 16#80#, others => <>);
      else
         if Source.Full_ASCII then
            return
              (Ada.Finalization.Controlled with
               Chars => new UTF_8_Character_Array'(Source.Chars (Source.Chars'First .. Source.Chars'First + Count - 1)),
               Full_ASCII => True, others => <>);
         else
            return Source.Slice (Source.First, Count);
         end if;
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
         return
           (Ada.Finalization.Controlled with
            Chars      => new UTF_8_Character_Array'(Source.Chars.all & (Count - Len) * (To_UTF8 (Pad))),
            Full_ASCII => Source.Full_ASCII and Unicode_Character'Pos (Pad) < 16#80#, others => <>);
      else
         if Source.Full_ASCII then
            return
              (Ada.Finalization.Controlled with
               Chars => new UTF_8_Character_Array'(Source.Chars (Source.Chars'Last - Count + 1 .. Source.Chars'Last)),
               Full_ASCII => True, others => <>);
         else
            return Source.Slice (Len - Count + 1, Len);
         end if;
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
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(Left * Right.Chars.all),
         Full_ASCII                             => Right.Full_ASCII, others => <>);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Unicode_Character) return UXString is
   begin
      return
        (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array'(Left * (To_UTF8 (Right))),
         Full_ASCII                             => Unicode_Character'Pos (Right) < 16#80#, others => <>);
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
      return From_UTF_8 (Encode (To_Lower (Decode (Item.Chars.all))));
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (Item : UXString) return UXString is
   begin
      return From_UTF_8 (Encode (To_Upper (Decode (Item.Chars.all))));
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
      return From_UTF_8 (Encode (To_Basic (Decode (Item.Chars.all))));
   end To_Basic;

   -------------
   -- Replace --
   -------------

   procedure Replace (Source : in out UXString; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive)
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
      Source := Result;
   end Replace;

end UXStrings;
