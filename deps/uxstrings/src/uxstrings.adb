-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString implementation.
-- NOTES                        : Ada 202x
--
-- COPYRIGHT                    : (c) Pascal Pignard 2021
-- LICENCE                      : CeCILL V2.1 (https://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
--  with Ada.Strings.UTF_Encoding.Conversions;
with STUTEN.SUEnco; -- Fix an issue in UTF-16 to UTF8 conversion
with Ada.Unchecked_Deallocation;
with Strings_Edit.UTF8;          use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Handling; use Strings_Edit.UTF8.Handling;

package body UXStrings is

   -- Missing subroutines in Strings_Edit as Wide_Wide_XX isn't Ada 95

   function To_UTF8 (Value : Wide_Wide_Character) return String is
      Result  : String (1 .. 4);
      Pointer : Integer := Result'First;
   begin
      Put (Result, Pointer, UTF8_Code_Point (Wide_Wide_Character'Pos (Value)));
      return Result (1 .. Pointer - 1);
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
      return Result (1 .. To - 1);
   end To_Wide_Wide_String;

   -- Encoding Scheme cross correspondance

--     To_UTF_Encoding : constant array (Encoding_Scheme) of Ada.Strings.UTF_Encoding.Encoding_Scheme :=
--       (Ada.Strings.UTF_Encoding.UTF_8, Ada.Strings.UTF_Encoding.UTF_8, Ada.Strings.UTF_Encoding.UTF_16BE,
--        Ada.Strings.UTF_Encoding.UTF_16LE);
   To_UTF_Encoding : constant array (Encoding_Scheme) of STUTEN.Encoding_Scheme :=
     (STUTEN.UTF_8, STUTEN.UTF_8, STUTEN.UTF_16BE,
      STUTEN.UTF_16LE);

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
         Free (Object.Chars);
      end if;
   end Finalize;

   -- UXStrings API implementation

   ------------
   -- Length --
   ------------

   function Length (Source : UXString) return Natural is
   begin
      return Length (String (Source.Chars.all));
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
      Pointer : Integer := Source.Chars'First;
   begin
      Skip (String (Source.Chars.all), Pointer, Index);
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

   ----------------
   -- Is_Latin_1 --
   ----------------

   function Is_Latin_1 (Source : UXString; Index : Positive) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Is_Latin_1 unimplemented");
      return raise Program_Error with "Unimplemented function Is_Latin_1";
   end Is_Latin_1;

   ----------------
   -- Is_Latin_1 --
   ----------------

   function Is_Latin_1 (Source : UXString) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Is_Latin_1 unimplemented");
      return raise Program_Error with "Unimplemented function Is_Latin_1";
   end Is_Latin_1;

   -----------------
   -- Get_Latin_1 --
   -----------------

   function Get_Latin_1
     (Source : UXString; Index : Positive; Substitute : in Latin_1_Character := '¿') return Latin_1_Character
   is
      Item    : UTF8_Code_Point;
      Pointer : Integer := Source.Chars'First;
   begin
      Skip (String (Source.Chars.all), Pointer, Index - 1);
      Get (String (Source.Chars.all), Pointer, Item);
      if Item > 16#FF# then
         return Substitute;
      else
         return Latin_1_Character'val (Item);
      end if;
   end Get_Latin_1;

   ----------------
   -- To_Latin_1 --
   ----------------

   function To_Latin_1 (Source : UXString; Substitute : in Latin_1_Character := '¿') return Latin_1_Character_Array is
   begin
      return To_String (String (Source.Chars.all), Substitute);
   end To_Latin_1;

   ------------------
   -- From_Latin_1 --
   ------------------

   function From_Latin_1 (Item : Latin_1_Character) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(To_UTF8 (Item));
      end return;
   end From_Latin_1;

   ------------------
   -- From_Latin_1 --
   ------------------

   function From_Latin_1 (Source : Latin_1_Character_Array) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(To_UTF8 (Source));
      end return;
   end From_Latin_1;

   ------------
   -- Is_BMP --
   ------------

   function Is_BMP (Source : UXString; Index : Positive) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Is_BMP unimplemented");
      return raise Program_Error with "Unimplemented function Is_BMP";
   end Is_BMP;

   ------------
   -- Is_BMP --
   ------------

   function Is_BMP (Source : UXString) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Is_BMP unimplemented");
      return raise Program_Error with "Unimplemented function Is_BMP";
   end Is_BMP;

   -------------
   -- Get_BMP --
   -------------

   function Get_BMP (Source : UXString; Index : Positive; Substitute : in BMP_Character := '¿') return BMP_Character is
      Item    : UTF8_Code_Point;
      Pointer : Integer := Source.Chars'First;
   begin
      Skip (String (Source.Chars.all), Pointer, Index - 1);
      Get (String (Source.Chars.all), Pointer, Item);
      if Item > 16#FFFF# then
         return Substitute;
      else
         return BMP_Character'val (Item);
      end if;
   end Get_BMP;

   ------------
   -- To_BMP --
   ------------

   function To_BMP (Source : UXString; Substitute : in BMP_Character := '¿') return BMP_Character_Array is
   begin
      return To_Wide_String (String (Source.Chars.all), Substitute);
   end To_BMP;

   --------------
   -- From_BMP --
   --------------

   function From_BMP (Item : BMP_Character) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(To_UTF8 (Item));
      end return;
   end From_BMP;

   --------------
   -- From_BMP --
   --------------

   function From_BMP (Source : BMP_Character_Array) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(To_UTF8 (Source));
      end return;
   end From_BMP;

   ----------------
   -- Is_Unicode --
   ----------------

   function Is_Unicode (Source : UXString; Index : Positive) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Is_Unicode unimplemented");
      return raise Program_Error with "Unimplemented function Is_Unicode";
   end Is_Unicode;

   ----------------
   -- Is_Unicode --
   ----------------

   function Is_Unicode (Source : UXString) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Is_Unicode unimplemented");
      return raise Program_Error with "Unimplemented function Is_Unicode";
   end Is_Unicode;

   -----------------
   -- Get_Unicode --
   -----------------

   function Get_Unicode (Source : UXString; Index : Positive) return Unicode_Character is
      Item    : UTF8_Code_Point;
      Pointer : Integer := Source.Chars'First;
   begin
      Skip (String (Source.Chars.all), Pointer, Index - 1);
      Get (String (Source.Chars.all), Pointer, Item);
      return Unicode_Character'Val (Item);
   end Get_Unicode;

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (Source : UXString) return Unicode_Character_Array is
   begin
      return To_Wide_Wide_String (String (Source.Chars.all));
   end To_Unicode;

   ------------------
   -- From_Unicode --
   ------------------

   function From_Unicode (Item : Unicode_Character) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(To_UTF8 (Item));
      end return;
   end From_Unicode;

   ------------------
   -- From_Unicode --
   ------------------

   function From_Unicode (Source : Unicode_Character_Array) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(To_UTF8 (Source));
      end return;
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
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(Source);
      end return;
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
        --          Ada.Strings.UTF_Encoding.Conversions.Convert
--            (Source.Chars.all, Ada.Strings.UTF_Encoding.UTF_8, To_UTF_Encoding (Output_Scheme), Output_BOM);
        STUTEN.SUEnco.Convert
          (Source.Chars.all, STUTEN.UTF_8, To_UTF_Encoding (Output_Scheme), Output_BOM);
   end To_UTF_16;

   -----------------
   -- From_UTF_16 --
   -----------------

   function From_UTF_16 (Source : UTF_16_Character_Array; Input_Scheme : UTF_16_Encoding_Scheme) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars :=
           new UTF_8_Character_Array'
         --               (Ada.Strings.UTF_Encoding.Conversions.Convert
--                  (Source, To_UTF_Encoding (Input_Scheme), Ada.Strings.UTF_Encoding.UTF_8));
         (STUTEN.SUEnco.Convert
                (Source, To_UTF_Encoding (Input_Scheme), STUTEN.UTF_8));
      end return;
   end From_UTF_16;

   ---------
   -- Set --
   ---------

   procedure Set (Target : out UXString; Unicode_Source : Unicode_Character_Array) is
   begin
      Target := From_Unicode (Unicode_Source);
   end Set;

   ------------
   -- Append --
   ------------

   procedure Append (Source : in out UXString; New_Item : UXString) is
      Saved_Access : UTF_8_Characters_Access := Source.Chars;
   begin
      Source.Chars := new UTF_8_Character_Array'(Source.Chars.all & New_Item.Chars.all);
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
      Source.Chars := new UTF_8_Character_Array'(Source.Chars.all & To_UTF8 (New_Item));
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
      Source.Chars := new UTF_8_Character_Array'(New_Item.Chars.all & Source.Chars.all);
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
      Source.Chars := new UTF_8_Character_Array'(To_UTF8 (New_Item) & Source.Chars.all);
      if Saved_Access /= null then
         Free (Saved_Access);
      end if;
   end Prepend;

   ---------
   -- "&" --
   ---------

   function "&" (Left : UXString; Right : UXString) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(Left.Chars.all & Right.Chars.all);
      end return;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : UXString; Right : Unicode_Character) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(Left.Chars.all & To_UTF8 (Right));
      end return;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Unicode_Character; Right : UXString) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(To_UTF8 (Left) & Right.Chars.all);
      end return;
   end "&";

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

   function Slice (Source : UXString; Low : Positive; High : Natural) return UXString is
      Pointer1 : Integer := Source.Chars'First;
      Pointer2 : Integer;
   begin
      Skip (String (Source.Chars.all), Pointer1, Low - 1);
      Pointer2 := Pointer1;
      Skip (String (Source.Chars.all), Pointer2, High - Low + 1);
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(Source.Chars.all (Pointer1 .. Pointer2 - 1));
      end return;
   end Slice;

   -----------
   -- Slice --
   -----------

   procedure Slice (Source : UXString; Target : out UXString; Low : Positive; High : Natural) is
      Pointer1 : Integer := Source.Chars'First;
      Pointer2 : Integer;
   begin
      Skip (String (Source.Chars.all), Pointer1, Low - 1);
      Pointer2 := Pointer1;
      Skip (String (Source.Chars.all), Pointer2, High - Low + 1);
      if Target.Chars /= null then
         Free (Target.Chars);
      end if;
      Target.Chars := new UTF_8_Character_Array'(Source.Chars.all (Pointer1 .. Pointer2 - 1));
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
      return Index (Source, Pattern, 1, Going, Mapping);
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural
   is
   begin
      return Index (Source, Pattern, 1, Going, Mapping);
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership := Inside; Going : Direction := Forward)
      return Natural
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Index unimplemented");
      return raise Program_Error with "Unimplemented function Index";
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural
   is
      Pointer1 : Integer := Source.Chars'First;
      Pointer2 : Integer;
   begin
      if Source.Chars /= null and Pattern.Chars /= null then
         Skip (String (Source.Chars.all), Pointer1, From - 1);
         Pointer2 := Index (String (Source.Chars.all), String (Pattern.Chars.all), Pointer1);
         if Pointer2 > 0 then
            return Length (String (Source.Chars.all (Source.Chars'First .. Pointer2 - 1))) + 1;
         else
            return 0;
         end if;
      else
         return 0;
      end if;
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Index unimplemented");
      return raise Program_Error with "Unimplemented function Index";
   end Index;

   -----------
   -- Index --
   -----------

   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Index unimplemented");
      return raise Program_Error with "Unimplemented function Index";
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank (Source : UXString; Going : Direction := Forward) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Index_Non_Blank unimplemented");
      return raise Program_Error with "Unimplemented function Index_Non_Blank";
   end Index_Non_Blank;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank (Source : UXString; From : Positive; Going : Direction := Forward) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Index_Non_Blank unimplemented");
      return raise Program_Error with "Unimplemented function Index_Non_Blank";
   end Index_Non_Blank;

   -----------
   -- Count --
   -----------

   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity) return Natural
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Count unimplemented");
      return raise Program_Error with "Unimplemented function Count";
   end Count;

   -----------
   -- Count --
   -----------

   function Count (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return Natural
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Count unimplemented");
      return raise Program_Error with "Unimplemented function Count";
   end Count;

   -----------
   -- Count --
   -----------

   function Count (Source : UXString; Set : Wide_Wide_Character_Set) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Count unimplemented");
      return raise Program_Error with "Unimplemented function Count";
   end Count;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source :     UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership; First : out Positive;
      Last   : out Natural)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Find_Token unimplemented");
      raise Program_Error with "Unimplemented procedure Find_Token";
   end Find_Token;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership; First : out Positive; Last : out Natural)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Find_Token unimplemented");
      raise Program_Error with "Unimplemented procedure Find_Token";
   end Find_Token;

   ---------------
   -- Translate --
   ---------------

   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping) return UXString is
   begin
      pragma Compile_Time_Warning (Standard.True, "Translate unimplemented");
      return raise Program_Error with "Unimplemented function Translate";
   end Translate;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Translate unimplemented");
      raise Program_Error with "Unimplemented procedure Translate";
   end Translate;

   ---------------
   -- Translate --
   ---------------

   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString is
   begin
      pragma Compile_Time_Warning (Standard.True, "Translate unimplemented");
      return raise Program_Error with "Unimplemented function Translate";
   end Translate;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping_Function) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Translate unimplemented");
      raise Program_Error with "Unimplemented procedure Translate";
   end Translate;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice (Source : UXString; Low : Positive; High : Natural; By : UXString) return UXString is
   begin
      pragma Compile_Time_Warning (Standard.True, "Replace_Slice unimplemented");
      return raise Program_Error with "Unimplemented function Replace_Slice";
   end Replace_Slice;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice (Source : in out UXString; Low : Positive; High : Natural; By : UXString) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Replace_Slice unimplemented");
      raise Program_Error with "Unimplemented procedure Replace_Slice";
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert (Source : UXString; Before : Positive; New_Item : UXString) return UXString is
   begin
      pragma Compile_Time_Warning (Standard.True, "Insert unimplemented");
      return raise Program_Error with "Unimplemented function Insert";
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (Source : in out UXString; Before : Positive; New_Item : UXString) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Insert unimplemented");
      raise Program_Error with "Unimplemented procedure Insert";
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite (Source : UXString; Position : Positive; New_Item : UXString) return UXString is
   begin
      pragma Compile_Time_Warning (Standard.True, "Overwrite unimplemented");
      return raise Program_Error with "Unimplemented function Overwrite";
   end Overwrite;

   ---------------
   -- Overwrite --
   ---------------

   procedure Overwrite (Source : in out UXString; Position : Positive; New_Item : UXString) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Overwrite unimplemented");
      raise Program_Error with "Unimplemented procedure Overwrite";
   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete (Source : UXString; From : Positive; Through : Natural) return UXString is
   begin
      pragma Compile_Time_Warning (Standard.True, "Delete unimplemented");
      return raise Program_Error with "Unimplemented function Delete";
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (Source : in out UXString; From : Positive; Through : Natural) is
      Pointer1     : Integer                 := Source.Chars'First;
      Pointer2     : Integer;
      Saved_Access : UTF_8_Characters_Access := Source.Chars;
   begin
      Skip (String (Source.Chars.all), Pointer1, From - 1);
      Pointer2 := Pointer1;
      Skip (String (Source.Chars.all), Pointer2, Through - From + 1);
      Source.Chars := new UTF_8_Character_Array'(Delete (String (Source.Chars.all), Pointer1, Pointer2 - 1));
      if Saved_Access /= null then
         Free (Saved_Access);
      end if;
   end Delete;

   ----------
   -- Trim --
   ----------

   function Trim (Source : UXString; Side : Trim_End) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(Trim (String (Source.Chars.all), Side));
      end return;
   end Trim;

   ----------
   -- Trim --
   ----------

   procedure Trim (Source : in out UXString; Side : Trim_End) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Trim unimplemented");
      raise Program_Error with "Unimplemented procedure Trim";
   end Trim;

   ----------
   -- Trim --
   ----------

   function Trim (Source : UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set) return UXString is
   begin
      pragma Compile_Time_Warning (Standard.True, "Trim unimplemented");
      return raise Program_Error with "Unimplemented function Trim";
   end Trim;

   ----------
   -- Trim --
   ----------

   procedure Trim (Source : in out UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Trim unimplemented");
      raise Program_Error with "Unimplemented procedure Trim";
   end Trim;

   ----------
   -- Head --
   ----------

   function Head (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString is
   begin
      pragma Compile_Time_Warning (Standard.True, "Head unimplemented");
      return raise Program_Error with "Unimplemented function Head";
   end Head;

   ----------
   -- Head --
   ----------

   procedure Head (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Head unimplemented");
      raise Program_Error with "Unimplemented procedure Head";
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString is
      Len : constant Positive := Source.Length;
   begin
      if Count > Len then
         return UXS : UXString do
            UXS.Chars := new UTF_8_Character_Array'(Source.Chars.all & (Count - Len - 1) * (To_UTF8 (Pad)));
         end return;
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
         UXS.Chars := new UTF_8_Character_Array'(Left * Right.Chars.all);
      end return;
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Unicode_Character) return UXString is
   begin
      return UXS : UXString do
         UXS.Chars := new UTF_8_Character_Array'(Left * (To_UTF8 (Right)));
      end return;
   end "*";

end UXStrings;
