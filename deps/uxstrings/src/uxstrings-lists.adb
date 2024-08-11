-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-lists.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString lists implementation.
-- NOTES                        : Ada 2022
--
-- COPYRIGHT                    : (c) Pascal Pignard 2024
-- LICENCE                      : CeCILL-C (https://cecill.info)
-- CONTACT                      : http://blady.chez.com
-------------------------------------------------------------------------------

package body UXStrings.Lists is

   use UXString_Lists;

   function Constant_Reference (Container : aliased UXString_List; Index : Positive) return Constant_Reference_Type is
     (Constant_Reference (Vector (Container), Index));
   function Reference (Container : aliased in out UXString_List; Index : Positive) return Reference_Type is
     (Reference (Vector (Container), Index));

   -------------------
   -- Append_Unique --
   -------------------

   procedure Append_Unique
     (Source : in out UXString_List; New_Item : UXString; Sensitivity : Case_Sensitivity := Sensitive)
   is
      Ind   : Cursor  := Source.First;
      Found : Boolean := False;
   begin
      while Ind /= No_Element loop
         if Sensitivity = Sensitive then
            if Element (Ind) = New_Item then
               Found := True;
            end if;
         else
            if Equal_Case_Insensitive (Element (Ind), New_Item) then
               Found := True;
            end if;
         end if;
         Next (Ind);
      end loop;
      if not Found then
         Source.Append (New_Item);
      end if;
   end Append_Unique;

   ------------
   -- Filter --
   ------------

   function Filter
     (Source : UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity)
      return UXString_List
   is
      Result : UXString_List;
   begin
      for Item of Source loop
         if Item.Index (Pattern, Forward, Mapping) > 0 then
            Result.Append (Item);
         end if;
      end loop;
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Source : in out UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity)
   is
   begin
      Source := Source.Filter (Pattern);
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Source : UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString_List
   is
      Result : UXString_List;
   begin
      for Item of Source loop
         if Item.Index (Pattern, Forward, Mapping) > 0 then
            Result.Append (Item);
         end if;
      end loop;
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   procedure Filter (Source : in out UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function)
   is
   begin
      Source := Source.Filter (Pattern);
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Source : UXString_List; Pattern : Wide_Wide_Character_Set; Test : Membership := Inside) return UXString_List
   is
      Result : UXString_List;
   begin
      for Item of Source loop
         if Item.Index (Pattern, Test, Forward) > 0 then
            Result.Append (Item);
         end if;
      end loop;
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   procedure Filter (Source : in out UXString_List; Pattern : Wide_Wide_Character_Set; Test : Membership := Inside) is
   begin
      Source := Source.Filter (Pattern);
   end Filter;

   ----------
   -- Join --
   ----------

   function Join (Source : UXString_List; Separator : Unicode_Character) return UXString is
      Result : UXString;
      use type Ada.Containers.Count_Type;
   begin
      if Source.Length > 0 then
         for Item of Source loop
            Result.Append (Item);
            Result.Append (Separator);
         end loop;
         return Result.Slice (Result.First, Result.Last - 1);
      else
         return Null_UXString;
      end if;
   end Join;

   ----------
   -- Join --
   ----------

   function Join (Source : UXString_List; Separator : UXString) return UXString is
      Result : UXString;
      use type Ada.Containers.Count_Type;
   begin
      if Source.Length > 0 then
         for Item of Source loop
            Result.Append (Item);
            Result.Append (Separator);
         end loop;
         return Result.Slice (Result.First, Result.Last - Separator.Length);
      else
         return Null_UXString;
      end if;
   end Join;

   -----------------------
   -- Remove_Duplicates --
   -----------------------

   function Remove_Duplicates (Source : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return UXString_List
   is
      Result : UXString_List;
   begin
      for Item of Source loop
         Result.Append_Unique (Item);
      end loop;
      return Result;
   end Remove_Duplicates;

   -----------------------
   -- Remove_Duplicates --
   -----------------------

   procedure Remove_Duplicates
     (Source : in out UXString_List; Removed_Count : out Natural; Sensitivity : Case_Sensitivity := Sensitive)
   is
      use type Ada.Containers.Count_Type;
      Old_Length : constant Ada.Containers.Count_Type := Source.Length;
   begin
      Source        := Source.Remove_Duplicates;
      Removed_Count := Natural (Old_Length - Source.Length);
   end Remove_Duplicates;

   -------------
   -- Replace --
   -------------

   function Replace
     (Source : UXString_List; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive)
      return UXString_List
   is
      Result : UXString_List := Source;
   begin
      for Item of Result loop
         Item.Replace (Before, After, Sensitivity);
      end loop;
      return Result;
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Source : in out UXString_List; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive)
   is
   begin
      Source := Source.Replace (Before, After, Sensitivity);
   end Replace;

   -----------
   -- Slice --
   -----------

   function Slice (Source : UXString_List; Low : Positive; High : Natural) return UXString_List is
      Result : UXString_List;
   begin
      for Ind in Low .. High loop
         Result.Append (Source (Ind));
      end loop;
      return Result;
   end Slice;

   package UXString_Lists_Sort_Sensitive is new UXString_Lists.Generic_Sorting;
   package UXString_Lists_Sort_Insensitive is new UXString_Lists.Generic_Sorting (Less_Case_Insensitive);

   ----------
   -- Sort --
   ----------

   function Sort (Source : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return UXString_List is
      Result : UXString_List := Source;
   begin
      if Sensitivity = Sensitive then
         UXString_Lists_Sort_Sensitive.Sort (UXString_Lists.Vector (Result));
      else
         UXString_Lists_Sort_Insensitive.Sort (UXString_Lists.Vector (Result));
      end if;
      return Result;
   end Sort;

   ----------
   -- Sort --
   ----------

   procedure Sort (Source : in out UXString_List; Sensitivity : Case_Sensitivity := Sensitive) is
   begin
      Source := Sort (Source, Sensitivity);
   end Sort;

   ---------------
   -- Is_Sorted --
   ---------------

   function Is_Sorted (Source : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return Boolean is
   begin
      if Sensitivity = Sensitive then
         return UXString_Lists_Sort_Sensitive.Is_Sorted (UXString_Lists.Vector (Source));
      else
         return UXString_Lists_Sort_Insensitive.Is_Sorted (UXString_Lists.Vector (Source));
      end if;
   end Is_Sorted;

   -----------
   -- Merge --
   -----------

   function Merge (Left, Right : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return UXString_List is
      Result : UXString_List := Left;
      Source : UXString_List := Right;
   begin
      if Sensitivity = Sensitive then
         UXString_Lists_Sort_Sensitive.Merge (UXString_Lists.Vector (Result), UXString_Lists.Vector (Source));
      else
         UXString_Lists_Sort_Insensitive.Merge (UXString_Lists.Vector (Result), UXString_Lists.Vector (Source));
      end if;
      return Result;
   end Merge;

   -----------
   -- Merge --
   -----------

   procedure Merge (Left : UXString_List; Right : in out UXString_List; Sensitivity : Case_Sensitivity := Sensitive) is
   begin
      Right := Merge (Left, Right, Sensitivity);
   end Merge;

end UXStrings.Lists;
