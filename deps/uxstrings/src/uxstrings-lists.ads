with Ada.Containers.Vectors;

package UXStrings.Lists is

   package UXString_Lists is new Ada.Containers.Vectors (Positive, UXString);
   type UXString_List is new UXString_Lists.Vector with private with
     Constant_Indexing => Constant_Reference, Variable_Indexing => Reference, Default_Iterator => Iterate,
     Iterator_Element  => UXString,
     Aggregate => (Empty => Empty, Add_Unnamed => Append, New_Indexed => New_Vector, Assign_Indexed => Replace_Element);

   function Constant_Reference
     (Container : aliased UXString_List; Index : Positive) return UXString_Lists.Constant_Reference_Type;
   function Reference (Container : aliased in out UXString_List; Index : Positive) return UXString_Lists.Reference_Type;

   procedure Append_Unique
     (Source : in out UXString_List; New_Item : UXString; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source to the concatenation of Source and New_Item if New_Item is not already included with respect of case sensitivity

   function Filter
     (Source : UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity)
      return UXString_List;
   -- Return a list of strings containing the pattern with respect of Mapping
   procedure Filter
     (Source : in out UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity);
   -- Update Source with a list of strings containing the pattern with respect of Mapping
   function Filter
     (Source : UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString_List;
   -- Return a list of strings containing the pattern with respect of Mapping
   procedure Filter (Source : in out UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function);
   -- Update Source with a list of strings containing the pattern with respect of Mapping
   function Filter
     (Source : UXString_List; Pattern : Wide_Wide_Character_Set; Test : Membership := Inside) return UXString_List;
   -- Return a list of strings containing the pattern character inside or outside Set matches Source with respect of Test membership
   procedure Filter (Source : in out UXString_List; Pattern : Wide_Wide_Character_Set; Test : Membership := Inside);
   -- Update Source with a list of strings containing the pattern character inside or outside Set matches Source with respect of Test membership

   function Join (Source : UXString_List; Separator : Unicode_Character) return UXString;
   -- Return a string with all the list's strings concatened and
   -- separated by the given separator
   function Join (Source : UXString_List; Separator : UXString) return UXString;
   -- Return a string with all the list's strings concatened and
   -- separated by the given separator (which can be an empty string)

   function Remove_Duplicates
     (Source : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return UXString_List;
   -- Return a list of strings without the duplicated ones with respect of case sensitivity
   procedure Remove_Duplicates
     (Source : in out UXString_List; Removed_Count : out Natural; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source with the list strings without the duplicated ones and the number of removed entries
   -- with respect of sensitivity

   function Replace
     (Source : UXString_List; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive)
      return UXString_List;
   -- Return a string list where every string has had the before text replaced with the after text
   -- wherever the before text is found with respect of case sensitivity
   procedure Replace
     (Source : in out UXString_List; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source where every string has had the before text replaced with the after text
   -- wherever the before text is found with respect of case sensitivity

   function Slice (Source : UXString_List; Low : Positive; High : Natural) return UXString_List;
   -- Return the slice at positions Low through High from Source

   function Sort (Source : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return UXString_List;
   -- Return a string list which is sorted in ascending order with respect of case sensitivity
   procedure Sort (Source : in out UXString_List; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source with the sorted list in ascending order with respect of case sensitivity
   function Is_Sorted (Source : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return Boolean;
   -- Return True if Source is sorted with respect of case sensitivity
   function Merge (Left, Right : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return UXString_List;
   --- Return a string list with merging Left and Right string lists with respect of case sensitivity
   procedure Merge (Left : UXString_List; Right : in out UXString_List; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source with the merged list of Left and Right string lists with respect of case sensitivity

private
   type UXString_List is new UXString_Lists.Vector with null record;
end UXStrings.Lists;
