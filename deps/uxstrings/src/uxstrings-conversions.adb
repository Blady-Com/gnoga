-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-conversions.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString conversions implementation.
-- NOTES                        : Ada 202x
--
-- COPYRIGHT                    : (c) Pascal Pignard 2020
-- LICENCE                      : CeCILL V2.1 (https://cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Strings_Edit.Integer_Edit;

package body UXStrings.Conversions is

   ------------------
   -- Scalar_Value --
   ------------------

   function Scalar_Value (Item : UXString) return T is
   begin
      return T'Wide_Wide_Value (To_Unicode (Item));
   end Scalar_Value;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value (Item : UXString; Base : in Number_Base := 10) return T is
      package Strings_Edit_T is new Strings_Edit.Integer_Edit (T);
   begin
      return Strings_Edit_T.Value (To_ASCII (Item), Base);
   end Integer_Value;

   --------------------------
   -- Floating_Point_Value --
   --------------------------

   function Floating_Point_Value (Item : UXString) return T is
   begin
      return T'Wide_Wide_Value (To_Unicode (Item));
   end Floating_Point_Value;

   -----------------------
   -- Fixed_Point_Value --
   -----------------------

   function Fixed_Point_Value (Item : UXString) return T is
   begin
      return T'Wide_Wide_Value (To_Unicode (Item));
   end Fixed_Point_Value;

   ------------------
   -- Scalar_Image --
   ------------------

   function Scalar_Image (Item : T) return UXString is
   begin
      return From_Unicode (T'Wide_Wide_Image (Item));
   end Scalar_Image;

   -------------------
   -- Integer_Image --
   -------------------

   function Integer_Image (Item : T; Base : in Number_Base := 10; Prefix : Number_Prefix := None) return UXString is
      package Strings_Edit_T is new Strings_Edit.Integer_Edit (T);
   begin
      case Prefix is
         when None =>
            return From_Latin_1 (Strings_Edit_T.Image (Item, Base, False));
         when ' ' =>
            return From_Latin_1 (' ' & Strings_Edit_T.Image (Item, Base, False));
         when '+' =>
            return From_Latin_1 (Strings_Edit_T.Image (Item, Base, True));
      end case;
   end Integer_Image;

   --------------------------
   -- Floating_Point_Image --
   --------------------------

   function Floating_Point_Image (Item : T) return UXString is
   begin
      return From_Unicode (T'Wide_Wide_Image (Item));
   end Floating_Point_Image;

   -----------------------
   -- Fixed_Point_Image --
   -----------------------

   function Fixed_Point_Image (Item : T) return UXString is
   begin
      return From_Unicode (T'Wide_Wide_Image (Item));
   end Fixed_Point_Image;

end UXStrings.Conversions;
