-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-conversions.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString conversions implementation.
-- NOTES                        : Ada 202x
--
-- COPYRIGHT                    : (c) Pascal Pignard 2020
-- LICENCE                      : CeCILL V2.1 (https://www.cecill.info)
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

   function Integer_Value (Item : UXString; Base : in NumberBase := 10) return T is
      package Strings_Edit_T is new Strings_Edit.Integer_Edit (T);
   begin
      return Strings_Edit_T.Value (To_Latin_1 (Item), Base);
   end Integer_Value;

   -----------------
   -- Real_Value --
   -----------------

   function Real_Value (Item : UXString) return T is
   begin
      return T'Wide_Wide_Value (To_Unicode (Item));
   end Real_Value;

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

   function Integer_Image (Item : T; Base : in NumberBase := 10) return UXString is
      package Strings_Edit_T is new Strings_Edit.Integer_Edit (T);
   begin
      return From_Latin_1 (Strings_Edit_T.Image (Item, Base));
   end Integer_Image;

   ----------------
   -- Real_Image --
   ----------------

   function Real_Image (Item : T) return UXString is
   begin
      return From_Unicode (T'Wide_Wide_Image (Item));
   end Real_Image;

end UXStrings.Conversions;
