package body UXStrings.Conversions is

   ------------------
   -- Scalar_Value --
   ------------------

   function Scalar_Value (Item : UXString) return T is
   begin
      return T'Wide_Wide_Value (To_Unicode (Item));
   end Scalar_Value;

   ------------------
   -- Scalar_Value --
   ------------------

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

   ----------------
   -- Real_Image --
   ----------------

   function Real_Image (Item : T) return UXString is
   begin
      return From_Unicode (T'Wide_Wide_Image (Item));
   end Real_Image;

end UXStrings.Conversions;
