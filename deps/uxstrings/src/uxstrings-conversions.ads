package UXStrings.Conversions is

   subtype Number_Base is Integer range 2 .. 16;
   type Number_Prefix is (None, ' ', '+');

   generic
      type T is (<>);
   function Scalar_Value (Item : UXString) return T;

   generic
      type T is range <>;
   function Integer_Value (Item : UXString; Base : in Number_Base := 10) return T;

   generic
      type T is digits <>;
   function Floating_Point_Value (Item : UXString) return T;

   generic
      type T is delta <>;
   function Fixed_Point_Value (Item : UXString) return T;

   generic
      type T is (<>);
   function Scalar_Image (Item : T) return UXString;

   generic
      type T is range <>;
   function Integer_Image (Item : T; Base : in Number_Base := 10; Prefix : Number_Prefix := None) return UXString;

   generic
      type T is digits <>;
   function Floating_Point_Image (Item : T) return UXString;

   generic
      type T is delta <>;
   function Fixed_Point_Image (Item : T) return UXString;

end UXStrings.Conversions;
