package UXStrings.Conversions is

   subtype NumberBase is Integer range 2 .. 16;

   generic
      type T is (<>);
   function Scalar_Value (Item : UXString) return T;

   generic
      type T is range <>;
   function Integer_Value (Item : UXString; Base : in NumberBase := 10) return T;

   generic
      type T is digits <>;
   function Real_Value (Item : UXString) return T;

   generic
      type T is (<>);
   function Scalar_Image (Item : T) return UXString;

   generic
      type T is range <>;
   function Integer_Image (Item : T; Base : in NumberBase := 10) return UXString;

   generic
      type T is digits <>;
   function Real_Image (Item : T) return UXString;

end UXStrings.Conversions;
