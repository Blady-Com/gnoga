package UXStrings.Conversions is

   generic
      type T is (<>);
   function Scalar_Value (Item : UXString) return T;

   generic
      type T is digits <>;
   function Real_Value (Item : UXString) return T;

   generic
      type T is (<>);
   function Scalar_Image (Item : T) return UXString;

   generic
      type T is digits <>;
   function Real_Image (Item : T) return UXString;

end UXStrings.Conversions;
