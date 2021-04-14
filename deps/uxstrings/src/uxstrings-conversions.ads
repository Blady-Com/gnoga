package UXStrings.Conversions is

   subtype Number_Base is Integer range 2 .. 16;
   -- Range of possible numerical base on integer values and images
   type Number_Prefix is (None, ' ', '+');
   -- List of possible prefix on integer images

   generic
      type T is (<>);
   function Scalar_Value (Item : UXString) return T;
   -- Return the conversion of the string Item into scalar value

   generic
      type T is range <>;
   function Integer_Value (Item : UXString; Base : in Number_Base := 10) return T;
   -- Return the conversion of the string Item into integer value with respect of specified Base

   generic
      type T is digits <>;
   function Floating_Point_Value (Item : UXString) return T;
   -- Return the conversion of the string Item into floating point value

   generic
      type T is delta <>;
   function Fixed_Point_Value (Item : UXString) return T;
   -- Return the conversion of the string Item into fixed point value

   generic
      type T is (<>);
   function Scalar_Image (Item : T) return UXString;
   -- Return the conversion of the scalar Item into string

   generic
      type T is range <>;
   function Integer_Image (Item : T; Base : in Number_Base := 10; Prefix : Number_Prefix := None) return UXString;
   -- Return the conversion of the integer Item into string with respect of specified Base and Prefix

   generic
      type T is digits <>;
   function Floating_Point_Image (Item : T) return UXString;
   -- Return the conversion of the floating point Item into string

   generic
      type T is delta <>;
   function Fixed_Point_Image (Item : T) return UXString;
   -- Return the conversion of the fixed point Item into string

end UXStrings.Conversions;
