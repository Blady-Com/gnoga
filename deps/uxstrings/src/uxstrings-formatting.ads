with Ada.Strings;

package UXStrings.Formatting is

   subtype Number_Base is Integer range 2 .. 16;
   -- Range of possible numerical base on integer values
   subtype Alignment is Ada.Strings.Alignment;
   -- List of possible alignment of integer values

   generic
      type T is range <>;
   function Integer_Format
     (Item    :    T; Base : in Number_Base := 10; Put_Plus : in Boolean := False; Field : in Natural := 0;
      Justify : in Alignment := Left; Fill : in Character := ' ') return UXString;
   -- Return the formatted string of the integer Item with respect of specified Base, Put_Plus sign,
   -- output Field size, alignment Justify, padding Fill

end UXStrings.Formatting;
