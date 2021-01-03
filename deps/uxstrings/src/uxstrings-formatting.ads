with Ada.Strings;

package UXStrings.Formatting is

   subtype Number_Base is Integer range 2 .. 16;
   subtype Alignment is Ada.Strings.Alignment;

   generic
      type T is range <>;
   function Integer_Format
     (Item    :    T; Base : in Number_Base := 10; Put_Plus : in Boolean := False; Field : in Natural := 0;
      Justify : in Alignment := Left; Fill : in Character := ' ') return UXString;

end UXStrings.Formatting;
