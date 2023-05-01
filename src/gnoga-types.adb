------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                         G N O G A . T Y P E S                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Ada.Exceptions;

package body Gnoga.Types is
   function To_RGBA_From_Hex
     (Value : String)
      return RGBA_Type;
   function To_RGBA_From_RGB_or_RGBA
     (Value : String)
      return RGBA_Type;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (RGBA : RGBA_Type)
      return String
   is
      function Image is new UXStrings.Conversions.Scalar_Image (Color_Type);
      function Image is new UXStrings.Conversions.Fixed_Point_Image (Alpha_Type);
   begin
      return
        "rgba(" & Image (RGBA.Red) & "," & Image (RGBA.Green) & "," & Image (RGBA.Blue) & "," & Image (RGBA.Alpha) &
        ")";
   end To_String;

   ------------
   -- To_Hex --
   ------------

   function To_Hex
     (RGBA : RGBA_Type)
      return String
   is
      Hex : constant String := "0123456789ABCDEF";
   begin
      return
        "0x" & Hex (Natural (RGBA.Red) / 16 + 1) & Hex (Natural (RGBA.Red) mod 16 + 1) &
        Hex (Natural (RGBA.Green) / 16 + 1) & Hex (Natural (RGBA.Green) mod 16 + 1) &
        Hex (Natural (RGBA.Blue) / 16 + 1) & Hex (Natural (RGBA.Blue) mod 16 + 1);
   end To_Hex;

   -------------
   -- To_RGBA --
   -------------

   function To_RGBA
     (Value : String)
      return RGBA_Type
   is
   begin
      if Value (Value.First) = '#' then
         return To_RGBA_From_Hex (Value);
      else
         return To_RGBA_From_RGB_or_RGBA (Value);
      end if;
   end To_RGBA;

   ----------------------
   -- To_RGBA_From_Hex --
   ----------------------

   function To_RGBA_From_Hex
     (Value : String)
      return RGBA_Type
   is
      RGBA : RGBA_Type;
      P    : constant Integer := Value.First;
   begin
      if Value.Length = 7 then
         RGBA.Red   := Color_Type'Value (To_ASCII ("16#" & Value.Slice ((P + 1), (P + 2))) & '#');
         RGBA.Green := Color_Type'Value (To_ASCII ("16#" & Value.Slice ((P + 3), (P + 4))) & '#');
         RGBA.Blue  := Color_Type'Value (To_ASCII ("16#" & Value.Slice ((P + 5), (P + 6))) & '#');
      elsif Value.Length = 9 then
         RGBA.Alpha := Alpha_Type (Integer'Value (To_ASCII ("16#" & Value.Slice ((P + 1), (P + 2))) & '#')) / 255;
         RGBA.Red   := Color_Type'Value (To_ASCII ("16#" & Value.Slice ((P + 3), (P + 4))) & '#');
         RGBA.Green := Color_Type'Value (To_ASCII ("16#" & Value.Slice ((P + 5), (P + 6))) & '#');
         RGBA.Blue  := Color_Type'Value (To_ASCII ("16#" & Value.Slice ((P + 7), (P + 8))) & '#');
      else
         Log ("Invalid Hex value for rbga value from " & Value);
      end if;

      return RGBA;
   exception
      when E : others =>
         Log ("Error converting to rbga value from " & Value);
         Log (From_UTF_8 (Ada.Exceptions.Exception_Information (E)));
         return RGBA;
   end To_RGBA_From_Hex;

   ------------------------------
   -- To_RGBA_From_RGB_or_RGBA --
   ------------------------------

   function To_RGBA_From_RGB_or_RGBA
     (Value : String)
      return RGBA_Type
   is
      S    : Integer := Value.First;
      F    : Integer := Value.First - 1;
      RGBA : RGBA_Type;

      function Split
        (P : String)
         return String;
      function Split
        (P : String)
         return Color_Type;
      function Split
        (P : String)
         return Alpha_Type;
      --  Split string and extract values

      function Split
        (P : String)
         return String
      is
      begin
         S := F + 1;
         F := Index (Source => Value, Pattern => P, From => S);
         return Value.Slice (S, (F - 1));
      end Split;

      function Split
        (P : String)
         return Color_Type
      is
         function Value is new UXStrings.Conversions.Scalar_Value (Color_Type);
      begin
         return Value (Split (P));
      end Split;

      function Split
        (P : String)
         return Alpha_Type
      is
         function Value is new UXStrings.Conversions.Fixed_Point_Value (Alpha_Type);
      begin
         return Value (Split (P));
      end Split;

      rtype : constant String := Split ("(");
   begin
      RGBA.Red   := Split (",");
      RGBA.Green := Split (",");

      if rtype.Length = 3 then
         RGBA.Blue := Split (")");
      else
         RGBA.Blue  := Split (",");
         RGBA.Alpha := Split (")");
      end if;

      return RGBA;
   exception
      when others =>
         Log ("Error converting to rbga value from " & Value);
         return RGBA;
   end To_RGBA_From_RGB_or_RGBA;

   -------------
   -- To_RGBA --
   -------------

   function To_RGBA
     (Value : in Pixel_Type)
      return RGBA_Type
   is
   begin
      return (Value.Red, Value.Green, Value.Blue, Fractional_Range_Type (Float (Value.Alpha) / 255.0));
   end To_RGBA;

   --------------
   -- To_Pixel --
   --------------

   function To_Pixel
     (Value : in RGBA_Type)
      return Pixel_Type
   is
   begin
      return (Value.Red, Value.Green, Value.Blue, Gnoga.Types.Color_Type (Value.Alpha * 255.0));
   end To_Pixel;
end Gnoga.Types;
