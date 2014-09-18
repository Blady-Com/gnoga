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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;

package body Gnoga.Types is

   ---------------
   -- To_String --
   ---------------

   function To_String (RGBA : RGBA_Type) return String is
   begin
      return "rgba(" &
        Left_Trim (RGBA.Red'Img) & "," &
        Left_Trim (RGBA.Green'Img) & "," &
        Left_Trim (RGBA.Blue'Img) & "," &
        Left_Trim (RGBA.Alpha'Img) & ")";
   end To_String;

   -------------
   -- To_RGBA --
   -------------

   function To_RGBA (Value : String) return RGBA_Type is
      use Ada.Strings.Fixed;

      S    : Integer   := Value'First;
      F    : Integer   := Value'First - 1;
      RGBA : RGBA_Type;

      function Split (P : String) return String is
      begin
         S := F + 1;
         F := Index (Source  => Value,
                     Pattern => P,
                     From    => S);
         return Value (S .. (F - 1));
      end;

      function Split (P : String) return Integer is
      begin
         return Integer'Value (Split (P));
      end Split;

      function Split (P : String) return Alpha_Type is
      begin
         return Alpha_Type'Value (Split (P));
      end Split;

      rtype : String := Split ("(");
   begin
      RGBA.Red := Split (",");
      RGBA.Green := Split (",");

      if rtype'Length = 3 then
         RGBA.Blue := Split (")");
      else
         RGBA.Blue := Split (",");
         RGBA.Alpha := Split (")");
      end if;

      return RGBA;
   exception
      when others =>
         Log ("Error converting to rbga value = " & Value);
         return RGBA;
   end To_RGBA;

end Gnoga.Types;