------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                      G N O G A . G U I . S C R E E N                     --
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
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Gnoga.Server.Connection;

package body Gnoga.Gui.Screen is

   -----------
   -- Width --
   -----------

   function Width (ID : Gnoga.Types.Connection_ID) return Integer is
   begin
      return Integer'Value
        (Gnoga.Server.Connection.Execute_Script (ID, "screen.width;"));
   end Width;

   ------------
   -- Height --
   ------------

   function Height (ID : Gnoga.Types.Connection_ID) return Integer is
   begin
      return Integer'Value
        (Gnoga.Server.Connection.Execute_Script (ID, "screen.height;"));
   end Height;

   ----------------------
   -- Available_Height --
   ----------------------

   function Available_Height
     (ID : Gnoga.Types.Connection_ID)
      return Integer
   is
   begin
      return Integer'Value
        (Gnoga.Server.Connection.Execute_Script (ID, "screen.availHeight;"));
   end Available_Height;

   ---------------------
   -- Available_Width --
   ---------------------

   function Available_Width (ID : Gnoga.Types.Connection_ID) return Integer is
   begin
      return Integer'Value
        (Gnoga.Server.Connection.Execute_Script (ID, "screen.availWidth;"));
   end Available_Width;

   -----------------
   -- Color_Depth --
   -----------------

   function Color_Depth (ID : Gnoga.Types.Connection_ID) return Integer is
   begin
      return Integer'Value
        (Gnoga.Server.Connection.Execute_Script (ID, "screen.colorDepth;"));
   end Color_Depth;

   -----------------
   -- Pixel_Depth --
   -----------------

   function Pixel_Depth (ID : Gnoga.Types.Connection_ID) return Integer is
   begin
      return Integer'Value
        (Gnoga.Server.Connection.Execute_Script (ID, "screen.pixelDepth;"));
   end Pixel_Depth;

end Gnoga.Gui.Screen;
