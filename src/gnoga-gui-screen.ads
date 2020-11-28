------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                      G N O G A . G U I . S C R E E N                     --
--                                                                          --
--                                 S p e c                                  --
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

with Gnoga.Gui.Window;

package Gnoga.Gui.Screen is

   --  Access information about user's screen

   function Width
     (Window : Gnoga.Gui.Window.Window_Type'Class)
      return Integer;
   --  Screen width

   function Height
     (Window : Gnoga.Gui.Window.Window_Type'Class)
      return Integer;
   --  Screen height

   function Available_Height
     (Window : Gnoga.Gui.Window.Window_Type'Class)
      return Integer;
   --  Available total height = Height - Window treatments

   function Available_Width
     (Window : Gnoga.Gui.Window.Window_Type'Class)
      return Integer;
   --  Available total width = Width - Window treatments

   function Color_Depth
     (Window : Gnoga.Gui.Window.Window_Type'Class)
      return Integer;
   --  Bit depth of rendering color palette

   function Pixel_Depth
     (Window : Gnoga.Gui.Window.Window_Type'Class)
      return Integer;
   --  Color depth of screen in bits per pixel

end Gnoga.Gui.Screen;
