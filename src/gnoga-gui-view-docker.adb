------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                 G N O G A . G U I . V I E W . D O C K E R                --
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

package body Gnoga.Gui.View.Docker is

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View          : in out Docker_View_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      ID            : in     String  := "")
   is
   begin
      View_Type (View).Create (Parent, ID);
   end Create;

   -----------------
   -- Update_Dock --
   -----------------

   procedure Update_Dock (View : in out Docker_View_Type) is
      Top_Height    : Natural := 0;
      Bottom_Height : Natural := 0;
      Left_Width    : Natural := 0;
      Right_Width   : Natural := 0;
   begin
      if View.Top_Dock /= null then
         View.Top_Dock.Top (0);
         View.Top_Dock.Left (0);
         View.Top_Dock.Box_Width (View.Outer_Width_To_Margin);
         Top_Height := View.Top_Dock.Outer_Height_To_Margin;
      end if;

      if View.Bottom_Dock /= null then
         View.Bottom_Dock.Bottom (0);
         View.Bottom_Dock.Left (0);
         View.Bottom_Dock.Box_Width (View.Outer_Width_To_Margin);
         Bottom_Height := View.Bottom_Dock.Outer_Height_To_Margin;
      end if;

      if View.Left_Dock /= null then
         View.Left_Dock.Top (Top_Height);
         View.Left_Dock.Left (0);
         View.Left_Dock.Box_Height
           (View.Outer_Height_To_Margin - Top_Height - Bottom_Height);
         Left_Width := View.Left_Dock.Width;
      end if;

      if View.Right_Dock /= null then
         View.Right_Dock.Top (Top_Height);
         View.Right_Dock.Right (0);
         View.Right_Dock.Box_Height
           (View.Outer_Height_To_Margin - Top_Height - Bottom_Height);
         Right_Width := View.Right_Dock.Width;
      end if;

      if View.Fill_Dock /= null then
         View.Fill_Dock.Top (Top_Height);
         View.Fill_Dock.Left (Left_Width);
         View.Fill_Dock.Box_Height
           (View.Outer_Height_To_Margin - Top_Height - Bottom_Height);
         View.Fill_Dock.Box_Width
           (View.Outer_Width_To_Margin - Left_Width - Right_Width);
      end if;
   exception
      when others =>
         null;
   end Update_Dock;

   --------------
   -- Top_Dock --
   --------------

   procedure Top_Dock (View : in out Docker_View_Type;
                       Dock : access View_Base_Type'Class)
   is
   begin
      View.Element_Map.Include ("top", Dock);
      Dock.Box_Sizing (Gnoga.Gui.Element.Border_Box);
      Dock.Position (Gnoga.Gui.Element.Absolute);
      View.Update_Dock;
   end Top_Dock;

   function Top_Dock (View : Docker_View_Type)
                      return Pointer_To_View_Base_Class
   is
   begin
      if View.Element_Map.Contains ("top") then
         return Pointer_To_View_Base_Class (View.Element_Map.Element ("top"));
      else
         return null;
      end if;
   end Top_Dock;

   -----------------
   -- Bottom_Dock --
   -----------------

   procedure Bottom_Dock (View : in out Docker_View_Type;
                          Dock : access View_Base_Type'Class)
   is
   begin
      View.Element_Map.Include ("bottom", Dock);
      Dock.Box_Sizing (Gnoga.Gui.Element.Border_Box);
      Dock.Position (Gnoga.Gui.Element.Absolute);
      View.Update_Dock;
   end Bottom_Dock;

   function Bottom_Dock (View : Docker_View_Type)
                         return Pointer_To_View_Base_Class
   is
   begin
      if View.Element_Map.Contains ("bottom") then
         return Pointer_To_View_Base_Class
           (View.Element_Map.Element ("bottom"));
      else
         return null;
      end if;
   end Bottom_Dock;

   ---------------
   -- Fill_Dock --
   ---------------

   procedure Fill_Dock (View : in out Docker_View_Type;
                        Dock : access View_Base_Type'Class)
   is
   begin
      View.Element_Map.Include ("fill", Dock);
      Dock.Box_Sizing (Gnoga.Gui.Element.Border_Box);
      Dock.Position (Gnoga.Gui.Element.Absolute);
      View.Update_Dock;
   end Fill_Dock;

   function Fill_Dock (View : Docker_View_Type)
                       return Pointer_To_View_Base_Class
   is
   begin
      if View.Element_Map.Contains ("fill") then
         return Pointer_To_View_Base_Class (View.Element_Map.Element ("fill"));
      else
         return null;
      end if;
   end Fill_Dock;

   ---------------
   -- Left_Dock --
   ---------------

   procedure Left_Dock (View : in out Docker_View_Type;
                        Dock : access View_Base_Type'Class)
   is
   begin
      View.Element_Map.Include ("left", Dock);
      Dock.Box_Sizing (Gnoga.Gui.Element.Border_Box);
      Dock.Position (Gnoga.Gui.Element.Absolute);
      View.Update_Dock;
   end Left_Dock;

   function Left_Dock (View : Docker_View_Type)
                       return Pointer_To_View_Base_Class
   is
   begin
      if View.Element_Map.Contains ("left") then
         return Pointer_To_View_Base_Class (View.Element_Map.Element ("left"));
      else
         return null;
      end if;
   end Left_Dock;

   ----------------
   -- Right_Dock --
   ----------------

   procedure Right_Dock (View : in out Docker_View_Type;
                         Dock : access View_Base_Type'Class)
   is
   begin
      View.Element_Map.Include ("right", Dock);
      Dock.Box_Sizing (Gnoga.Gui.Element.Border_Box);
      Dock.Position (Gnoga.Gui.Element.Absolute);
      View.Update_Dock;
   end Right_Dock;

   function Right_Dock (View : Docker_View_Type)
                        return Pointer_To_View_Base_Class
   is
   begin
      if View.Element_Map.Contains ("right") then
         return Pointer_To_View_Base_Class
           (View.Element_Map.Element ("right"));
      else
         return null;
      end if;
   end Right_Dock;

   ---------------
   -- On_Resize --
   ---------------

   overriding
   procedure On_Resize (View : in out Docker_View_Type) is
   begin
      View.Update_Dock;
   end On_Resize;

end Gnoga.Gui.View.Docker;
