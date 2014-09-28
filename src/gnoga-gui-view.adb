------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A . G U I . V I E W                        --
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

with Gnoga.Gui.Window;
with Gnoga.Gui.Element.Common;

package body Gnoga.Gui.View is

   ------------
   -- Create --
   ------------

   procedure Create
     (View    : in out View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach  : in     Boolean := True;
      ID      : in     String := "")
   is
   begin
      View.Create_From_HTML (Parent, "<div />", ID);

      if Parent in Gnoga.Gui.Window.Window_Type'Class and Attach then
         Gnoga.Gui.Window.Window_Type (Parent).Set_View (View);
      end if;
   end Create;

   --------------------
   -- On_Child_Added --
   --------------------

   procedure On_Child_Added (View  : in out View_Type;
                             Child : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      use Gnoga.Gui.Element;
   begin
      if Child in Element_Type'Class then
         Element_Type (Child).Place_Inside_Bottom_Of (View);
      end if;
   end On_Child_Added;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (View    : in out View_Type;
                       Message : in     String;
                       Class   : in     String := "";
                       ID      : in     String := "")
   is
      D : Gnoga.Gui.Element.Common.DIV_Type;
   begin
      D.Create (View, Message, ID);
      if Class /= "" then
         D.Class_Name (Class);
      end if;
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (View : in out View_Type) is
      D : Gnoga.Gui.Element.Common.DIV_Type;
   begin
      D.Create (View, "<br />");
   end New_Line;

end Gnoga.Gui.View;
