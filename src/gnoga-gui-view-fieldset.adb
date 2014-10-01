------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--               G N O G A . G U I . V I E W . F I E L D S E T              --
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

package body Gnoga.Gui.View.Fieldset is
   ------------
   -- Create --
   ------------

   procedure Create
     (View          : in out Fieldset_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach        : in     Boolean := True;
      ID            : in     String  := "")
   is
   begin
      View.Create_From_HTML (Parent, "<fieldset />", ID);

      if Parent in Gnoga.Gui.Window.Window_Type'Class and Attach then
         Gnoga.Gui.Window.Window_Type (Parent).Set_View (View);
      end if;
   end Create;

   ----------------
   -- Put_Legend --
   ----------------

   procedure Put_Legend (View  : in out Fieldset_Type;
                         Value : in     String;
                         ID    : in     String  := "")
   is
      D : Gnoga.Gui.Element.Element_Type;
   begin
      D.Create_From_HTML (View, "<legend>" & Value & "</legend>", ID);
   end Put_Legend;

end Gnoga.Gui.View.Fieldset;
