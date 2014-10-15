------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                   G N O G A . G U I . V I E W . C A R D                  --
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

package body Gnoga.Gui.View.Card is

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View          : in out Card_View_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach        : in     Boolean := True;
      ID            : in     String  := "")
   is
   begin
      View_Type (View).Create (Parent, Attach, ID);

      View.Element_Map.Insert ("current", null);

      View.Bind_Event (Event   => "resize",
                         Message => "");
   end Create;

   ------------------
   -- Current_Card --
   ------------------

   function Current_Card
     (View : Card_View_Type)
      return Pointer_To_View_Base_Class
   is
   begin
      return View.Card ("current");
   end Current_Card;

   ----------
   -- Card --
   ----------

   function Card
     (View : Card_View_Type;
      Name : String)
      return Pointer_To_View_Base_Class
   is
   begin
      if View.Element_Map.Contains (Name) then
         return Pointer_To_View_Base_Class (View.Element_Map.Element (Name));
      else
         return null;
      end if;
   end Card;

   --------------
   -- Add_Card --
   --------------

   procedure Add_Card (View : in out Card_View_Type;
                       Name : in     String;
                       Card : access View_Base_Type'Class := null;
                       Show : in     Boolean              := False)
   is
      use Gnoga.Gui.Element;

      New_Card : Pointer_To_View_Base_Class;
   begin
      if Card = null then
         New_Card := new View_Type;
         View_Access (New_Card).Create (View);
      else
         New_Card := Card.all'Unrestricted_Access;
      end if;

      if not Show then
         New_Card.Display ("none");
      end if;

      New_Card.Box_Sizing (Border_Box);
      New_Card.Position (Gnoga.Gui.Element.Relative);
      New_Card.Left (0);
      New_Card.Top (0);
      View.Element_Map.Insert (Name, Pointer_To_Element_Class (New_Card));

      if Show then
         View.Show_Card (Name);
      end if;
   end Add_Card;

   ---------------
   -- Show_Card --
   ---------------

   procedure Show_Card
     (View : in out Card_View_Type;
      Name : in     String)
   is
      use Gnoga.Gui.Element;

      Current : Pointer_To_View_Base_Class := View.Current_Card;
   begin
      if Current /= null then
         Current.Display ("none");
      end if;

      Current := View.Card (Name);

      if Current /= null then
         View.Element_Map.Include
           ("current", Gnoga.Gui.Element.Pointer_To_Element_Class (Current));

         Current.Display ("block");
         Current.Box_Height (View.Height);
         Current.Box_Width (View.Width);
      end if;
   end Show_Card;

   ---------------
   -- On_Resize --
   ---------------

   overriding
   procedure On_Resize (View : in out Card_View_Type) is
   begin
      View.Show_Card ("current");

      View_Type (View).On_Resize;
   end On_Resize;

end Gnoga.Gui.View.Card;
