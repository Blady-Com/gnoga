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
      return Pointer_To_View_Class
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
      return Pointer_To_View_Class
   is
   begin
      return Pointer_To_View_Class (View.Element_Map.Element (Name));
   end Card;

   --------------
   -- Add_Card --
   --------------

   procedure Add_Card
     (View : in out Card_View_Type;
      Name : in     String;
      Show : in     Boolean := True)
   is
      use Gnoga.Gui.Element;

      Card : View_Access := new View_Type;
   begin
      Card.Create (View);

      if not Show then
         Card.Display ("none");
      end if;

      Card.Box_Sizing (Border_Box);
      Card.Box_Sizing (Border_Box);
      Card.Position (Gnoga.Gui.Element.Fixed);
      Card.Left (0);
      Card.Top (0);
      View.Element_Map.Insert
        (Name, Gnoga.Gui.Base.Pointer_To_Base_Class (Card));

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

      Current : Pointer_To_View_Class := View.Current_Card;
   begin
      if Current /= null then
         Current.Display ("none");
      end if;

      Current := View.Card (Name);
      View.Element_Map.Include
        ("current", Gnoga.Gui.Base.Pointer_To_Base_Class (Current));

      Current.Display ("block");
      Current.Box_Height (View.Height);
      Current.Box_Width (View.Width);
   end Show_Card;

   -----------------------
   -- On_Resize_Handler --
   -----------------------

   overriding procedure On_Resize_Handler
     (Object  : in out Card_View_Type;
      Handler : in     Gnoga.Gui.Base.Action_Event)
   is
      use type Gnoga.Gui.Base.Action_Event;
   begin
      Object.Unbind_Event ("resize");
      --  Remove binding set by Card_View_Type create.

      View_Type (Object).On_Resize_Handler (Handler);

      if Handler = null then
         Object.Bind_Event (Event   => "resize",
                            Message => "");
         --  Rebind if additional handler was removed.
      end if;
   end On_Resize_Handler;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize (View : in out Card_View_Type) is
   begin
      View.Show_Card ("current");
   end On_Resize;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Object  : in out Card_View_Type;
      Event   : in     String;
      Message : in     String)
   is
   begin
      if Event = "resize" then
         Card_View_Type'Class (Object).On_Resize;
         Object.Fire_On_Resize;
      else
         Gnoga.Gui.Base.Base_Type (Object).On_Message (Event, Message);
      end if;
   end On_Message;

end Gnoga.Gui.View.Card;
