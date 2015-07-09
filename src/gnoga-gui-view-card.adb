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

with Gnoga.Gui.Window;
with Gnoga.Gui.Element.Style_Block;

package body Gnoga.Gui.View.Card is

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View          : in out Card_View_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      ID            : in     String  := "")
   is
   begin
      View_Type (View).Create (Parent, ID);

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

   ------------
   -- Create --
   ------------

   procedure Create
     (Tab          : in out Tab_Type;
      Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
      Card_View    : in out Card_View_Type'Class;
      Text_Color   : in     Gnoga.Types.RGBA_Type := (255, 255, 255, 1.0);
      Tab_Color    : in     Gnoga.Types.RGBA_Type := (0, 0, 0, 1.0);
      Select_Color : in     Gnoga.Types.RGBA_Type := (128, 128, 128, 1.0);
      ID           : in     String  := "")
   is
   begin
      Tab.Card_View    := Card_View'Unrestricted_Access;
      Tab.Tab_Color    := Tab_Color;
      Tab.Select_Color := Select_Color;

      Tab.Create_From_HTML (Parent, "<ul />", ID);

      declare
         use Gnoga.Types;

         Name  : constant String := Tab.ID;
         Style : Gnoga.Gui.Element.Style_Block.Style_Type;
      begin
         Style.Create
           (Parent,
            "ul#" & Name & " {padding: 0;}" &
              " ul#" & Name & " li {display: inline;}" &
              " ul#" & Name &
              "   li a {background-color: " & To_String (Tab_Color) & ";" &
              "   color: " & To_String (Text_Color) & ";" &
              "   padding: 10px 20px;" &
              "   margin: 3px;" &
              "   text-decoration: none;" &
              "   border-radius: 4px 4px 0 0;}");
      end;
   end Create;

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
   begin
      Tab_Item_Type (Object).Tab_Select;
   end On_Click;

   -------------
   -- Add_Tab --
   -------------

   procedure Add_Tab (Tab      : in out Tab_Type;
                      Card     : in     String;
                      Label    : in     String;
                      Selected : in     Boolean := False)
   is
      T : Tab_Item_Access := new Tab_Item_Type;
   begin
      T.Dynamic;
      T.Create (Parent => Tab,
                Card   => Card,
                Label  => Label);

      if Selected then
         Tab.Select_Tab (Card);
      end if;
   end Add_Tab;

   ----------------
   -- Select_Tab --
   ----------------

   procedure Select_Tab (Tab  : in out Tab_Type;
                         Card : in     String)
   is
      use type Gnoga.Gui.Element.Pointer_To_Element_Class;

      P : Gnoga.Gui.Element.Pointer_To_Element_Class := Tab.Element (Card);
   begin
      if P /= null then
         Tab_Item_Access (P).Tab_Select;
      end if;
   end Select_Tab;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item   : in out Tab_Item_Type;
      Parent : in out Tab_Type'Class;
      Card   : in     String;
      Label  : in     String;
      ID     : in     String := "")
   is
   begin
      Item.Create_From_HTML
        (Parent, "<li />", ID);
      Item.Inner_HTML ("<a id='" & Item.ID & "_a" &
                         "' href='javascript:void(0)'>" &
                         Escape_Quotes (Label) & "</a>");

      Item.On_Click_Handler (On_Click'Access);

      declare
         L : Gnoga.Gui.Element.Element_Type;
      begin
         L.Attach_Using_Parent (Item, ID => Item.ID & "_a");
         Parent.Height (L.Offset_Top * 2 + L.Client_Height);
      end;

      Item.Card_Name := Ada.Strings.Unbounded.To_Unbounded_String (Card);

      Parent.Add_Element (Card, Item'Unrestricted_Access);
      --  Allow for the Parent.Select_Tab to work
   end Create;

   ----------------
   -- Tab_Select --
   ----------------

   procedure Tab_Select (Item  : in out Tab_Item_Type)
   is
      Link : Gnoga.Gui.Element.Element_Type;
   begin
      declare
         C : Gnoga.Gui.Element. Element_Access :=
               new Gnoga.Gui.Element.Element_Type;
         N : Gnoga.Gui.Element.Element_Access;
      begin
         Gnoga.Gui.Element.Element_Access (Item.Parent).First_Child (C.all);

         while C.ID /= "undefined" loop
            declare
               L : Gnoga.Gui.Element.Element_Type;
            begin
               L.Attach_Using_Parent (Item, ID => C.ID & "_a");
               L.Background_Color (Tab_Access (Item.Parent).Tab_Color);
            end;

            N := new Gnoga.Gui.Element.Element_Type;
            C.Next_Sibling (N.all);
            C.Free;
            C := N;
         end loop;

         C.Free;
      end;

      Link.Attach_Using_Parent (Item, ID => Item.ID & "_a");
      Link.Background_Color (Tab_Access (Item.Parent).Select_Color);

      Tab_Access (Item.Parent).Card_View.Show_Card
        (Ada.Strings.Unbounded.To_String (Item.Card_Name));
   end Tab_Select;

   ------------------
   -- Tab_Selected --
   ------------------

   function Tab_Selected (Item : Tab_Item_Type) return Boolean
   is
      use type Gnoga.Types.RGBA_Type;

      Link : Gnoga.Gui.Element.Element_Type;
   begin
      Link.Attach_Using_Parent (Item, ID => Item.ID & "_a");

      return Link.Background_Color = Tab_Access (Item.Parent).Select_Color;
   end Tab_Selected;

end Gnoga.Gui.View.Card;
