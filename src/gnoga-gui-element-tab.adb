------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                  G N O G A . G U I . E L E M E N T . T A B               --
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

with Ada.Strings.Fixed;

with Gnoga.Gui.Window;
with Gnoga.Gui.Element.Style_Block;

package body Gnoga.Gui.Element.Tab is

   ------------
   -- Create --
   ------------

   procedure Create
     (Tab          : in out Tab_Type;
      Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
      Card_View    : in out Gnoga.Gui.View.Card.Card_View_Type'Class;
      Text_Color   : in     Gnoga.Types.RGBA_Type := (255, 255, 255, 1.0);
      Tab_Color    : in     Gnoga.Types.RGBA_Type := (0, 0, 0, 1.0);
      Select_Color : in     Gnoga.Types.RGBA_Type := (128, 128, 128, 1.0);
      Attach       : in     Boolean := True;
      ID           : in     String  := "")
   is
   begin
      Tab.Card_View    := Card_View'Unrestricted_Access;
      Tab.Tab_Color    := Tab_Color;
      Tab.Select_Color := Select_Color;

      Tab.Create_From_HTML (Parent, "<ul />", ID);

      if Parent in Gnoga.Gui.Window.Window_Type'Class and Attach then
         Gnoga.Gui.Window.Window_Type (Parent).Set_View (Tab);
      end if;

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

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
   begin
      Tab_Item_Type (Object).Tab_Select;
   end On_Click;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item   : in out Tab_Item_Type;
      Parent : in out Tab_Type'Class;
      Card   : in     String;
      Text   : in     String := "";
      ID     : in     String := "")
   is
   begin

      Item.Create_From_HTML
        (Parent, "<li />", ID);
      Item.Inner_HTML ("<a id='" & Item.ID & "_a" &
                         "' href='javascript:void(0)'>" &
                         Escape_Quotes (Text) & "</a>");

      Item.On_Click_Handler (On_Click'Access);

      Parent.Height (Item.Offset_Height + Item.Offset_Top + Item.Height);

      Item.Card_Name := Ada.Strings.Unbounded.To_Unbounded_String (Card);
   end Create;

   ----------------
   -- Tab_Select --
   ----------------

   procedure Tab_Select (Item  : in out Tab_Item_Type)
   is
      Link : Element_Type;
   begin
      declare
         C : Element_Access := new Element_Type;
         N : Element_Access;
      begin
         Element_Access (Item.Parent).First_Child (C.all);

         while C.ID /= "undefined" loop
            declare
               L : Element_Type;
            begin
               L.Attach_Using_Parent (Item, ID => C.ID & "_a");
               L.Background_Color (Tab_Access (Item.Parent).Tab_Color);
            end;

            N := new Element_Type;
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
      Link : Element_Type;
   begin
      Link.Attach_Using_Parent (Item, ID => Item.ID & "_a");

      return Link.Background_Color = Tab_Access (Item.Parent).Select_Color;
   end Tab_Selected;

end Gnoga.Gui.Element.Tab;
