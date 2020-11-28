------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . P L U G I N . M N M E N U             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2017 Pascal Pignard                    --
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

package body Gnoga.Gui.Plugin.MNMenu is

   -----------------
   -- Load_MNMenu --
   -----------------

   procedure Load_MNMenu (Window : in out Gnoga.Gui.Window.Window_Type'Class) is
   begin
      Window.Document.Head_Element.jQuery_Execute
        ("append('" & Escape_Quotes ("<link href='/css/mnmenu.css'" & " type='text/css' rel='stylesheet'>") & "')");
      Window.Document.Head_Element.jQuery_Execute
        ("append('" &
         Escape_Quotes ("<script src='/js/jquery.mnmenu.js'" & " type='text/javascript' charset='utf-8'></script>") &
         "')");
   end Load_MNMenu;

   ------------------
   -- Add_Menu_Bar --
   ------------------

   procedure Add_Menu_Bar
     (Menu   : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type;
      Parent : in out Gnoga.Gui.Element.Element_Type'Class;
      ID     : in     String := "")
   is
   begin
      Menu.Create (Parent, ID);
   end Add_Menu_Bar;

   ------------------
   -- New_Menu_Bar --
   ------------------

   procedure New_Menu_Bar
     (View   :        not null access Gnoga.Gui.View.View_Base_Type'Class;
      Name   : in     String;
      Menu   :    out Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      Parent :        not null access Gnoga.Gui.Element.Element_Type'Class;
      ID     : in     String := "")
   is
   begin
      Menu := Gnoga.Gui.Plugin.MNMenu.MNMenu_Access (View.New_Element (Name, new Gnoga.Gui.Plugin.MNMenu.MNMenu_Type));
      Menu.Create (Parent.all, ID);
      Menu.View := Gnoga.Gui.View.Pointer_To_View_Base_Class (View);
   end New_Menu_Bar;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Menu : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type;
      Name : in     String;
      Item : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type'Class;
      ID   : in     String := "")
   is
   begin
      Item.Create (Menu, Name, ID);
   end Add_Item;

   --------------
   -- New_Item --
   --------------

   procedure New_Item
     (Menu : in     not null Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      Name : in     String;
      Item :    out Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access;
      ID   : in     String := "")
   is
   begin
      Item :=
        Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
          (Menu.View.New_Element (Name, new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type));
      Item.Create (Menu.all, Name, ID);
   end New_Item;

   ------------------
   -- Add_Sub_Menu --
   ------------------

   procedure Add_Sub_Menu
     (Menu     : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type;
      Item     : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type'Class;
      Sub_Menu : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type'Class;
      ID       : in     String := "")
   is
   begin
      Sub_Menu.Create (Menu, ID);
      Sub_Menu.Place_Inside_Bottom_Of (Item);
   end Add_Sub_Menu;

   -----------------------
   -- Add_Sub_Menu_Item --
   -----------------------

   procedure Add_Sub_Menu_Item
     (Menu     : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type;
      Name     : in     String;
      Item     : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type'Class;
      Sub_Menu : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type'Class;
      ID       : in     String := "")
   is
   begin
      Item.Create (Menu, Name, ID);
      Sub_Menu.Create (Menu, ID);
      Sub_Menu.Place_Inside_Bottom_Of (Item);
   end Add_Sub_Menu_Item;

   ------------------
   -- Display_Menu --
   ------------------

   procedure Display_Menu (Menu : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type) is
   begin
      Menu.jQuery_Execute ("mnmenu()");
   end Display_Menu;

   ------------------
   -- New_Sub_Menu --
   ------------------

   procedure New_Sub_Menu
     (Menu     : in     not null Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      Item     : in     not null Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access;
      Sub_Menu :    out Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      ID       : in     String := "")
   is
   begin
      Sub_Menu :=
        Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
          (Menu.View.New_Element (Item.Text, new Gnoga.Gui.Plugin.MNMenu.MNMenu_Type));
      Sub_Menu.Create (Menu.all, ID);
      Sub_Menu.Place_Inside_Bottom_Of (Item.all);
      Sub_Menu.View := Menu.View;
   end New_Sub_Menu;

   -----------------------
   -- New_Sub_Menu_Item --
   -----------------------

   procedure New_Sub_Menu_Item
     (Menu     : in     not null Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      Name     : in     String;
      Sub_Menu :    out Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      ID       : in     String := "")
   is
      Item : Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access;
   begin
      Item :=
        Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
          (Menu.View.New_Element (Name, new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type));
      Item.Create (Menu.all, Name, ID);
      Sub_Menu :=
        Gnoga.Gui.Plugin.MNMenu.MNMenu_Access (Menu.View.New_Element (Name, new Gnoga.Gui.Plugin.MNMenu.MNMenu_Type));
      Sub_Menu.Create (Menu.all, ID);
      Sub_Menu.Place_Inside_Bottom_Of (Item.all);
      Sub_Menu.View := Menu.View;
   end New_Sub_Menu_Item;

   ------------------
   -- Display_Menu --
   ------------------

   procedure Display_Menu (Menu : in not null Gnoga.Gui.Plugin.MNMenu.MNMenu_Access) is
   begin
      Menu.jQuery_Execute ("mnmenu()");
   end Display_Menu;

end Gnoga.Gui.Plugin.MNMenu;
