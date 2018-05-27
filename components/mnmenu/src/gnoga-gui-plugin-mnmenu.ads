------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . P L U G I N . M N M E N U             --
--                                                                          --
--                                 S p e c                                  --
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

--  MNMenu Ada API is inspired from a JQuery plugin to create dropdown menus from <ul> lists.
--  https://github.com/manusa/mnmenu
--  MNMenu.js is released under the (http://opensource.org/licenses/MIT) MIT License.
--  Both static and dynamic set of API are proposed. Don't mix them.

with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.List;

package Gnoga.Gui.Plugin.MNMenu is

   procedure Load_MNMenu (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load MNMenu code into window

   -------------------------------------------------------------------------
   --  MNMenu_Types
   -------------------------------------------------------------------------

   type MNMenu_Type is
     new Gnoga.Gui.Element.List.Unordered_List_Type with private;
   type MNMenu_Access is access all MNMenu_Type;
   type Pointer_To_MNMenu_Class is access all MNMenu_Type'Class;

   -------------------------------------------------------------------------
   --  MNMenu_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Add_Menu_Bar
     (Menu   : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type;
      Parent : in out Gnoga.Gui.Element.Element_Type'Class;
      ID     : in     String := "");
   --  Main menu creation.

   procedure New_Menu_Bar
     (View   :        not null access Gnoga.Gui.View.View_Base_Type'Class;
      Name   : in     String;
      Menu   :    out Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      Parent :        not null access Gnoga.Gui.Element.Element_Type'Class;
      ID     : in     String := "");
   --  Main menu bar creation. Only use for dynamic objects.

   -------------------------------------------------------------------------
   --  MNMenu_Item_Types
   -------------------------------------------------------------------------

   type MNMenu_Item_Type is
     new Gnoga.Gui.Element.List.List_Item_Type with private;
   type MNMenu_Item_Access is access all MNMenu_Item_Type;
   type Pointer_To_MNMenu_Item_Class is access all MNMenu_Item_Type'Class;

   -------------------------------------------------------------------------
   --  MNMenu_Item_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Add_Item
     (Menu : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type;
      Name : in     String;
      Item : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type'Class;
      ID   : in     String := "");
   --  Menu item creation.

   procedure New_Item
     (Menu : in     not null Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      Name : in     String;
      Item :    out Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access;
      ID   : in     String := "");
   --  Menu item creation. Only use for dynamic objects.

   -------------------------------------------------------------------------
   --  MNMenu_Type - Methods
   -------------------------------------------------------------------------

   procedure Add_Sub_Menu
     (Menu     : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type;
      Item     : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type'Class;
      Sub_Menu : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type'Class;
      ID       : in     String := "");
   --  Submenu creation from a menu item.

   procedure Add_Sub_Menu_Item
     (Menu     : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type;
      Name     : in     String;
      Item     : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type'Class;
      Sub_Menu : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type'Class;
      ID       : in     String := "");
   --  Submenu creation together with item.

   procedure Display_Menu (Menu : in out Gnoga.Gui.Plugin.MNMenu.MNMenu_Type);
   --  Menu shape display

   procedure New_Sub_Menu
     (Menu     : in     not null Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      Item     : in     not null Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access;
      Sub_Menu :    out Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      ID       : in     String := "");
   --  Submenu creation from a menu item. Only use for dynamic objects.

   procedure New_Sub_Menu_Item
     (Menu     : in     not null Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      Name     : in     String;
      Sub_Menu :    out Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      ID       : in     String := "");
   --  Submenu creation together with item. Only use for dynamic objects.

   procedure Display_Menu
     (Menu : in not null Gnoga.Gui.Plugin.MNMenu.MNMenu_Access);
   --  Menu shape display

private

   type MNMenu_Type is new Gnoga.Gui.Element.List.Unordered_List_Type with
   record
      View : Gnoga.Gui.View.Pointer_To_View_Base_Class;
   end record;

   type MNMenu_Item_Type is new Gnoga.Gui.Element.List.List_Item_Type with
   null record;

end Gnoga.Gui.Plugin.MNMenu;
