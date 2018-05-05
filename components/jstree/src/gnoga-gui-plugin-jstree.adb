------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . P L U G I N . J S T R E E             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2018 Pascal Pignard                    --
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

package body Gnoga.Gui.Plugin.JSTree is

   -----------------
   -- Load_JSTree --
   -----------------

   function Build_Options (Options : Option_Type) return String;
   function Build_Options (Options : Option_Type) return String is
      Plugins : constant String :=
        (if Options.Plugins (CheckBox) then "'checkbox'," else "") &
        (if Options.Plugins (ContextMenu) then "'contextmenu'," else "") &
        (if Options.Plugins (DragAndDrop) then "'dnd'," else "") &
        (if Options.Plugins (Sort) then "'sort'," else "") &
        (if Options.Plugins (Unique) then "'unique'," else "") &
        (if Options.Plugins (WholeRow) then "'wholerow'," else "");
   begin
      return '{' &
        "'core':{" &
        (if
           Options.Plugins (ContextMenu) or Options.Plugins (DragAndDrop)
         then
           "'check_callback':true,"
         else "") &
        "'animation':" &
        Options.Core.Animation'Img &
        ',' &
        "'multiple':" &
        Options.Core.Multiple'Img &
        ',' &
        "'force_text':" &
        Options.Core.Force_Text'Img &
        ',' &
        "'dblclick_toggle':" &
        Options.Core.DblClick_Toggle'Img &
        ',' &
        "'themes':{" &
        "'dots':" &
        Options.Core.Themes.Dots'Img &
        ',' &
        "'icons':" &
        Options.Core.Themes.Icons'Img &
        ',' &
        "'stripes':" &
        Options.Core.Themes.Stripes'Img &
        ',' &
        '}' &
        "}," &
        "'plugins':[" &
        (if Plugins'Length > 0 then Plugins (Plugins'First .. Plugins'Last - 1)
         else "") &
        ']' &
        '}';
   end Build_Options;

   -----------------
   -- Load_JSTree --
   -----------------

   procedure Load_JSTree
     (Window : in out Gnoga.Gui.Window.Window_Type'Class)
   is
   begin
      Window.Document.Head_Element.jQuery_Execute
      ("append('" &
       Escape_Quotes
         ("<link href='/css/jstree_themes/default/style.min.css'" &
          " type='text/css' rel='stylesheet'>") &
       "')");
      Window.Document.Head_Element.jQuery_Execute
      ("append('" &
       Escape_Quotes
         ("<script src='/js/jstree.min.js'" &
          " type='text/javascript' charset='utf-8'></script>") &
       "')");
   end Load_JSTree;

   -------------------
   -- Add_Root_Tree --
   -------------------

   procedure Add_Root_Tree
     (Tree   : in out JSTree_Type;
      Parent : in out Gnoga.Gui.Element.Element_Type'Class;
      ID     : in     String := "")
   is
   begin
      Tree.View := new Gnoga.Gui.View.View_Type;
      Tree.View.Create (Parent, ID);
      Tree.Create (Tree.View.all);
   end Add_Root_Tree;

   -------------------
   -- New_Root_Tree --
   -------------------

   procedure New_Root_Tree
     (Tree   :    out JSTree_Access;
      Name   : in     String;
      Parent : in out Gnoga.Gui.Element.Element_Type'Class;
      ID     : in     String := "")
   is
   begin
      Tree      := new JSTree_Type;
      Tree.View := new Gnoga.Gui.View.View_Type;
      Tree.View.Create (Parent, ID);
      JSTree_Access (Tree.View.New_Element (Name, Tree)).Create
      (Tree.View.all);
   end New_Root_Tree;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Tree     : in out JSTree_Type;
      Name     : in     String;
      Item     : in out JSTree_Item_Type'Class;
      ID       : in     String  := "";
      Icon     : in     String  := "";
      Selected : in     Boolean := False;
      Opened   : in     Boolean := False;
      Disabled : in     Boolean := False)
   is
      Attributes : constant String :=
        (if Icon /= "" then """icon"":""" & Icon & """," else "") &
        (if Selected then """selected"":true," & Icon else "") &
        (if Opened then """opened"":true," & Icon else "") &
        (if Disabled then """disabled"":true," & Icon else "");
   begin
      Item.Create (Tree, Name, ID);
      Item.Attribute
      ("data-jstree", '{' &
       (if
          Attributes'Length > 0
        then
          Attributes (Attributes'First .. Attributes'Last - 1)
        else "") &
       '}');
   end Add_Item;

   --------------
   -- New_Item --
   --------------

   procedure New_Item
     (Tree     :        not null access JSTree_Type;
      Name     : in     String;
      Item     :    out JSTree_Item_Access;
      ID       : in     String  := "";
      Icon     : in     String  := "";
      Selected : in     Boolean := False;
      Opened   : in     Boolean := False;
      Disabled : in     Boolean := False)
   is
   begin
      Item :=
        JSTree_Item_Access
          (Tree.View.New_Element (Name, new JSTree_Item_Type));
      Add_Item
        (Tree.all,
         Name,
         Item.all,
         ID,
         Icon,
         Selected,
         Opened,
         Disabled);
   end New_Item;

   ------------------
   -- Add_Sub_Tree --
   ------------------

   procedure Add_Sub_Tree
     (Tree     : in out JSTree_Type;
      Item     : in out JSTree_Item_Type'Class;
      Sub_Tree : in out JSTree_Type'Class;
      ID       : in     String := "")
   is
   begin
      Sub_Tree.Create (Tree, ID);
      Sub_Tree.Place_Inside_Bottom_Of (Item);
      Sub_Tree.View := Tree.View;
   end Add_Sub_Tree;

   -----------------------
   -- Add_Sub_Tree_Item --
   -----------------------

   procedure Add_Sub_Tree_Item
     (Tree     : in out JSTree_Type;
      Name     : in     String;
      Item     : in out JSTree_Item_Type'Class;
      Sub_Tree : in out JSTree_Type'Class;
      ID       : in     String  := "";
      Icon     : in     String  := "";
      Selected : in     Boolean := False;
      Opened   : in     Boolean := False;
      Disabled : in     Boolean := False)
   is
   begin
      Add_Item (Tree, Name, Item, ID, Icon, Selected, Opened, Disabled);
      Sub_Tree.Create (Tree, ID);
      Sub_Tree.Place_Inside_Bottom_Of (Item);
   end Add_Sub_Tree_Item;

   ------------------
   -- Display_Tree --
   ------------------

   procedure Display_Tree
     (Tree    : in out JSTree_Type;
      Options : in     Option_Type := (others => <>))
   is
   begin
      Tree.View.jQuery_Execute ("jstree(" & Build_Options (Options) & ")");
   end Display_Tree;

   ------------------
   -- New_Sub_Tree --
   ------------------

   procedure New_Sub_Tree
     (Tree     :        not null access JSTree_Type;
      Item     : in     not null JSTree_Item_Access;
      Sub_Tree :    out JSTree_Access;
      ID       : in     String := "")
   is
   begin
      Sub_Tree :=
        JSTree_Access (Tree.View.New_Element (Item.Text, new JSTree_Type));
      Add_Sub_Tree (Tree.all, Item.all, Sub_Tree.all, ID);
   end New_Sub_Tree;

   -----------------------
   -- New_Sub_Tree_Item --
   -----------------------

   procedure New_Sub_Tree_Item
     (Tree     :        not null access JSTree_Type;
      Name     : in     String;
      Sub_Tree :    out JSTree_Access;
      ID       : in     String  := "";
      Icon     : in     String  := "";
      Selected : in     Boolean := False;
      Opened   : in     Boolean := False;
      Disabled : in     Boolean := False)
   is
      Item : JSTree_Item_Access;
   begin
      Item :=
        JSTree_Item_Access
          (Tree.View.New_Element (Name, new JSTree_Item_Type));
      Add_Item
        (Tree.all,
         Name,
         Item.all,
         ID,
         Icon,
         Selected,
         Opened,
         Disabled);
      Sub_Tree :=
        JSTree_Access (Tree.View.New_Element (Name, new JSTree_Type));
      Add_Sub_Tree (Tree.all, Item.all, Sub_Tree.all, ID);
   end New_Sub_Tree_Item;

   ------------------
   -- Display_Tree --
   ------------------

   procedure Display_Tree
     (Tree    :    not null access JSTree_Type;
      Options : in Option_Type := (others => <>))
   is
   begin
      Tree.View.jQuery_Execute ("jstree(" & Build_Options (Options) & ")");
   end Display_Tree;

end Gnoga.Gui.Plugin.JSTree;
