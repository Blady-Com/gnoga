------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . P L U G I N . J S T R E E             --
--                                                                          --
--                                 S p e c                                  --
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

--  JSTree Ada API is inspired from a JQuery plugin to create dropdown trees from <ul> lists.
--  https://github.com/vakata/jstree
--  JSTree is released under the (http://opensource.org/licenses/MIT) MIT License.
--  Some comments come from JSTree documentation.
--  Both static and dynamic set of API are proposed. Don't mix them.

with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.List;
with Gnoga.Gui.Base;

package Gnoga.Gui.Plugin.JSTree is

   procedure Load_JSTree (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load JSTree code into window

   -------------------------------------------------------------------------
   --  JSTree_Types
   -------------------------------------------------------------------------

   type JSTree_Type is
     new Gnoga.Gui.Element.List.Unordered_List_Type with private;
   type JSTree_Access is access all JSTree_Type;
   type Pointer_To_JSTree_Class is access all JSTree_Type'Class;

   type Themes_Type is record
      Dots : Boolean := True;
      --  a boolean indicating if connecting dots are shown
      Icons : Boolean := True;
      --  a boolean indicating if node icons are shown
      Stripes : Boolean := False;
      --  a boolean indicating if the tree background is striped
   end record;

   type Core_Type is record
      Animation : Natural := 200;
      --  the open / close animation duration in milliseconds
      Multiple : Boolean := True;
      --  a boolean indicating if multiple nodes can be selected
      Force_Text : Boolean := False;
      --  Force node text to plain text (and escape HTML)
      DblClick_Toggle : Boolean := True;
      --  Should the node should be toggled if the text is double clicked
      Themes : Themes_Type;
   end record;

   type Plugins_Enum is
     (CheckBox,
   --  Renders a checkbox icon in front of each node, making multiselection easy.
    ContextMenu,
   --  Makes it possible to right click nodes and shows a list of
   --  configurable actions in a menu.
    DragAndDrop,
   --  Makes it possible to drag and drop tree nodes and rearrange the tree.
    Sort,
   --  Automatically arranges all sibling nodes according to a comparison function,
   --  which defaults to alphabetical order.
    Unique,
   --  Enforces that no nodes with the same name can coexist as siblings
   --  prevents renaming and moving nodes to a parent,
   --  which already contains a node with the same name.
    WholeRow
   --  Makes each node appear block level which makes selection easier.
   --  May cause slow down for large trees in old browsers.
   );

   type Plugins_Type is array (Plugins_Enum) of Boolean with
        Default_Component_Value => False;

   type Option_Type is record
      Core    : Core_Type;
      Plugins : Plugins_Type;
   end record;

   type JSTree_Event is access procedure
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Node   : in     String);

   -------------------------------------------------------------------------
   --  JSTree_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Add_Root_Tree
     (Tree   : in out JSTree_Type;
      Parent : in out Gnoga.Gui.Element.Element_Type'Class;
      ID     : in     String := "");
   --  Main tree creation.

   procedure New_Root_Tree
     (Tree   :    out JSTree_Access;
      Name   : in     String;
      Parent : in out Gnoga.Gui.Element.Element_Type'Class;
      ID     : in     String := "");
   --  Main tree bar creation. Only use for dynamic objects.

   -------------------------------------------------------------------------
   --  JSTree_Item_Types
   -------------------------------------------------------------------------

   type JSTree_Item_Type is
     new Gnoga.Gui.Element.List.List_Item_Type with private;
   type JSTree_Item_Access is access all JSTree_Item_Type;
   type Pointer_To_JSTree_Item_Class is access all JSTree_Item_Type'Class;

   -------------------------------------------------------------------------
   --  JSTree_Item_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Add_Item
     (Tree     : in out JSTree_Type;
      Name     : in     String;
      Item     : in out JSTree_Item_Type'Class;
      ID       : in     String  := "";
      Icon     : in     String  := "";
      Selected : in     Boolean := False;
      Opened   : in     Boolean := False;
      Disabled : in     Boolean := False);
   --  Tree item creation.

   procedure New_Item
     (Tree     :        not null access JSTree_Type;
      Name     : in     String;
      Item     :    out JSTree_Item_Access;
      ID       : in     String  := "";
      Icon     : in     String  := "";
      Selected : in     Boolean := False;
      Opened   : in     Boolean := False;
      Disabled : in     Boolean := False);
   --  Tree item creation. Only use for dynamic objects.

   -------------------------------------------------------------------------
   --  JSTree_Type - Methods
   -------------------------------------------------------------------------

   procedure Add_Sub_Tree
     (Tree     : in out JSTree_Type;
      Item     : in out JSTree_Item_Type'Class;
      Sub_Tree : in out JSTree_Type'Class;
      ID       : in     String := "");
   --  Subtree creation from a tree item.

   procedure Add_Sub_Tree_Item
     (Tree     : in out JSTree_Type;
      Name     : in     String;
      Item     : in out JSTree_Item_Type'Class;
      Sub_Tree : in out JSTree_Type'Class;
      ID       : in     String  := "";
      Icon     : in     String  := "";
      Selected : in     Boolean := False;
      Opened   : in     Boolean := False;
      Disabled : in     Boolean := False);
   --  Subtree creation togather with item.

   procedure Display_Tree
     (Tree    : in out JSTree_Type;
      Options : in     Option_Type := (others => <>));
   --  Tree shape display

   procedure New_Sub_Tree
     (Tree     :        not null access JSTree_Type;
      Item     : in     not null JSTree_Item_Access;
      Sub_Tree :    out JSTree_Access;
      ID       : in     String := "");
   --  Subtree creation from a tree item. Only use for dynamic objects.

   procedure New_Sub_Tree_Item
     (Tree     :        not null access JSTree_Type;
      Name     : in     String;
      Sub_Tree :    out JSTree_Access;
      ID       : in     String  := "";
      Icon     : in     String  := "";
      Selected : in     Boolean := False;
      Opened   : in     Boolean := False;
      Disabled : in     Boolean := False);
   --  Subtree creation togather with item. Only use for dynamic objects.

   procedure Display_Tree
     (Tree    :    not null access JSTree_Type;
      Options : in Option_Type := (others => <>));
   --  Tree shape display

   -------------------------------------------------------------------------
   --  JSTree_Type - Event Handlers
   -------------------------------------------------------------------------

   procedure On_Open_Node_Handler
     (Tree    : in out JSTree_Type;
      Handler : in     JSTree_Event);
   procedure Fire_On_Open_Node (Tree : in out JSTree_Type; Node : in String);

   procedure On_Close_Node_Handler
     (Tree    : in out JSTree_Type;
      Handler : in     JSTree_Event);
   procedure Fire_On_Close_Node (Tree : in out JSTree_Type; Node : in String);

   procedure On_Select_Node_Handler
     (Tree    : in out JSTree_Type;
      Handler : in     JSTree_Event);
   procedure Fire_On_Select_Node (Tree : in out JSTree_Type; Node : in String);

   procedure On_Deselect_Node_Handler
     (Tree    : in out JSTree_Type;
      Handler : in     JSTree_Event);
   procedure Fire_On_Deselect_Node
     (Tree : in out JSTree_Type;
      Node : in     String);

   procedure On_Check_Node_Handler
     (Tree    : in out JSTree_Type;
      Handler : in     JSTree_Event);
   procedure Fire_On_Check_Node (Tree : in out JSTree_Type; Node : in String);

   procedure On_Uncheck_Node_Handler
     (Tree    : in out JSTree_Type;
      Handler : in     JSTree_Event);
   procedure Fire_On_Uncheck_Node
     (Tree : in out JSTree_Type;
      Node : in     String);

private

   type JSTree_View_Type is new Gnoga.Gui.View.View_Type with record
      On_Open_Node_Event     : JSTree_Event  := null;
      On_Close_Node_Event    : JSTree_Event  := null;
      On_Select_Node_Event   : JSTree_Event  := null;
      On_Deselect_Node_Event : JSTree_Event  := null;
      On_Check_Node_Event    : JSTree_Event  := null;
      On_Uncheck_Node_Event  : JSTree_Event  := null;
      Parent_Tree            : JSTree_Access := null;
   end record;

   overriding procedure On_Message
     (Object  : in out JSTree_View_Type;
      Event   : in     String;
      Message : in     String);
   --  Called on receiving any message or event from browser.

   type JSTree_Type is new Gnoga.Gui.Element.List.Unordered_List_Type with
   record
      View : access JSTree_View_Type;
   end record;

   type JSTree_Item_Type is new Gnoga.Gui.Element.List.List_Item_Type with
   null record;

end Gnoga.Gui.Plugin.JSTree;
