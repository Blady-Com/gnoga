------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--             G N O G A . G U I . P L U G I N . J Q U E R Y U I            --
--                                                                          --
--                                 S p e c                                  --
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
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.List;

package Gnoga.Gui.Plugin.jQueryUI is

   -------------------------------------------------------------------------
   --  jQueryUI Plug In
   -------------------------------------------------------------------------
   --  http://jQueryUI.com
   --  Binding to jQueryUI

   Failed_To_Load_jQueryUI : exception;

   -------------------------------------------------------------------------
   --  jQueryUI Library
   -------------------------------------------------------------------------

   procedure Load_jQueryUI
     (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load jQueryUI CSS and Scripting code in to Window
   --  This proceure loads them from Google's CDN
   --  You can also modify boot.html to include the jQueryUI CSS
   --  and scripting code so that you can customize themes, etc.

   -------------------------------------------------------------------------
   --  jQueryUI Interactions
   -------------------------------------------------------------------------

   procedure Make_Draggable
     (Element : in out Gnoga.Gui.Element.Element_Type'Class);

   jQuery_Dropped_Event_Name : constant String := "jqueryui_dropped";

   procedure Make_Droppable
     (Element : in out Gnoga.Gui.Element.Element_Type'Class);
   --  To receive an On_Drop, bind the On_Message of Element
   --  check for Event = jQuery_Dropped_Event_Name
   --  Message will be the Element.ID

   procedure Make_Resizable
     (Element : in out Gnoga.Gui.Element.Element_Type'Class);

   procedure Make_Sortable
     (List : in out Gnoga.Gui.Element.List.Ordered_List_Type'Class);

   procedure Make_Selectable
     (List            : in out Gnoga.Gui.Element.List.Ordered_List_Type'Class;
      Selecting_Color : in     String := "#ccc";
      Selected_Color  : in     String := "#999");

   function Is_Selected (Item : Gnoga.Gui.Element.List.List_Item_Type'Class)
                         return Boolean;

   -------------------------------------------------------------------------
   --  jQueryUI Effects
   -------------------------------------------------------------------------

   procedure Add_Class_Name_Animated
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Class_Name         : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing");

   procedure Remove_Class_Name_Animated
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Class_Name         : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing");

   procedure Toggle_Class_Name_Animated
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Class_Name         : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing");

   procedure Switch_Class_Name_Animated
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Old_Class_Name     : in     String;
      New_Class_Name     : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing");

   procedure Apply_Effect
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Effect_Name        : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing";
      Options            : in     String  := "");

   procedure Toggle_With_Effect
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Effect_Name        : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing";
      Options            : in     String  := "");

   procedure Show_With_Effect
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Effect_Name        : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing";
      Options            : in     String  := "");

   procedure Hide_With_Effect
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Effect_Name        : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing";
      Options            : in     String  := "");

   -------------------------------------------------------------------------
   --  jQueryUI Utilities
   -------------------------------------------------------------------------

   procedure Position
     (Element      : in out Gnoga.Gui.Element.Element_Type'Class;
      Target       : in out Gnoga.Gui.Base.Base_Type'Class;
      Using_My     : in     String := "center";
      At_Target    : in     String := "center";
      On_Collision : in     String := "flip");

   procedure Position
     (Element      : in out Gnoga.Gui.Element.Element_Type'Class;
      X, Y         : in     Integer;
      Using_My     : in     String := "center";
      On_Collision : in     String := "flip");
   --  Position Element at target location of X,Y (page coordinates)

end Gnoga.Gui.Plugin.jQueryUI;
