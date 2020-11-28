------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--             G N O G A . G U I . P L U G I N . J Q U E R Y U I            --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;

with Gnoga.Server.Connection;

with Gnoga.Gui.Element.Style_Block;

package body Gnoga.Gui.Plugin.jQueryUI is

   procedure jQueryUI_Effect
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Effect_Method      : in     String;
      Effect_Name        : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing";
      Options            : in     String  := "");
   --  Apply Effect_Method using these parameters to Element

   -------------------
   -- Load_jQueryUI --
   -------------------

   procedure Load_jQueryUI (Window : in out Gnoga.Gui.Window.Window_Type'Class) is
      Delay_Count : Natural := 0;
   begin
      Window.Document.Head_Element.jQuery_Execute
        ("append ('" & Escape_Quotes ("<link rel='stylesheet' " & "href='/css/jquery-ui.min.css' />") & "')");
      Window.Document.Head_Element.jQuery_Execute
        ("append ('" & Escape_Quotes ("<script src='/js/jquery-ui.min.js' />") & "')");

      while Gnoga.Server.Connection.Execute_Script (Window.Connection_ID, "$.ui") = "undefined" or Delay_Count > 10 loop
         delay 0.25;
         Delay_Count := Delay_Count + 1;
      end loop;

      if Delay_Count > 10 then
         raise Failed_To_Load_jQueryUI;
      end if;
   end Load_jQueryUI;

   --------------------
   -- Make_Draggable --
   --------------------

   procedure Make_Draggable (Element : in out Gnoga.Gui.Element.Element_Type'Class) is
   begin
      Element.jQuery_Execute ("draggable()");
   end Make_Draggable;

   --------------------
   -- Make_Droppable --
   --------------------

   procedure Make_Droppable (Element : in out Gnoga.Gui.Element.Element_Type'Class) is
      US : constant String := Element.Unique_ID'Img;

      Full_Message : constant String :=
        US (US'First + 1 .. US'Last) & "|" & jQuery_Dropped_Event_Name & "|" & Element.ID;
   begin
      Element.jQuery_Execute
        ("droppable({ drop: function(event, ui) {" & "ws.send ('" & Escape_Quotes (Full_Message) & "');}})");
   end Make_Droppable;

   --------------------
   -- Make_Resizable --
   --------------------

   procedure Make_Resizable (Element : in out Gnoga.Gui.Element.Element_Type'Class) is
   begin
      Element.jQuery_Execute ("resizable()");
   end Make_Resizable;

   -------------------
   -- Make_Sortable --
   -------------------

   procedure Make_Sortable (List : in out Gnoga.Gui.Element.List.Ordered_List_Type'Class) is
   begin
      List.jQuery_Execute ("sortable()");
   end Make_Sortable;

   ---------------------
   -- Make_Selectable --
   ---------------------

   procedure Make_Selectable
     (List            : in out Gnoga.Gui.Element.List.Ordered_List_Type'Class;
      Selecting_Color : in     String := "#ccc";
      Selected_Color  : in     String := "#999")
   is
      S : Gnoga.Gui.Element.Style_Block.Style_Type;
   begin
      S.Create
        (List.Parent.all,
         List.DOM_Selector & " .ui-selecting { background: " & Selecting_Color & "; } " & List.DOM_Selector &
         " .ui-selected { background: " & Selected_Color & "; } ");

      List.jQuery_Execute ("selectable()");
   end Make_Selectable;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
     (Item : Gnoga.Gui.Element.List.List_Item_Type'Class)
      return Boolean
   is
   begin
      return Ada.Strings.Fixed.Index (Item.Class_Name, "ui-selected") > 0;
   end Is_Selected;

   -----------------------------
   -- Add_Class_Name_Animated --
   -----------------------------

   procedure Add_Class_Name_Animated
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Class_Name         : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing")
   is
   begin
      Element.jQuery_Execute ("addClass('" & Class_Name & "'," & Animation_Duration'Img & ",'" & Easing & "')");
   end Add_Class_Name_Animated;

   --------------------------------
   -- Remove_Class_Name_Animated --
   --------------------------------

   procedure Remove_Class_Name_Animated
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Class_Name         : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing")
   is
   begin
      Element.jQuery_Execute ("removeClass('" & Class_Name & "'," & Animation_Duration'Img & ",'" & Easing & "')");
   end Remove_Class_Name_Animated;

   --------------------------------
   -- Toggle_Class_Name_Animated --
   --------------------------------

   procedure Toggle_Class_Name_Animated
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Class_Name         : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing")
   is
   begin
      Element.jQuery_Execute ("toggleClass('" & Class_Name & "'," & Animation_Duration'Img & ",'" & Easing & "')");
   end Toggle_Class_Name_Animated;

   --------------------------------
   -- Switch_Class_Name_Animated --
   --------------------------------

   procedure Switch_Class_Name_Animated
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Old_Class_Name     : in     String;
      New_Class_Name     : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing")
   is
   begin
      Element.jQuery_Execute
        ("switchClass('" & Old_Class_Name & "','" & New_Class_Name & "'," & Animation_Duration'Img & ",'" & Easing &
         "')");
   end Switch_Class_Name_Animated;

   ---------------------
   -- jQueryUI_Effect --
   ---------------------

   procedure jQueryUI_Effect
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Effect_Method      : in     String;
      Effect_Name        : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing";
      Options            : in     String  := "")
   is
      function Has_Options return String;

      function Has_Options return String is
      begin
         if Options /= "" then
            return ", " & Escape_Quotes (Options);
         else
            return "";
         end if;
      end Has_Options;
   begin
      Element.jQuery_Execute
        (Effect_Method & "('" & Effect_Name & "'," & "{ easing: '" & Easing & "'" & Has_Options & " }," &
         Animation_Duration'Img & ")");
   end jQueryUI_Effect;

   ------------------
   -- Apply_Effect --
   ------------------

   procedure Apply_Effect
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Effect_Name        : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing";
      Options            : in     String  := "")
   is
   begin
      jQueryUI_Effect (Element, "effect", Effect_Name, Animation_Duration, Easing, Options);
   end Apply_Effect;

   ------------------------
   -- Toggle_With_Effect --
   ------------------------

   procedure Toggle_With_Effect
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Effect_Name        : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing";
      Options            : in     String  := "")
   is
   begin
      jQueryUI_Effect (Element, "toggle", Effect_Name, Animation_Duration, Easing, Options);
   end Toggle_With_Effect;

   ----------------------
   -- Show_With_Effect --
   ----------------------

   procedure Show_With_Effect
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Effect_Name        : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing";
      Options            : in     String  := "")
   is
   begin
      jQueryUI_Effect (Element, "show", Effect_Name, Animation_Duration, Easing, Options);
   end Show_With_Effect;

   ----------------------
   -- Hide_With_Effect --
   ----------------------

   procedure Hide_With_Effect
     (Element            : in out Gnoga.Gui.Element.Element_Type'Class;
      Effect_Name        : in     String;
      Animation_Duration : in     Natural := 400;
      Easing             : in     String  := "swing";
      Options            : in     String  := "")
   is
   begin
      jQueryUI_Effect (Element, "hide", Effect_Name, Animation_Duration, Easing, Options);
   end Hide_With_Effect;

   --------------
   -- Position --
   --------------

   procedure Position
     (Element      : in out Gnoga.Gui.Element.Element_Type'Class;
      Target       : in out Gnoga.Gui.Base.Base_Type'Class;
      Using_My     : in     String := "center";
      At_Target    : in     String := "center";
      On_Collision : in     String := "flip")
   is
   begin
      Element.jQuery_Execute
        ("position({" & "my: '" & Using_My & "'," & "at: '" & At_Target & "'," & "of: " & Target.jQuery & "," &
         "collision: '" & On_Collision & "'" & "});");
   end Position;

   procedure Position
     (Element      : in out Gnoga.Gui.Element.Element_Type'Class;
      X, Y         : in     Integer;
      Using_My     : in     String := "center";
      On_Collision : in     String := "flip")
   is
   begin
      Element.jQuery_Execute
        ("position({" & "my: '" & Using_My & "'," & "of: new MouseEvent ('click'," & " {'clientX':" & X'Img & "," &
         "  'clientY':" & Y'Img & "," & "  'view': window}), " & "collision: '" & On_Collision & "'" & "});");
   end Position;

end Gnoga.Gui.Plugin.jQueryUI;
