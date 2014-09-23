------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                          G N O G A . B A S E                             --
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
------------------------------------------------------------------------------                                                                          --

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Gnoga.Connections;

package body Gnoga.Base is

   Mouse_Event_Script : constant String :=
     "e.clientX + '|' + e.clientY + '|' + e.screenX + '|' + " &
     "e.screenY + '|' + e.which + '|' + e.altKey + '|' + " &
     "e.ctrlKey + '|' + e.shiftKey + '|' + e.metaKey + '|'";
   --  e.buttons would be better but not supported currently outside
   --  of firefox and would always return 0 on Mac so using e.which.

   Keyboard_Event_Script : constant String :=
     "e.which + '|' + e.altKey + '|' + e.ctrlKey + '|' + e.shiftKey + '|' + " &
     "e.metaKey + '|'";

   function Parse_Mouse_Event (Message : String) return Mouse_Event_Record;
   --  Parse event message in to Mouse_Event_Record

   function Parse_Keyboard_Event (Message : String) return Keyboard_Event_Record;
   --  Parse event message in to Keyboard_Event_Record

   -----------------------
   -- Parse_Mouse_Event --
   -----------------------

   function Parse_Mouse_Event (Message : String) return Mouse_Event_Record is
      use Ada.Strings.Fixed;

      Event  : Mouse_Event_Record;
      S      : Integer := Message'First;
      F      : Integer := Message'First - 1;
      Button : Integer;

      function Split return String is
      begin
         S := F + 1;
         F := Index (Source  => Message,
                     Pattern => "|",
                     From    => S);
         return Message (S .. (F - 1));
      end;

      function Split return Integer is
      begin
         return Integer'Value (Split);
      end Split;

      function Split return Boolean is
      begin
         return Split = "true";
      end Split;

      No_Button     : constant := 0;
      Left_Button   : constant := 1;
      Middle_Button : constant := 2;
      Right_Button  : constant := 3;
   begin
      Event.X := Split;
      Event.Y := Split;
      Event.Screen_X := Split;
      Event.Screen_Y := Split;

      Button := Split;
      Event.Left_Button := Button = Left_Button;
      Event.Middle_Button := Button = Middle_Button;
      Event.Right_Button := Button = Right_Button;

      Event.Alt := Split;
      Event.Control := Split;
      Event.Shift := Split;
      Event.Meta := Split;

      return Event;
   end Parse_Mouse_Event;

   --------------------------
   -- Parse_Keyboard_Event --
   --------------------------

   function Parse_Keyboard_Event (Message : String) return Keyboard_Event_Record is
      use Ada.Strings.Fixed;

      Event  : Keyboard_Event_Record;
      S      : Integer := Message'First;
      F      : Integer := Message'First - 1;

      function Split return String is
      begin
         S := F + 1;
         F := Index (Source  => Message,
                     Pattern => "|",
                     From    => S);
         return Message (S .. (F - 1));
      end;

      function Split return Integer is
      begin
         return Integer'Value (Split);
      end Split;

      function Split return Boolean is
      begin
         return Split = "true";
      end Split;
   begin
      Event.Key_Code := Split;
      Event.Alt := Split;
      Event.Control := Split;
      Event.Shift := Split;
      Event.Meta := Split;

      return Event;
   end Parse_Keyboard_Event;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Base_Type) is
   begin
      Gnoga.Connections.New_Unique_ID (Object.Unique_ID);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Base_Type) is
      use type Gnoga.Types.ID_Enumeration;
   begin
      Object.On_Destroy;
      if Object.Connection_ID /= Gnoga.Types.No_Connection then
         if Object.ID_Type = Gnoga.Types.Gnoga_ID then
            begin
               Gnoga.Connections.Execute_Script
                 (Object.Connection_ID, "delete gnoga['" & Object.ID & "'];");
            exception
               when Gnoga.Connections.Connection_Error =>
                  null;
            end;
         end if;

         Object.Detach_From_Message_Queue;
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Object : in out Base_Type) is
      P : Pointer_To_Base_Class := Object'Unchecked_Access;
      procedure Free_Object is
        new Ada.Unchecked_Deallocation (Base_Type'Class,
                                        Pointer_To_Base_Class);
   begin
      Free_Object (P);
   end Free;

   -------------------------------------------------------------------------
   --  Base_Type - Creation Methods
   -------------------------------------------------------------------------

   ------------------------
   -- Create_With_Script --
   ------------------------

   procedure Create_With_Script
     (Object        : in out Base_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String;
      Script        : in     String;
      ID_Type       : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID)
   is
   begin
      if Object.Connection_ID /= Gnoga.Types.No_Connection then
         raise Object_Already_Created;
      end if;

      Gnoga.Connections.Execute_Script (ID     => Connection_ID,
                                        Script => Script);

      Object.Attach (Connection_ID => Connection_ID,
                     ID            => ID,
                     ID_Type       => ID_Type);

      Object.On_Create;
   end Create_With_Script;

   -------------------------
   -- Attach_Using_Parent --
   -------------------------

   procedure Attach_Using_Parent
     (Object   : in out Base_Type;
      Parent   : in     Gnoga.Base.Base_Type'Class;
      ID       : in     String;
      ID_Type  : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID)
   is
   begin
      Object.Attach (Connection_ID => Parent.Connection_ID,
                     ID            => ID,
                     ID_Type       => ID_Type);
   end Attach_Using_Parent;
   ------------
   -- Attach --
   ------------

   procedure Attach
     (Object        : in out Base_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String;
      ID_Type       : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID)
   is
   begin
      Object.Web_ID        := Ada.Strings.Unbounded.To_Unbounded_String (ID);
      Object.Connection_ID := Connection_ID;
      Object.ID_Type       := ID_Type;

      Object.Attach_To_Message_Queue;
   end Attach;

   -------------------------------------------------------------------------
   --  Base_Type - Properties
   -------------------------------------------------------------------------

   ---------------
   -- Unique_ID --
   ---------------

   function Unique_ID (Object : Base_Type) return Gnoga.Types.Unique_ID is
   begin
      return Object.Unique_ID;
   end Unique_ID;

   function Unique_ID (Object : Base_Type) return String is
   begin
      return Object.Unique_ID'Img;
   end Unique_ID;

   -------------------
   -- Connection_ID --
   -------------------

   function Connection_ID (Object : Base_Type)
			  return Gnoga.Types.Connection_ID
   is
   begin
      return Object.Connection_ID;
   end Connection_ID;

   procedure Connection_ID (Object : in out Base_Type;
                            Value  : in Gnoga.Types.Connection_ID)
   is
   begin
      Object.Connection_ID := Value;
   end Connection_ID;

   -----------
   -- Valid --
   -----------

   function Valid (Object : Base_Type) return Boolean is
   begin
      if Object.Connection_ID = Gnoga.Types.No_Connection then
         return False;
      else
         return Gnoga.Connections.Valid (Object.Connection_ID);
      end if;
   end Valid;

   --------
   -- ID --
   --------

   function ID (Object : Base_Type) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Object.Web_ID);
   end ID;

   -------------
   -- ID_Type --
   -------------

   function ID_Type (Object : Base_Type) return Gnoga.Types.ID_Enumeration
   is
   begin
      return Object.ID_Type;
   end ID_Type;

   ---------------------
   -- Connection_Data --
   ---------------------

   function Connection_Data
     (Object : Base_Type)
      return Gnoga.Types.Pointer_to_Connection_Data_Class
   is
   begin
      return Gnoga.Connections.Connection_Data (Object.Connection_ID);
   end Connection_Data;

   ------------
   -- Parent --
   ------------

   function Parent (Object : Base_Type)
                    return Pointer_To_Base_Class
   is
   begin
      return Object.Parent_Object;
   end Parent;

   procedure Parent (Object : in out Base_Type;
                     Value  : in out Base_Type'Class)
   is
   begin
      Object.Parent_Object := Value'Unchecked_Access;

      Value.On_Child_Added (Object);
   end Parent;

   procedure Parent (Object : in out Base_Type;
                     Value  : in Pointer_To_Base_Class)
   is
   begin
      Object.Parent (Value.all);
   end Parent;

   ------------
   -- Height --
   ------------

   procedure Height (Object : in out Base_Type; Value : in Integer) is
   begin
      Object.jQuery_Execute ("height(" & Left_Trim (Value'Img) & ");");
   end Height;

   function Height (Object : Base_Type) return Integer is
   begin
      return Object.jQuery_Execute ("height();");
   end Height;

   -----------
   -- Width --
   -----------

   procedure Width (Object : in out Base_Type; Value : in Integer) is
   begin
      Object.jQuery_Execute ("width(" & Left_Trim (Value'Img) & ");");
   end Width;

   function Width (Object : Base_Type) return Integer is
   begin
      return Object.jQuery_Execute ("width();");
   end Width;

   --------------
   -- Property --
   --------------

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     String)
   is
   begin
      Object.jQuery_Execute ("prop ('" & Name & "',""" &
                               Escape_Quotes (Value) & """);");
   end Property;

   function Property (Object : Base_Type; Name : String) return String is
   begin
      return Object.jQuery_Execute ("prop ('" & Name & "');");
   end Property;

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Integer)
   is
   begin
      Object.jQuery_Execute ("prop ('" & Name & "'," & Value'img & ");");
   end Property;

   function Property (Object : Base_Type; Name : String) return Integer is
   begin
      return Integer'Value (Object.Property (Name));
   exception
      when others =>
         return 0;
   end Property;

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Boolean)
   is
   begin
      Object.jQuery_Execute ("prop ('" & Name & "'," & Value'img & ");");
   end Property;

   function Property (Object : Base_Type; Name : String) return Boolean is
   begin
      return Object.Property (Name) = "true";
   end Property;

   -------------------------------------------------------------------------
   --  Base_Type - Methods
   -------------------------------------------------------------------------

   -----------
   -- Focus --
   -----------

   procedure Focus (Object : in out Base_Type) is
   begin
      Object.Execute ("focus();");
   end Focus;

   ----------
   -- Blur --
   ----------

   procedure Blur (Object : in out Base_Type) is
   begin
      Object.Execute ("blur();");
   end Blur;

   ------------
   -- Execute--
   ------------

   procedure Execute (Object : in out Base_Type; Method : in String) is
      Message_Script : constant String := jQuery(Object) & ".get(0)." & Method;
   begin
      Object.jQuery_Execute ("get(0)." & Method);
   end Execute;

   function Execute (Object : Base_Type; Method : in String) return String is
   begin
      return Object.jQuery_Execute ("get(0)." & Method);
   end Execute;

   -------------------------------------------------------------------------
   --  Base_Type - Events
   -------------------------------------------------------------------------

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Resize_Event := Handler;

      Object.Bind_Event (Event   => "resize",
                         Message => "");
   end On_Resize_Handler;

   procedure Fire_On_Resize (Object : in out Base_Type)
   is
   begin
      if Object.On_Resize_Event /= null then
         Object.On_Resize_Event (Object);
      end if;
   end Fire_On_Resize;

   ---------------
   -- On_Scroll --
   ---------------

   procedure On_Scroll_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Scroll_Event := Handler;

      Object.Bind_Event (Event   => "scroll",
                         Message => "");
   end On_Scroll_Handler;

   procedure Fire_On_Scroll (Object : in out Base_Type)
   is
   begin
      if Object.On_Scroll_Event /= null then
         Object.On_Scroll_Event (Object);
      end if;
   end Fire_On_Scroll;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      Object.On_Focus_Event := Handler;

      Object.Bind_Event (Event   => "focus",
                         Message => "");
   end On_Focus_Handler;

   procedure Fire_On_Focus (Object : in out Base_Type)
   is
   begin
      if Object.On_Focus_Event /= null then
         Object.On_Focus_Event (Object);
      end if;
   end Fire_On_Focus;

   -------------
   -- On_Blur --
   -------------

   procedure On_Blur_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      Object.On_Blur_Event := Handler;

      Object.Bind_Event (Event   => "blur",
                         Message => "");
   end On_Blur_Handler;

   procedure Fire_On_Blur (Object : in out Base_Type)
   is
   begin
      if Object.On_Blur_Event /= null then
         Object.On_Blur_Event (Object);
      end if;
   end Fire_On_Blur;

   ---------------
   -- On_Change --
   ---------------

   procedure On_Change_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Change_Event := Handler;

      Object.Bind_Event (Event   => "change",
                         Message => "");
   end On_Change_Handler;

   procedure Fire_On_Change (Object : in out Base_Type)
   is
   begin
      if Object.On_Change_Event /= null then
         Object.On_Change_Event (Object);
      end if;
   end Fire_On_Change;

   -----------------
   -- On_Focus_In --
   -----------------

   procedure On_Focus_In_Handler (Object  : in out Base_Type;
                                  Handler : in     Action_Event)
   is
   begin
      Object.On_Focus_In_Event := Handler;

      Object.Bind_Event (Event   => "focusin",
                         Message => "");
   end On_Focus_In_Handler;

   procedure Fire_On_Focus_In (Object : in out Base_Type)
   is
   begin
      if Object.On_Focus_In_Event /= null then
         Object.On_Focus_In_Event (Object);
      end if;
   end Fire_On_Focus_In;

   ------------------
   -- On_Focus_Out --
   ------------------

   procedure On_Focus_Out_Handler (Object  : in out Base_Type;
                                   Handler : in     Action_Event)
   is
   begin
      Object.On_Focus_Out_Event := Handler;

      Object.Bind_Event (Event   => "focusout",
                         Message => "");
   end On_Focus_Out_Handler;

   procedure Fire_On_Focus_Out (Object : in out Base_Type)
   is
   begin
      if Object.On_Focus_Out_Event /= null then
         Object.On_Focus_Out_Event (Object);
      end if;
   end Fire_On_Focus_Out;

   --------------
   -- On_Input --
   --------------

   procedure On_Input_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      Object.On_Input_Event := Handler;

      Object.Bind_Event (Event   => "input",
                         Message => "");
   end On_Input_Handler;

   procedure Fire_On_Input (Object : in out Base_Type)
   is
   begin
      if Object.On_Input_Event /= null then
         Object.On_Input_Event (Object);
      end if;
   end Fire_On_Input;

   --------------
   -- On_Reset --
   --------------

   procedure On_Reset_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      Object.On_Reset_Event := Handler;

      Object.Bind_Event (Event   => "reset",
                         Message => "",
                         Cancel => True);
   end On_Reset_Handler;

   procedure Fire_On_Reset (Object : in out Base_Type)
   is
   begin
      if Object.On_Reset_Event /= null then
         Object.On_Reset_Event (Object);
      end if;
   end Fire_On_Reset;

   ---------------
   -- On_Search --
   ---------------

   procedure On_Search_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Search_Event := Handler;

      Object.Bind_Event (Event   => "search",
                         Message => "");
   end On_Search_Handler;

   procedure Fire_On_Search (Object : in out Base_Type)
   is
   begin
      if Object.On_Search_Event /= null then
         Object.On_Search_Event (Object);
      end if;
   end Fire_On_Search;

   ---------------
   -- On_Select --
   ---------------

   procedure On_Select_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Select_Event := Handler;

      Object.Bind_Event (Event   => "select",
                         Message => "");
   end On_Select_Handler;

   procedure Fire_On_Select (Object : in out Base_Type)
   is
   begin
      if Object.On_Select_Event /= null then
         Object.On_Select_Event (Object);
      end if;
   end Fire_On_Select;

   ---------------
   -- On_Submit --
   ---------------

   procedure On_Submit_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Submit_Event := Handler;

      Object.Bind_Event (Event   => "submit",
                         Message => "",
                         Cancel  => True);
   end On_Submit_Handler;

   procedure Fire_On_Submit (Object : in out Base_Type)
   is
   begin
      if Object.On_Submit_Event /= null then
         Object.On_Submit_Event (Object);
      end if;
   end Fire_On_Submit;

   --------------
   -- On_Click --
   --------------

   procedure On_Click_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      Object.On_Click_Event := Handler;

      Object.Bind_Event (Event   => "click",
                         Message => "");
   end On_Click_Handler;

   procedure Fire_On_Click (Object : in out Base_Type)
   is
   begin
      if Object.On_Click_Event /= null then
         Object.On_Click_Event (Object);
      end if;
   end Fire_On_Click;

   --------------------
   -- On_Mouse_Click --
   --------------------

   procedure On_Mouse_Click_Handler (Object  : in out Base_Type;
                                     Handler : in     Mouse_Event)
   is
   begin
      Object.On_Mouse_Click_Event := Handler;

      Object.Bind_Event (Event   => "click",
                         Message => "",
                         Script  => Mouse_Event_Script);
   end On_Mouse_Click_Handler;

   procedure Fire_On_Mouse_Click (Object   : in out Base_Type;
                                  Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Click_Event /= null then
         Object.On_Mouse_Click_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Click;

   ---------------------
   -- On_Context_Menu --
   ---------------------

   procedure On_Context_Menu_Handler (Object  : in out Base_Type;
                                      Handler : in     Action_Event)
   is
   begin
      Object.On_Context_Menu_Event := Handler;

      Object.Bind_Event (Event   => "contextmenu",
                         Message => "");
   end On_Context_Menu_Handler;

   procedure Fire_On_Context_Menu (Object : in out Base_Type)
   is
   begin
      if Object.On_Context_Menu_Event /= null then
         Object.On_Context_Menu_Event (Object);
      end if;
   end Fire_On_Context_Menu;

   --------------------------
   -- On_Mouse_Right_Click --
   --------------------------

   procedure On_Mouse_Right_Click_Handler (Object  : in out Base_Type;
                                           Handler : in     Mouse_Event)
   is
   begin
      Object.On_Mouse_Right_Click_Event := Handler;

      Object.Bind_Event (Event   => "contextmenu",
                         Message => "",
                         Script  => Mouse_Event_Script);
   end On_Mouse_Right_Click_Handler;

   procedure Fire_On_Mouse_Right_Click (Object   : in out Base_Type;
                                        Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Right_Click_Event /= null then
         Object.On_Mouse_Right_Click_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Right_Click;

   ---------------------
   -- On_Double_Click --
   ---------------------

   procedure On_Double_Click_Handler (Object  : in out Base_Type;
                                      Handler : in     Action_Event)
   is
   begin
      Object.On_Double_Click_Event := Handler;

      Object.Bind_Event (Event   => "dblclick",
                         Message => "");
   end On_Double_Click_Handler;

   procedure Fire_On_Double_Click (Object : in out Base_Type)
   is
   begin
      if Object.On_Double_Click_Event /= null then
         Object.On_Double_Click_Event (Object);
      end if;
   end Fire_On_Double_Click;

   ---------------------------
   -- On_Mouse_Double_Click --
   ---------------------------

   procedure On_Mouse_Double_Click_Handler (Object  : in out Base_Type;
                                            Handler : in     Mouse_Event)
   is
   begin
      Object.On_Mouse_Double_Click_Event := Handler;

      Object.Bind_Event (Event   => "dblclick",
                         Message => "",
                         Script  => Mouse_Event_Script);
   end On_Mouse_Double_Click_Handler;

   procedure Fire_On_Mouse_Double_Click (Object   : in out Base_Type;
                                         Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Double_Click_Event /= null then
         Object.On_Mouse_Double_Click_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Double_Click;

   --------------------
   -- On_Mouse_Enter --
   --------------------

   procedure On_Mouse_Enter_Handler (Object  : in out Base_Type;
                                     Handler : in     Action_Event)
   is
   begin
      Object.On_Mouse_Enter_Event := Handler;

      Object.Bind_Event (Event   => "mouseenter",
                         Message => "");
   end On_Mouse_Enter_Handler;

   procedure Fire_On_Mouse_Enter (Object : in out Base_Type)
   is
   begin
      if Object.On_Mouse_Enter_Event /= null then
         Object.On_Mouse_Enter_Event (Object);
      end if;
   end Fire_On_Mouse_Enter;

   --------------------
   -- On_Mouse_Leave --
   --------------------

   procedure On_Mouse_Leave_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      Object.On_Mouse_Leave_Event := Handler;

      Object.Bind_Event (Event   => "mouseleave",
                         Message => "");
   end On_Mouse_Leave_Handler;

   procedure Fire_On_Mouse_Leave (Object : in out Base_Type)
   is
   begin
      if Object.On_Mouse_Leave_Event /= null then
         Object.On_Mouse_Leave_Event (Object);
      end if;
   end Fire_On_Mouse_Leave;

   --------------------
   -- On_Mouse_Over --
   --------------------

   procedure On_Mouse_Over_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      Object.On_Mouse_Over_Event := Handler;

      Object.Bind_Event (Event   => "mouseover",
                         Message => "");
   end On_Mouse_Over_Handler;

   procedure Fire_On_Mouse_Over (Object : in out Base_Type)
   is
   begin
      if Object.On_Mouse_Over_Event /= null then
         Object.On_Mouse_Over_Event (Object);
      end if;
   end Fire_On_Mouse_Over;

   ------------------
   -- On_Mouse_Out --
   ------------------

   procedure On_Mouse_Out_Handler (Object  : in out Base_Type;
                                   Handler : in     Action_Event)
   is
   begin
      Object.On_Mouse_Out_Event := Handler;

      Object.Bind_Event (Event   => "mouseout",
                         Message => "");
   end On_Mouse_Out_Handler;

   procedure Fire_On_Mouse_Out (Object : in out Base_Type)
   is
   begin
      if Object.On_Mouse_Out_Event /= null then
         Object.On_Mouse_Out_Event (Object);
      end if;
   end Fire_On_Mouse_Out;

   -------------------
   -- On_Mouse_Down --
   -------------------

   procedure On_Mouse_Down_Handler (Object  : in out Base_Type;
                                    Handler : in     Mouse_Event)
   is
   begin
      Object.On_Mouse_Down_Event := Handler;

      Object.Bind_Event (Event   => "mousedown",
                         Message => "",
                         Script  => Mouse_Event_Script);
   end On_Mouse_Down_Handler;

   procedure Fire_On_Mouse_Down (Object   : in out Base_Type;
                                 Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Down_Event /= null then
         Object.On_Mouse_Down_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Down;

   -----------------
   -- On_Mouse_Up --
   -----------------

   procedure On_Mouse_Up_Handler (Object  : in out Base_Type;
                                  Handler : in     Mouse_Event)
   is
   begin
      Object.On_Mouse_Up_Event := Handler;

      Object.Bind_Event (Event   => "mouseup",
                         Message => "",
                         Script  => Mouse_Event_Script);
   end On_Mouse_Up_Handler;

   procedure Fire_On_Mouse_Up (Object   : in out Base_Type;
                               Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Up_Event /= null then
         Object.On_Mouse_Up_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Up;

   -------------------
   -- On_Mouse_Move --
   -------------------

   procedure On_Mouse_Move_Handler (Object  : in out Base_Type;
                                    Handler : in     Mouse_Event)
   is
   begin
      Object.On_Mouse_Move_Event := Handler;

      Object.Bind_Event (Event   => "mousemove",
                         Message => "",
                         Script  => Mouse_Event_Script);
   end On_Mouse_Move_Handler;

   procedure Fire_On_Mouse_Move (Object   : in out Base_Type;
                                 Event    : in     Mouse_Event_Record)
   is
   begin
      if Object.On_Mouse_Move_Event /= null then
         Object.On_Mouse_Move_Event (Object, Event);
      end if;
   end Fire_On_Mouse_Move;


   ------------------
   -- On_Character --
   ------------------

   procedure On_Character_Handler (Object  : in out Base_Type;
                                   Handler : in     Character_Event)
   is
   begin
      Object.On_Character_Event := Handler;

      Object.Bind_Event (Event   => "keypress",
                         Message => "",
                         Script  => Keyboard_Event_Script);
   end On_Character_Handler;

   procedure Fire_On_Character (Object : in out Base_Type;
                                Key    : in     Character)
   is
   begin
      if Object.On_Character_Event /= null then
         Object.On_Character_Event (Object, Key);
      end if;
   end Fire_On_Character;

   -----------------------
   -- On_Wide_Character --
   -----------------------

   procedure On_Wide_Character_Handler (Object  : in out Base_Type;
                                        Handler : in     Wide_Character_Event)
   is
   begin
      Object.On_Wide_Character_Event := Handler;

      Object.Bind_Event (Event   => "keypress",
                         Message => "",
                         Script  => Keyboard_Event_Script);
   end On_Wide_Character_Handler;

   procedure Fire_On_Wide_Character (Object : in out Base_Type;
                                     Key    : in     Wide_Character)
   is
   begin
      if Object.On_Wide_Character_Event /= null then
         Object.On_Wide_Character_Event (Object, Key);
      end if;
   end Fire_On_Wide_Character;

   ------------------
   -- On_Key_Down --
   ------------------

   procedure On_Key_Down_Handler (Object  : in out Base_Type;
                                  Handler : in     Keyboard_Event)
   is
   begin
      Object.On_Key_Down_Event := Handler;

      Object.Bind_Event (Event   => "keydown",
                         Message => "",
                         Script  => Keyboard_Event_Script);
   end On_Key_Down_Handler;

   procedure Fire_On_Key_Down (Object : in out Base_Type;
                               Event  : in     Keyboard_Event_Record)
   is
   begin
      if Object.On_Key_Down_Event /= null then
         Object.On_Key_Down_Event (Object, Event);
      end if;
   end Fire_On_Key_Down;

   ---------------
   -- On_Key_Up --
   ---------------

   procedure On_Key_Up_Handler (Object  : in out Base_Type;
                                Handler : in     Keyboard_Event)
   is
   begin
      Object.On_Key_Up_Event := Handler;

      Object.Bind_Event (Event   => "keyup",
                         Message => "",
                         Script  => Keyboard_Event_Script);
   end On_Key_Up_Handler;

   procedure Fire_On_Key_Up (Object : in out Base_Type;
                             Event  : in     Keyboard_Event_Record)
   is
   begin
      if Object.On_Key_Up_Event /= null then
         Object.On_Key_Up_Event (Object, Event);
      end if;
   end Fire_On_Key_Up;

   ------------------
   -- On_Key_Press --
   ------------------

   procedure On_Key_Press_Handler (Object  : in out Base_Type;
                                  Handler : in     Keyboard_Event)
   is
   begin
      Object.On_Key_Press_Event := Handler;

      Object.Bind_Event (Event   => "keypress",
                         Message => "",
                         Script  => Keyboard_Event_Script);
   end On_Key_Press_Handler;

   procedure Fire_On_Key_Press (Object : in out Base_Type;
                               Event  : in     Keyboard_Event_Record)
   is
   begin
      if Object.On_Key_Press_Event /= null then
         Object.On_Key_Press_Event (Object, Event);
      end if;
   end Fire_On_Key_Press;

   -------------
   -- On_Copy --
   -------------

   procedure On_Copy_Handler (Object  : in out Base_Type;
                              Handler : in     Action_Event)
   is
   begin
      Object.On_Copy_Event := Handler;

      Object.Bind_Event (Event   => "copy",
                         Message => "");
   end On_Copy_Handler;

   procedure Fire_On_Copy (Object : in out Base_Type)
   is
   begin
      if Object.On_Copy_Event /= null then
         Object.On_Copy_Event (Object);
      end if;
   end Fire_On_Copy;

   ------------
   -- On_Cut --
   ------------

   procedure On_Cut_Handler (Object  : in out Base_Type;
                             Handler : in     Action_Event)
   is
   begin
      Object.On_Cut_Event := Handler;

      Object.Bind_Event (Event   => "cut",
                         Message => "");
   end On_Cut_Handler;

   procedure Fire_On_Cut (Object : in out Base_Type)
   is
   begin
      if Object.On_Cut_Event /= null then
         Object.On_Cut_Event (Object);
      end if;
   end Fire_On_Cut;

   --------------
   -- On_Paste --
   --------------

   procedure On_Paste_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      Object.On_Paste_Event := Handler;

      Object.Bind_Event (Event   => "paste",
                         Message => "");
   end On_Paste_Handler;

   procedure Fire_On_Paste (Object : in out Base_Type)
   is
   begin
      if Object.On_Paste_Event /= null then
         Object.On_Paste_Event (Object);
      end if;
   end Fire_On_Paste;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Object : in out Base_Type)
   is
   begin
      Object.Fire_On_Create;
   end On_Create;

   procedure On_Create_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Create_Event := Handler;
   end On_Create_Handler;

   procedure Fire_On_Create (Object : in out Base_Type)
   is
   begin
      if Object.On_Create_Event /= null then
         Object.On_Create_Event (Object);
      end if;
   end Fire_On_Create;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Object : in out Base_Type)
   is
   begin
      Object.Fire_On_Destroy;
   end On_Destroy;

   procedure On_Destroy_Handler (Object  : in out Base_Type;
                                 Handler : in     Action_Event)
   is
   begin
      Object.On_Destroy_Event := Handler;
   end On_Destroy_Handler;

   procedure Fire_On_Destroy (Object : in out Base_Type)
   is
   begin
      if Object.On_Destroy_Event /= null then
         Object.On_Destroy_Event (Object);
      end if;
   end Fire_On_Destroy;

   ----------------
   -- On_Child_Added --
   ----------------

   procedure On_Child_Added (Object : in out Base_Type;
                             Child  : in out Base_Type'Class)
   is
   begin
      Object.Fire_On_Child_Added (Child);
   end On_Child_Added;

   procedure On_Child_Added_Handler (Object  : in out Base_Type;
                                     Handler : in     Child_Added_Event)
   is
   begin
      Object.On_Child_Added_Event := Handler;
   end On_Child_Added_Handler;

   procedure Fire_On_Child_Added (Object : in out Base_Type;
                                  Child  : in out Base_Type'Class)
   is
   begin
      if Object.On_Child_Added_Event /= null then
         Object.On_Child_Added_Event (Object, Child);
      end if;
   end Fire_On_Child_Added;

   ----------------
   -- On_Message --
   ----------------

   procedure On_Message (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String)
   is
   begin
      -- Object Events --
      if Event = "resize" then
         Object.Fire_On_Resize;
      elsif Event = "scroll" then
         Object.Fire_On_Scroll;

      -- Form Events --
      elsif Event = "focus" then
         Object.Fire_On_Focus;
      elsif Event = "blur" then
         Object.Fire_On_Blur;
      elsif Event = "change" then
         Object.Fire_On_Change;
      elsif Event = "focusin" then
         Object.Fire_On_Focus_In;
      elsif Event = "focusout" then
         Object.Fire_On_Focus_Out;
      elsif Event = "input" then
         Object.Fire_On_Input;
      elsif Event = "reset" then
         Object.Fire_On_Reset;
      elsif Event = "search" then
         Object.Fire_On_Search;
      elsif Event = "select" then
         Object.Fire_On_Select;
      elsif Event = "submit" then
         Object.Fire_On_Submit;

      -- Mouse Events --

      elsif Event = "click" then
         if Message = "" then
            Object.Fire_On_Click;
         else
            Object.Fire_On_Mouse_Click (Parse_Mouse_Event (Message));
         end if;
      elsif Event = "contextmenu" then
         if Message = "" then
            Object.Fire_On_Double_Click;
         else
            Object.Fire_On_Mouse_Double_Click (Parse_Mouse_Event (Message));
         end if;
      elsif Event = "dblclick" then
         if Message = "" then
            Object.Fire_On_Context_Menu;
         else
            Object.Fire_On_Mouse_Right_Click (Parse_Mouse_Event (Message));
         end if;
      elsif Event = "mouseenter" then
         Object.Fire_On_Mouse_Enter;
      elsif Event = "mouseleave" then
         Object.Fire_On_Mouse_Leave;
      elsif Event = "mouseover" then
         Object.Fire_On_Mouse_Over;
      elsif Event = "mouseout" then
         Object.Fire_On_Mouse_Out;
      elsif Event = "mousedown" then
         Object.Fire_On_Mouse_Down (Parse_Mouse_Event (Message));
      elsif Event = "mouseup" then
         Object.Fire_On_Mouse_Up (Parse_Mouse_Event (Message));
      elsif Event = "mousemove" then
         Object.Fire_On_Mouse_Move (Parse_Mouse_Event (Message));

      -- Keyboard Events --

      elsif Event = "keydown" then
         Object.Fire_On_Key_Down (Parse_Keyboard_Event (Message));
      elsif Event = "keyup" then
         Object.Fire_On_Key_Down (Parse_Keyboard_Event (Message));
      elsif Event = "keypress" then
         declare
            E : Keyboard_Event_Record := Parse_Keyboard_Event (Message);
            C : Character;
         begin
            Object.Fire_On_Key_Press (E);
            Object.Fire_On_Wide_Character (Wide_Character'Val (E.Key_Code));

            if E.Key_Code > 255 then
               C := Character'Val (0);
            else
               C := Character'Val (E.Key_Code);
            end if;

            Object.Fire_On_Character (C);
         end;

      -- Clipboard Events --

      elsif Event = "copy" then
         Object.Fire_On_Copy;
      elsif Event = "cut" then
         Object.Fire_On_Cut;
      elsif Event = "paste" then
         Object.Fire_On_Paste;
      else
         Gnoga.Log ("Unhandled Event : " & Event);
      end if;
   end On_Message;

   procedure On_Message_Handler (Object  : in out Base_Type;
                                 Handler : in     Message_Event)
   is
   begin
      Object.On_Message_Event := Handler;
   end On_Message_Handler;

   procedure Fire_On_Message (Object   : in out Base_Type;
                              Event    : in     String;
                              Message  : in     String;
                              Continue : out    Boolean)
   is
   begin
      Continue := True;

      if Object.On_Message_Event /= null then
         Object.On_Message_Event (Object, Event, Message, Continue);
      end if;
   end Fire_On_Message;

   -------------------------------------------------------------------------
   --  Base_Type - Event Internals
   -------------------------------------------------------------------------

   ----------------
   -- Bind_Event --
   ----------------

   procedure Bind_Event (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String;
                         Script  : in     String    := "";
                         Cancel  : in     Boolean   := false)
   is
      US : constant String := Object.Unique_ID'Img;

      Full_Message : constant String := US (US'First + 1 .. US'Last) &
        "|" & Event & "|" & Message;

      function If_Script return String is
      begin
         if Script = "" then
            return "";
         else
            return "+" & Script;
         end if;
      end If_Script;

      function Cancel_Event return String is
      begin
         if Cancel then
            return " return false;";
         else
            return "";
         end if;
      end Cancel_Event;
   begin
      Bind_Event_Script (Object => Object,
                         Event  => Event,
                         Script => "ws.send (""" &
                           Escape_Quotes (Full_Message) & """" &
                           If_Script & ");" & Cancel_Event);
   end Bind_Event;

   -----------------------
   -- Bind_Event_Script --
   -----------------------

   procedure Bind_Event_Script (Object : in out Base_Type;
                                Event  : in     String;
                                Script : in     String)
   is
   begin
      Object.jQuery_Execute ("on (""" & Event & """, function (e) {" &
                               Script & "});");
   end Bind_Event_Script;

   -----------------------------
   -- Attach_To_Message_Queue --
   -----------------------------

   procedure Attach_To_Message_Queue (Object : in out Base_Type) is
   begin
      Gnoga.Connections.Add_To_Message_Queue (Object);
   end Attach_To_Message_Queue;

   --------------------------------
   -- Detach_From_Message_Queue --
   --------------------------------

   procedure Detach_From_Message_Queue (Object : in out Base_Type) is
   begin
      Gnoga.Connections.Delete_From_Message_Queue (Object);
   end Detach_From_Message_Queue;

   ---------------------
   -- Script_Accessor --
   ---------------------

   function Script_Accessor (Object : Base_Type) return String is
   begin
      return Script_Accessor (Object.ID, Object.ID_Type);
   end;

   function Script_Accessor (ID : String; ID_Type : Gnoga.Types.ID_Enumeration)
                             return String
   is
      use Gnoga.Types;
   begin
      case ID_Type is
         when DOM_ID =>
            return "#" & ID;
         when Script =>
            return ID;
         when Gnoga_ID =>
            return "gnoga['" & ID & "']";
      end case;
   end Script_Accessor;

   ------------
   -- jQuery --
   ------------

   function jQuery (Object : Base_Type) return String is
      use Gnoga.Types;
   begin
      case Object.ID_Type is
      when DOM_ID =>
         return "$('" & Object.Script_Accessor & "')";
      when Script | Gnoga_ID =>
         return "$(" & Object.Script_Accessor & ")";
      end case;
   end jQuery;

   --------------------
   -- jQuery_Execute --
   --------------------

   procedure jQuery_Execute (Object : in out Base_Type; Method : String) is
      Message_Script : constant String := jQuery(Object) & "." & Method;
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   end jQuery_Execute;

   function jQuery_Execute (Object : Base_Type; Method : String)
                            return String
   is
      Message_Script : constant String := jQuery(Object) & "." & Method;
   begin
      return Gnoga.Connections.Execute_Script (ID     => Object.Connection_ID,
                                               Script => Message_Script);
   end jQuery_Execute;

   function jQuery_Execute (Object : Base_Type; Method : String)
                            return Integer
   is
   begin
      return Integer'Value (Object.jQuery_Execute (Method));
   exception
      when others =>
         return 0;
   end;
end Gnoga.Base;
