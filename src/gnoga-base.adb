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

with Gnoga.Connections;

package body Gnoga.Base is
   
   Mouse_Event_Script : constant String :=
     "e.clientX + '|' + e.clientY + '|' + e.screenX + '|' + " &
     "e.screenY + '|' + e.which + '|' + e.altKey + '|' + " &
     "e.ctrlKey + '|' + e.shiftKey + '|' + e.metaKey + '|'";
   --  e.buttons would be better but not supported currently outside
   --  of firefox and would always return 0 on Mac so using e.which.
      
   function Parse_Mouse_Event (Message : String) return Mouse_Event_Record;
   --  Parse event message in to Mouse_Event_Record
   
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
   begin
      Object.On_Destroy;
      if Object.Connection_ID /= Gnoga.Types.No_Connection then
         Object.Detach_From_Message_Queue;
      end if;
   end Finalize;

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
      Gnoga.Connections.Execute_Script (ID     => Connection_ID,
                                        Script => Script);

      Object.Attach (Connection_ID => Connection_ID,
                     ID            => ID);
      
      Object.On_Create;      
   end Create_With_Script;

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

   function Connection_Data
     (Object : Base_Type)
      return Gnoga.Types.Pointer_to_Connection_Data_Class
   is
   begin
      return Gnoga.Connections.Connection_Data (Object.Connection_ID);
   end Connection_Data;
   
   ------------
   -- Height --
   ------------
   
   procedure Height (Object : in out Base_Type; Value : in Integer) is
      Message_Script : constant String := jQuery(Object) &
        ".height(" & Left_Trim (Value'Img) & ");";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   end Height;
   
   function Height (Object : Base_Type) return Integer is
      Message_Script : constant String := jQuery(Object) & ".height();";

      R : String := Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   begin
      return Integer (Float'Value (R));
   end Height;
   
   -----------
   -- Width --
   -----------
   
   procedure Width (Object : in out Base_Type; Value : in Integer) is
      Message_Script : constant String := jQuery(Object) &
        ".width(" & Left_Trim (Value'Img) & ");";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   end Width;
         
   function Width (Object : Base_Type) return Integer is
      Message_Script : constant String := jQuery(Object) & ".width();";
      R : String := Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   begin
      return Integer (Float'Value (R));
   end Width;
   
   --------------
   -- Property --
   --------------
   
   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     String)
   is
      Message_Script : constant String := jQuery(Object) &
        ".prop ('" & Name & "',""" & Escape_Quotes (Value) & """);";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);               
   end Property;
   
   function Property (Object : Base_Type; Name : String) return String is
      Message_Script : constant String := jQuery(Object) &
        ".prop ('" & Name & "');";
   begin
      return Gnoga.Connections.Execute_Script (ID     => Object.Connection_ID,
                                               Script => Message_Script);
   end Property;

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Integer)
   is
      Message_Script : constant String := jQuery(Object) &
        ".prop ('" & Name & "')=" & Value'Img & ";";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);               
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
      Message_Script : constant String := jQuery(Object) &
        ".prop ('" & Name & "')=" & Value'Img & ";";      
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);               
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
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);         
   end Execute;

   function Execute (Object : Base_Type; Method : in String) return String is
      Message_Script : constant String := jQuery(Object) & ".get(0)." & Method;
   begin
      return Gnoga.Connections.Execute_Script (ID     => Object.Connection_ID,
                                               Script => Message_Script);
   end Execute;
   
   -------------------------------------------------------------------------
   --  Base_Type - Events
   -------------------------------------------------------------------------

   --------------
   -- On_Abort --
   --------------   

   procedure On_Abort_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      Object.On_Abort_Event := Handler;      
      
      Object.Bind_Event (Event   => "abort",
                         Message => "");
   end On_Abort_Handler;
   
   procedure Fire_On_Abort (Object : in out Base_Type)
   is
   begin
      if Object.On_Abort_Event /= null then
         Object.On_Abort_Event (Object);
      end if;
   end Fire_On_Abort;

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

   --------------
   -- On_Double_Click --
   --------------   

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

   --------------------
   -- On_Mouse_Out --
   --------------------

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
   -- On_Message --
   ----------------
   
   procedure On_Message (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String)
   is
   begin
      if Event = "abort" then
         Object.Fire_On_Abort;
      elsif Event = "blur" then
         Object.Fire_On_Blur;

         
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
                         Script  : in     String    := "")
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
      end;
   begin
      Bind_Event_Script (Object => Object,
                         Event  => Event,
                         Script => "ws.send (""" &
                           Escape_Quotes (Full_Message) & """" &
                           If_Script & ");");
   end Bind_Event;

   -----------------------
   -- Bind_Event_Script --
   -----------------------
   
   procedure Bind_Event_Script (Object : in out Base_Type;
                                Event  : in     String;
                                Script : in     String)
   is
      Message_Script : constant String :=
        jQuery (Object) & ".on (""" & Event &
        """, function (e) {" &
        Script & "}" &
        "); ";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
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
      use Gnoga.Types;
   begin
      case Object.ID_Type is
      when DOM_ID =>
         return "#" & Object.ID;
      when Script =>
         return Object.ID;
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
      when Script =>
         return "$(" & Object.Script_Accessor & ")";
      end case;
   end jQuery;    
end Gnoga.Base;
