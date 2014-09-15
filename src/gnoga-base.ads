------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                          G N O G A . B A S E                             --
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
------------------------------------------------------------------------------                                                                          --

with Ada.Finalization;

with Gnoga.Types;

package Gnoga.Base is

   -------------------------------------------------------------------------
   --  Base_Type
   -------------------------------------------------------------------------
   --  Base_Type is the parent class of all Gnoga GUI Objects.
   --  It is generally used internally to create and bind Gnoga objects to
   --  HTML5 DOM objects.

   type Base_Type is new Ada.Finalization.Limited_Controlled with private;
   type Base_Access is access all Base_Type;
   type Pointer_To_Base_Class is access all Base_Type'Class;

   Object_Already_Create : exception;
   --  Thrown when an attempt is made to perform a create method on an already
   --  created or attached Gnoga object.

   overriding procedure Initialize (Object : in out Base_Type);
   --  Assigns Unique_ID

   overriding procedure Finalize (Object : in out Base_Type);
   --  Detaches object from message queue and fires On_Destroy

   procedure Free (Object : in out Base_Type);
   --  Free a dynamically created Object

   -------------------------------------------------------------------------
   --  Base_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create_With_Script
     (Object        : in out Base_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String;
      Script        : in     String;
      ID_Type       : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID);
   --  Create a Gnoga object on Connection ID with ID using Script.
   --  The Script must include creating the id attribute equal to ID.
   --  Script is eval'd JavaScript.

   procedure Attach
     (Object        : in out Base_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String;
      ID_Type       : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID);
   --  Attache a Gnoga object on Connection ID to an existing DOM object
   --  with ID. On_Create event is not fired.

   -------------------------------------------------------------------------
   --  Base_Type - Properties
   -------------------------------------------------------------------------

   --  Framework Properties  --

   function Unique_ID (Object : Base_Type) return Gnoga.Types.Unique_ID;
   --  Returns the Unique_ID for Object

   function Connection_ID (Object : Base_Type)
                           return Gnoga.Types.Connection_ID;
   --  The Gnoga Connection ID of Object.

   function Valid (Object : Base_Type) return Boolean;
   --  Returns true if Connection_ID is valid, i.e. Object was created and
   --  the connection is still valid.

   function ID (Object : Base_Type) return String;
   --  The ID for Object.

   function ID_Type (Object : Base_Type) return Gnoga.Types.ID_Enumeration;
   --  Returns the type of ID stored for Object

   function Connection_Data
     (Object : Base_Type)
      return Gnoga.Types.Pointer_to_Connection_Data_Class;

   function Parent (Object : Base_Type)
                    return Pointer_To_Base_Class;
   procedure Parent (Object : in out Base_Type;
                     Value  : in out Base_Type'Class);
   procedure Parent (Object : in out Base_Type;
                     Value  : in Pointer_To_Base_Class);
   --  Parent of Object. Setting the parent will on Parent_Added on parent.
   --  Setting the parent Object does not change the position Object may have
   --  in the DOM by default.

   --  Object Properties --

   procedure Height (Object : in out Base_Type; Value : in Integer);
   function Height (Object : Base_Type) return Integer;

   procedure Width (Object : in out Base_Type; Value : in Integer);
   function Width (Object : Base_Type) return Integer;

   --  Generic Access  --

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     String);
   function Property (Object : Base_Type; Name : String) return String;

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Integer);
   function Property (Object : Base_Type; Name : String) return Integer;

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Boolean);
   function Property (Object : Base_Type; Name : String) return Boolean;
   --  General access to property Name

   -------------------------------------------------------------------------
   --  Base_Type - Methods
   -------------------------------------------------------------------------

   --  Object Properties --

   procedure Focus (Object : in out Base_Type);
   --  Set focus on Object

   procedure Blur (Object : in out Base_Type);
   --  Remove focus from Object


   --  Generic Methods --

   procedure Execute (Object : in out Base_Type; Method : in String);
   function Execute (Object : Base_Type; Method : in String) return String;
   --  General access to execute a Method

   -------------------------------------------------------------------------
   --  Base_Type - Event Handlers
   -------------------------------------------------------------------------
   -- When an event handler is set any event binding to the browser will be
   -- installed automatically.

   type Action_Event is access
     procedure (Object : in out Base_Type'Class);

   type Mouse_Event_Record is
      record
         X             : Integer;
         Y             : Integer;
         Screen_X      : Integer;
         Screen_Y      : Integer;
         Left_Button   : Boolean := False;
         Middle_Button : Boolean := False;
         Right_Button  : Boolean := False;
         Alt           : Boolean := False;
         Control       : Boolean := False;
         Shift         : Boolean := False;
         Meta          : Boolean := False;
      end record;

   type Mouse_Event is access
     procedure (Object      : in out Base_Type'Class;
                Mouse_Event : in     Mouse_Event_Record);

   type Keyboard_Event_Record is
      record
         Key_Code      : Integer;
         Alt           : Boolean := False;
         Control       : Boolean := False;
         Shift         : Boolean := False;
         Meta          : Boolean := False;
      end record;

   type Keyboard_Event is access
     procedure (Object         : in out Base_Type'Class;
                Keyboard_Event : in     Keyboard_Event_Record);

   type Character_Event is access
     procedure (Object : in out Base_Type'Class;
                Key    : in     Character);

   type Wide_Character_Event is access
     procedure (Object : in out Base_Type'Class;
                Key    : in     Wide_Character);

   type Message_Event is access
     procedure (Object   : in out Base_Type'Class;
                Event    : in     String;
                Message  : in     String;
                Continue : out    Boolean);

   type Child_Added_Event is access
     procedure (Object : in out Base_Type;
                Child  : in out Base_Type'Class);

   -- Network Events --

   procedure On_Abort_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Abort (Object : in out Base_Type);

   procedure On_Error_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Error (Object : in out Base_Type);

   procedure On_Hash_Change_Handler (Object  : in out Base_Type;
                                     Handler : in     Action_Event);
   procedure Fire_On_Hash_Change (Object : in out Base_Type);

   -- Object Events --

   procedure On_Resize_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Resize (Object : in out Base_Type);
   --  Handle object size change.

   procedure On_Scroll_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Scroll (Object : in out Base_Type);
   --  Handle scroll changes.

   -- Form Events --

   procedure On_Focus_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Focus (Object : in out Base_Type);
   --  Handle focus on object

   procedure On_Blur_Handler (Object  : in out Base_Type;
                              Handler : in     Action_Event);
   procedure Fire_On_Blur (Object : in out Base_Type);
   --  Handle loss of focus, many browsers poorly support this event.

   procedure On_Change_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Change (Object : in out Base_Type);

   procedure On_Focus_In_Handler (Object  : in out Base_Type;
                                  Handler : in     Action_Event);
   procedure Fire_On_Focus_In (Object : in out Base_Type);

   procedure On_Focus_Out_Handler (Object  : in out Base_Type;
                                   Handler : in     Action_Event);
   procedure Fire_On_Focus_Out (Object : in out Base_Type);

   procedure On_Input_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Input (Object : in out Base_Type);

   procedure On_Reset_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Reset (Object : in out Base_Type);

   procedure On_Search_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Search (Object : in out Base_Type);

   procedure On_Select_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Select (Object : in out Base_Type);

   procedure On_Submit_Handler (Object  : in out Base_Type;
                                   Handler : in     Action_Event);
   procedure Fire_On_Submit (Object : in out Base_Type);


   -- Mouse Events --

   procedure On_Click_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Click (Object : in out Base_Type);
   --  Handle mouse click events

   procedure On_Mouse_Click_Handler (Object  : in out Base_Type;
                                     Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Click (Object   : in out Base_Type;
                                  Event    : in     Mouse_Event_Record);
   --  Handle mouse click events, but also return mouse event data

   procedure On_Context_Menu_Handler (Object  : in out Base_Type;
                                      Handler : in     Action_Event);
   procedure Fire_On_Context_Menu (Object : in out Base_Type);
   --  Handle right mouse button click events

   procedure On_Mouse_Right_Click_Handler (Object  : in out Base_Type;
                                           Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Right_Click (Object   : in out Base_Type;
                                        Event    : in     Mouse_Event_Record);
   --  Handle mouse right click event, but also return mouse event data

   procedure On_Double_Click_Handler (Object  : in out Base_Type;
                                      Handler : in     Action_Event);
   procedure Fire_On_Double_Click (Object : in out Base_Type);
   --  Handle mouse double click events

   procedure On_Mouse_Double_Click_Handler (Object  : in out Base_Type;
                                            Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Double_Click (Object   : in out Base_Type;
                                         Event    : in     Mouse_Event_Record);
   --  Handle mouse double click events, but also return mouse event data

   procedure On_Mouse_Enter_Handler (Object  : in out Base_Type;
                                     Handler : in     Action_Event);
   procedure Fire_On_Mouse_Enter (Object : in out Base_Type);
   --  Handle mouse enter to object events

   procedure On_Mouse_Leave_Handler (Object  : in out Base_Type;
                                     Handler : in     Action_Event);
   procedure Fire_On_Mouse_Leave (Object : in out Base_Type);
   --  Handle mouse leave object events

   procedure On_Mouse_Over_Handler (Object  : in out Base_Type;
                                     Handler : in     Action_Event);
   procedure Fire_On_Mouse_Over (Object : in out Base_Type);
   --  Handle mouse enter to object or it's children events

   procedure On_Mouse_Out_Handler (Object  : in out Base_Type;
                                   Handler : in     Action_Event);
   procedure Fire_On_Mouse_Out (Object : in out Base_Type);
   --  Handle mouse leave object or it's children events

   procedure On_Mouse_Down_Handler (Object  : in out Base_Type;
                                    Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Down (Object   : in out Base_Type;
                                 Event    : in     Mouse_Event_Record);
   --  Handle mouse down events

   procedure On_Mouse_Up_Handler (Object  : in out Base_Type;
                                  Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Up (Object   : in out Base_Type;
                               Event    : in     Mouse_Event_Record);
   --  Handle mouse down events

   procedure On_Mouse_Move_Handler (Object  : in out Base_Type;
                                    Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Move (Object   : in out Base_Type;
                                 Event    : in     Mouse_Event_Record);
   --  Handle mouse down events

   -- Drag and Drop Events --

   -- On_Drop event.preventDefault, data = event.dataTransfer.getData(Mime)
   -- On_Drag_Over (call to event.preventDefault on element allowing drop)
   -- On_Drag_Start (Mime, ID)

   --  Keyboard Events --

   procedure On_Character_Handler (Object  : in out Base_Type;
                                   Handler : in     Character_Event);
   procedure Fire_On_Character (Object : in out Base_Type;
                                Key    : in     Character);

   procedure On_Wide_Character_Handler (Object  : in out Base_Type;
                                        Handler : in     Wide_Character_Event);
   procedure Fire_On_Wide_Character (Object : in out Base_Type;
                                     Key    : in     Wide_Character);

   procedure On_Key_Down_Handler (Object  : in out Base_Type;
                                  Handler : in     Keyboard_Event);
   procedure Fire_On_Key_Down (Object : in out Base_Type;
                               Event  : in     Keyboard_Event_Record);

   procedure On_Key_Up_Handler (Object  : in out Base_Type;
                                Handler : in     Keyboard_Event);
   procedure Fire_On_Key_Up (Object : in out Base_Type;
                             Event  : in     Keyboard_Event_Record);

   procedure On_Key_Press_Handler (Object  : in out Base_Type;
                                   Handler : in     Keyboard_Event);
   procedure Fire_On_Key_Press (Object : in out Base_Type;
                                Event  : in     Keyboard_Event_Record);

   --  Clipboard Events  --

   procedure On_Copy_Handler (Object  : in out Base_Type;
                              Handler : in     Action_Event);
   procedure Fire_On_Copy (Object : in out Base_Type);

   procedure On_Cut_Handler (Object  : in out Base_Type;
                             Handler : in     Action_Event);
   procedure Fire_On_Cut (Object : in out Base_Type);

   procedure On_Paste_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Paste (Object : in out Base_Type);


   --  Generic Events --

   procedure On_Create_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Create (Object : in out Base_Type);
   --  Called on creation of a new Gnoga object after attached to message
   --  queue.

   procedure On_Destroy_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Destroy (Object : in out Base_Type);
   --  Called before detaching Gnoga object from message queue during
   --  finalization of Object.

   procedure On_Child_Added_Handler (Object  : in out Base_Type;
                                     Handler : in     Child_Added_Event);
   procedure Fire_On_Child_Added (Object : in out Base_Type;
                                  Child  : in out Base_Type'Class);

   procedure On_Message_Handler (Object  : in out Base_Type;
                                 Handler : in     Message_Event);
   procedure Fire_On_Message (Object   : in out Base_Type;
                              Event    : in     String;
                              Message  : in     String;
                              Continue : out    Boolean);
   --  Generic message event handler, if set is called before every event.
   --  If Continue is set to false, no more event processing will occur.

   -------------------------------------------------------------------------
   --  Base_Type - Event Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed and internal functionality of the event is handled
   --  properly, always call the base class event method.
   --
   --  Event Methods are always bound on creation of Gnoga object or do not
   --  require event binding.

   procedure On_Create (Object : in out Base_Type);
   --  Called on creation of a new Gnoga object after attached to message
   --  queue.

   procedure On_Destroy (Object : in out Base_Type);
   --  Called before detaching Gnoga object from message queue during
   --  finalization of Object.

   procedure On_Child_Added (Object : in out Base_Type;
                             Child  : in out Base_Type'Class);
   --  Called when a Child is created claiming Object as its parent.

   procedure On_Message (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String);
   --  Called on receiving any message or event from browser.

   -------------------------------------------------------------------------
   --  Base_Type - Event Internals
   -------------------------------------------------------------------------
   --  Event binding is usually used internally during On_Create or
   --  when setting a message handler. It can be used though to bind events
   --  not bound by Gnoga or custom events.

   procedure Bind_Event (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String;
                         Script  : in     String    := "");
   --  On Event occuring to Object Gnoga will fire Object.On_Message with
   --  Event and Message, the result of Script is concatinated to Message.

   procedure Bind_Event_Script (Object : in out Base_Type;
                                Event  : in     String;
                                Script : in     String);
   --  On Event occuring to Object, the Script will be executed on browser.

   procedure Attach_To_Message_Queue (Object : in out Base_Type);
   --  Attach Object to Message Queue

   procedure Detach_From_Message_Queue (Object : in out Base_Type);
   --  Detach Object from Message Queue

   function Script_Accessor (Object : Base_Type) return String;
   --  Returns the script representation for ID. For DOM_ID '#ID' for
   --  Script 'ID'

   function jQuery (Object : Base_Type) return String;
   --  Returns the jQuery selector for Object

   procedure jQuery_Execute (Object : in out Base_Type; Method : String);
   function jQuery_Execute (Object : Base_Type; Method : String) return String;
   function jQuery_Execute (Object : Base_Type; Method : String) return Integer;
   --  Execute Method of jQuery wrapper object
private
   type Base_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         Unique_ID     : Gnoga.Types.Unique_ID;
         Web_ID        : Gnoga.Types.Web_ID;
         ID_Type       : Gnoga.Types.ID_Enumeration;
         Connection_ID : Gnoga.Types.Connection_ID := Gnoga.Types.No_Connection;
         Parent_Object : Pointer_To_Base_Class := null;


         -- Network Events
         On_Abort_Event              : Action_Event         := null;
         On_Error_Event              : Action_Event         := null;
         On_Hash_Change_Event        : Action_Event         := null;

         -- Object Events
         On_Resize_Event             : Action_Event         := null;
         On_Scroll_Event             : Action_Event         := null;

         -- Form Events
         On_Focus_Event              : Action_Event         := null;
         On_Blur_Event               : Action_Event         := null;
         On_Change_Event             : Action_Event         := null;
         On_Focus_In_Event           : Action_Event         := null;
         On_Focus_Out_Event          : Action_Event         := null;
         On_Input_Event              : Action_Event         := null;
         On_Reset_Event              : Action_Event         := null;
         On_Search_Event             : Action_Event         := null;
         On_Select_Event             : Action_Event         := null;
         On_Submit_Event             : Action_Event         := null;

         -- Mouse Events
         On_Click_Event              : Action_Event         := null;
         On_Mouse_Click_Event        : Mouse_Event          := null;
         On_Mouse_Right_Click_Event  : Mouse_Event          := null;
         On_Context_Menu_Event       : Action_Event         := null;
         On_Double_Click_Event       : Action_Event         := null;
         On_Mouse_Double_Click_Event : Mouse_Event          := null;
         On_Mouse_Enter_Event        : Action_Event         := null;
         On_Mouse_Leave_Event        : Action_Event         := null;
         On_Mouse_Over_Event         : Action_Event         := null;
         On_Mouse_Out_Event          : Action_Event         := null;
         On_Mouse_Down_Event         : Mouse_Event          := null;
         On_Mouse_Up_Event           : Mouse_Event          := null;
         On_Mouse_Move_Event         : Mouse_Event          := null;

         -- Keyboard Events
         On_Character_Event          : Character_Event      := null;
         On_Wide_Character_Event     : Wide_Character_Event := null;
         On_Key_Down_Event           : Keyboard_Event       := null;
         On_Key_Up_Event             : Keyboard_Event       := null;
         On_Key_Press_Event          : Keyboard_Event       := null;

         -- Clipboard Events
         On_Copy_Event               : Action_Event         := null;
         On_Cut_Event                : Action_Event         := null;
         On_Paste_Event              : Action_Event         := null;

         -- Generic Events
         On_Create_Event             : Action_Event         := null;
         On_Destroy_Event            : Action_Event         := null;
         On_Child_Added_Event        : Child_Added_Event    := null;
         On_Message_Event            : Message_Event        := null;
      end record;
end Gnoga.Base;
