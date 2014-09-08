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

   overriding procedure Initialize (Object : in out Base_Type);
   --  Assigns Unique_ID

   overriding procedure Finalize (Object : in out Base_Type);
   --  Detaches object from message queue and fires On_Destroy

   -------------------------------------------------------------------------
   --  Base_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create_Root (Object        : in out Base_Type;
                          Connection_ID : in     Gnoga.Types.Connection_ID;
                          ID            : in     String;
                          HTML          : in     String);
   --  Create a Gnoga oject on Connection ID with DOM ID using HTML in <body>
   --  Warning: This will destroy all DOM objects in body.

   procedure Create_Inside (Object        : in out Base_Type;
                            Parent        : in out Base_Type'Class;
                            ID            : in     String;
                            HTML          : in     String);
   --  Create a Gnoga object on with DOM ID using HTML inside Parent.
   --  Warning: This will destroy all DOM objects already in Parent.

   procedure Create_Inside_At_Top (Object        : in out Base_Type;
                                   Parent        : in out Base_Type'Class;
                                   ID            : in     String;
                                   HTML          : in     String);
   --  Create a Gnoga object on with DOM ID using HTML inside Parent at top
   --  of existing objects.

   procedure Create_Inside_At_Bottom (Object        : in out Base_Type;
                                      Parent        : in out Base_Type'Class;
                                      ID            : in     String;
                                      HTML          : in     String);
   --  Create a Gnoga object on with DOM ID using HTML inside Parent at bottom
   --  of existing objects.


   procedure Create_After (Object        : in out Base_Type;
                           Target        : in out Base_Type'Class;
                           ID            : in     String;
                           HTML          : in     String);
   --  Create a Gnoga object with DOM ID using HTML after Target.

   procedure Create_Before (Object        : in out Base_Type;
                            Target        : in out Base_Type'Class;
                            ID            : in     String;
                            HTML          : in     String);
   --  Create a Gnoga object with DOM ID using HTML before Target.

   procedure Create_With_Script
     (Object        : in out Base_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String;
      Script        : in     String);
   --  Create a Gnoga object on Connection ID with DOM ID using Script.
   --  The Script must include creating the id attribute equal to ID.
   --  Script is eval'd JavaScript

   procedure Attach (Object        : in out Base_Type;
                     Connection_ID : in     Gnoga.Types.Connection_ID;
                     ID            : in     String);
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

   function ID (Object : Base_Type) return String;
   --  The DOM ID for Object.


   --  Object Propertie --

   procedure Height (Object : in out Base_Type; Value : in Integer);
   function Height (Object : Base_Type) return Integer;

   procedure Width (Object : in out Base_Type; Value : in Integer);
   function Width (Object : Base_Type) return Integer;

   procedure Visible (Object : in out Base_Type; Value : Boolean := True);
   function Visible (Object : Base_Type) return Boolean;


   --  Generic Access  --

   procedure Property (Object : in out Base_Type;
                       Name   : in String;
                       Value  : in String);
   function Property (Object : Base_Type; Name : String) return String;
   --  General access to property Name

   procedure Style (Object : in out Base_Type;
                    Name   : in String;
                    Value  : in String);
   function Style (Object : Base_Type; Name : String) return String;
   --  General access to style Name

   procedure Attribute (Object : in out Base_Type;
                        Name   : in String;
                        Value  : in String);
   function Attribute (Object : Base_Type; Name : String) return String;
   --  General access to attribute Name



   -------------------------------------------------------------------------
   --  Base_Type - Methods
   -------------------------------------------------------------------------

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

   type Message_Event is access
     procedure (Object  : in out Base_Type'Class;
                Event   : in     String;
                Message : in     String);

   -- Mouse Events --

   procedure On_Click_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Click (Object : in out Base_Type);
   -- Handle mouse click events


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

   procedure On_Message_Handler (Object  : in out Base_Type;
                                 Handler : in     Message_Event);
   procedure Fire_On_Message (Object  : in out Base_Type;
                              Event   : in     String;
                              Message : in     String);
   -- Generic message event handler

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
                         Message : in     String);
   --  On Event occuring to Object Gnoga will fire Object.On_Message with
   --  Message.

   procedure Bind_Event_Script (Object : in out Base_Type;
                                Event  : in     String;
                                Script : in     String);
   --  On Event occuring to Object, the Script will be executed on browser.

   procedure Bind_Event_Script (Connection_ID : in Gnoga.Types.Connection_ID;
                                Query         : in String;
                                Event         : in String;
                                Script        : in String);
   --  On Event occuring to jQuery(Query) the Script will be executed on
   --  Connection_ID.

   procedure Attach_To_Message_Queue (Object : in out Base_Type);
   --  Attach Object to Message Queue

   procedure Detach_From_Message_Queue (Object : in out Base_Type);
   --  Detach Object from Message Queue

   function Script_Accessor (Object : Base_Type) return String;
   --  Return the internal DOM representation for scripts for Object
   --  unless overidden returns #ID

private
   type Base_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         Unique_ID     : Gnoga.Types.Unique_ID;
         DOM_ID        : Gnoga.Types.DOM_ID;
         Connection_ID : Gnoga.Types.Connection_ID := -1;

         -- Event Handlers
         On_Click_Event      : Action_Event  := null;
         On_Create_Event     : Action_Event  := null;
         On_Destroy_Event    : Action_Event  := null;
         On_Message_Event    : Message_Event := null;
      end record;
end Gnoga.Base;
