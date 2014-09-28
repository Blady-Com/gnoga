------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                      G N O G A . G U I . W I N D O W                     --
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

with Gnoga.Types;
with Gnoga.Gui.Base;
with Gnoga.Gui.Document;
with Gnoga.Gui.Location;

package Gnoga.Gui.Window is

   -------------------------------------------------------------------------
   --  Window_Type
   -------------------------------------------------------------------------
   --  Window_Type is the class encapsulating an individual Gnoga browser
   --  windows and tabs. It can also encapsulate any window or iFrame
   --  that is reachable by the DOM in a Gnoga browser window or tab, i.e.
   --  that was loaded with a connection via websockets to the Gnoga app.
   --  usually with Gnoga's standard bootstrap file.


   type Window_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Window_Access is access all Window_Type;
   type Pointer_To_Window_Class is access all Window_Type'Class;

   Invalid_ID_Type    : exception;

   Not_A_Gnoga_Window : exception;

   overriding procedure Attach
     (Window        : in out Window_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String                     := "window";
      ID_Type       : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.Script);
   --  Attach a Gnoga Window_Type to Connection_ID. This can be used to attach
   --  a non-Gnoga Window and events bound will use Connection_ID which should
   --  be the parent window in this case.
   --  ID_Type = DOM_ID will raise Invalid_ID_Type

   procedure Attach
     (Window  : in out Window_Type;
      Parent  : in out Window_Type'Class;
      ID      : in     String;
      ID_Type : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.Script);
   --  Attach a Window with ID with in the Parent window.
   --  ID_Type = DOM_ID will raise Invalid_ID_Type
   --  Note: This will only work if the window pointed to by ID has
   --        a Gnoga connection in it. Raises Not_A_Gnoga_Window if window
   --        does not contain a gnoga connection.

   procedure Reattach (Window : in out Window_Type;
                       Parent : in out Window_Type'Class);
   --  Attach a Window launched by Parent to its own Gnoga connection.
   --  Will raise Not_A_Gnoga_Window if connection was not established in
   --  Window or is not a Gnoga window (i.e. if has no websocket to app).

   procedure Set_View (Window : in out Window_Type;
                       Object : in out Gnoga.Gui.Base.Base_Type'Class;
                       Place  : in     Boolean := True);
   --  Sets Object as the Window's View. Object will be auto resized to fill
   --  the entire client area of Window. If Object is not in the DOM the
   --  resize will fail, therefore on an Object with a valid DOM id will
   --  work. Elements created with Gnoga always have a DOM id set if if
   --  the Object.ID_Type is not DOM_ID. Only the ID for Object is stored
   --  and used therefore even if Object is finalized as long as Object
   --  is still in the DOM it will continue to resize. If Place is True
   --  then Object will first be placed using Element.Place_Inside_Top_Of.
   --  If Object is not an Gnog.Element.Element_Type or a child of it an
   --  exception will be raised.
   --  ID_Type is not DOM_ID or Gnoga_ID will raise Invalid_ID_Type

   procedure Remove_View (Window : in out Window_Type);
   --  Remove the current View Object from Window

   -------------------------------------------------------------------------
   --  Window_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Launch (Window   : in out Window_Type;
                     Parent   : in out Window_Type'Class;
                     URL      : in     String;
                     Width    : in     Integer := -1;
                     Height   : in     Integer := -1;
                     Left     : in     Integer := -1;
                     Top      : in     Integer := -1;
                     Menu     : in     Boolean := False;
                     Status   : in     Boolean := False;
                     Tool_Bar : in     Boolean := False;
                     Title    : in     Boolean := False);
   --  Launch a new Window on Parent's connection. If Parent window is closed
   --  events will no longer work on Window. If the launched URL has a Gnoga
   --  connection can run Reattach on it before adding any elements or events.
   --  Before running Reattach you need to be sure that the gnoga websocket
   --  has been established on the launched page.

   -------------------------------------------------------------------------
   --  Window_Type - Properties
   -------------------------------------------------------------------------

   function Document (Window : Window_Type)
                      return Gnoga.Gui.Document.Document_Access;
   --  DOM Document Node

   function Location (Window : Window_Type)
                      return Gnoga.Gui.Location.Location_Access;
   --  Browser location object

   procedure Name (Window : in out Window_Type; Value : String);
   function Name (Window : Window_Type) return String;
   --  Hyperlink "target" Name for Window

   procedure Inner_Height (Window : in out Window_Type; Value : in Integer);
   function Inner_Height (Window : Window_Type) return Integer;

   procedure Inner_Width (Window : in out Window_Type; Value : in Integer);
   function Inner_Width (Window : Window_Type) return Integer;

   procedure Outer_Height (Window : in out Window_Type; Value : in Integer);
   function Outer_Height (Window : Window_Type) return Integer;

   procedure Outer_Width (Window : in out Window_Type; Value : in Integer);
   function Outer_Width (Window : Window_Type) return Integer;

   function X_Offset (Window : Window_Type) return Integer;

   function Y_Offset (Window : Window_Type) return Integer;

   function Top (Window : Window_Type) return Integer;

   function Left (Window : Window_Type) return Integer;

   function Search_Parameter (Window : Window_Type; Name  : String)
                              return String;
   --  Returns the value of parameters passed in on URL. Returns "undefined"
   --  if Name is not in URL search parameters.
   --  For example: http://localhost:8080/?page_id=2
   --  Search_Parameter (Window, "page_id") = "2"

   --  Framework Properties  --

   function Gnoga_Session_ID (Window : Window_Type; Name : String := "gid")
                              return String;
   --  If Name exists in Client.Storage.Session_Storage it returns that value,
   --  if not a unique Session ID is generated and stored for future
   --  invocations.

   -------------------------------------------------------------------------
   --  Window_Type - Methods
   -------------------------------------------------------------------------

   procedure Alert (Window : in out Window_Type; Message : String);
   --  Display Alert box on Window with Message

   procedure Log (Window : in out Window_Type; Message : String);
   --  Log message on browser console

   procedure Error (Window : in out Window_Type; Message : String);
   --  Log error message on browser console

   procedure Print (Window : in out Window_Type);
   --  Print Window contents

   procedure Scroll_By (Window : in out Window_Type; X, Y : Integer);
   --  Scroll contents in window by x, y

   procedure Scroll_To (Window : in out Window_Type; X, Y : Integer);
   --  Scroll contents in window to x, y


   -- Window Placement Methods --
   --
   -- These methods will only work on child windows that have been launched

   procedure Close (Window : in out Window_Type);

   procedure Resize_By (Window : in out Window_Type; Width, Height : Integer);

   procedure Resize_To (Window : in out Window_Type; Width, Height : Integer);

   procedure Move_By (Window : in out Window_Type; X, Y : Integer);

   procedure Move_To (Window : in out Window_Type; X, Y : Integer);

   -------------------------------------------------------------------------
   --  Window_Type - Event Handlers
   -------------------------------------------------------------------------

   procedure On_Abort_Handler (Window  : in out Window_Type;
                               Handler : in     Gnoga.Gui.Base.Action_Event);
   procedure Fire_On_Abort (Window : in out Window_Type);

   procedure On_Error_Handler (Window  : in out Window_Type;
                               Handler : in     Gnoga.Gui.Base.Action_Event);
   procedure Fire_On_Error (Window : in out Window_Type);

   procedure On_Hash_Change_Handler
     (Window  : in out Window_Type;
      Handler : in     Gnoga.Gui.Base.Action_Event);
   procedure Fire_On_Hash_Change (Window : in out Window_Type);

   procedure On_Orientation_Change_Handler
     (Window  : in out Window_Type;
      Handler : in     Gnoga.Gui.Base.Action_Event);
   procedure Fire_On_Orientation_Change (Window : in out Window_Type);

   procedure On_Storage_Handler (Window  : in out Window_Type;
                                 Handler : in     Gnoga.Gui.Base.Action_Event);
   procedure Fire_On_Storage (Window : in out Window_Type);
   -- local or sessin storage data was changed

   -------------------------------------------------------------------------
   --  Winow_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Resize (Window : in out Window_Type);
   --  Handle resizing an object set as a View for Window to new
   --  height and width of Window

   overriding
   procedure On_Message (Object  : in out Window_Type;
                         Event   : in     String;
                         Message : in     String);
private
   type Window_Type is new Gnoga.Gui.Base.Base_Type with
      record
         DOM_Document : aliased Gnoga.Gui.Document.Document_Type;
         Location     : aliased Gnoga.Gui.Location.Location_Type;
         View_ID      : Gnoga.Types.Web_ID;
         Has_View     : Boolean := False;

         On_Abort_Event              : Gnoga.Gui.Base.Action_Event := null;
         On_Error_Event              : Gnoga.Gui.Base.Action_Event := null;
         On_Hash_Change_Event        : Gnoga.Gui.Base.Action_Event := null;
         On_Orientation_Change_Event : Gnoga.Gui.Base.Action_Event := null;
         On_Storage_Event            : Gnoga.Gui.Base.Action_Event := null;
      end record;
end Gnoga.Gui.Window;
