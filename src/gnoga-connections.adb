------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                     G N O G A . C O N N E C I O N S                      --
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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with Ada.Containers.Ordered_Maps;

with AWS.Config;
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Services.Web_Block.Registry;
with AWS.Response;
with AWS.Services.Dispatchers.URI;
with AWS.Status;
with AWS.Server;
with AWS.Dispatchers;
with AWS.Net.Websocket;
with AWS.Net.WebSocket.Registry;
with AWS.Net.WebSocket.Registry.Control;

with GNAT.Traceback.Symbolic;

package body Gnoga.Connections is
   use type Gnoga.Types.Unique_ID;

   Boot_HTML : Ada.Strings.Unbounded.Unbounded_String;

   Web_Server         : AWS.Server.HTTP;
   Web_Config         : AWS.Config.Object;
   Web_Dispatcher     : AWS.Services.Dispatchers.URI.Handler;

   On_Connect_Event : Connect_Event := null;

   -------------
   -- Default --
   -------------

   type Default is new AWS.Services.Dispatchers.URI.Handler with null record;
   --  Handle everything not covered by the other dispatchers (CSS, Image)

   overriding function Dispatch
     (Dispatcher : in Default;
      Request    : in AWS.Status.Data) return AWS.Response.Data;

   ---------
   -- CSS --
   ---------

   type CSS is new AWS.Services.Dispatchers.URI.Handler with null record;

   overriding function Dispatch
     (Dispatcher : in CSS;
      Request    : in AWS.Status.Data) return AWS.Response.Data;

   ---------
   -- JS --
   ---------

   type JS is new AWS.Services.Dispatchers.URI.Handler with null record;

   overriding function Dispatch
     (Dispatcher : in JS;
      Request    : in AWS.Status.Data) return AWS.Response.Data;

   -----------
   -- Image --
   -----------

   type Image is new AWS.Services.Dispatchers.URI.Handler with null record;

   overriding function Dispatch
     (Dispatcher : in Image;
      Request    : in AWS.Status.Data) return AWS.Response.Data;

   ------------------
   --  Socket_Type --
   ------------------

   type Socket_Type is new AWS.Net.WebSocket.Object with null record;

   function Socket_Type_Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data)
      return AWS.Net.WebSocket.Object'Class;

   overriding procedure On_Open
     (Web_Socket : in out Socket_Type; Message : String);

   overriding procedure On_Close
     (Web_Socket : in out Socket_Type; Message : String);

   overriding procedure On_Error
     (Web_Socket : in out Socket_Type; Message : String);

   overriding procedure On_Message
     (Web_Socket : in out Socket_Type; Message : String);

   Web_Root : Ada.Strings.Unbounded.Unbounded_String;

   Default_Dispatcher : Default;
   CSS_Dispatcher     : CSS;
   JS_Dispatcher      : JS;
   Image_Dispatcher   : Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Host : in String  := "";
                         Port : in Integer := 8080;
                         Boot : in String  := "boot.html")
   is
      Config_Root : constant String := AWS.Config.WWW_Root (Web_Config);
   begin
      --  Setup server
      AWS.Config.Set.Reuse_Address (Web_Config, True);
      AWS.Config.Set.Server_Host   (Web_Config, Host);
      AWS.Config.Set.Server_Port   (Web_Config, Port);

      Boot_HTML := Ada.Strings.Unbounded.To_Unbounded_String ("/" & Boot);

      if Config_Root = "./" or else Config_Root = "" then
         Web_Root := Ada.Strings.Unbounded.To_Unbounded_String ("../");
      else
         Web_Root := Ada.Strings.Unbounded.To_Unbounded_String (Config_Root);
      end if;

      Write_To_Console ("Starting Web Server with web root at " &
                              Ada.Strings.Unbounded.To_String (Web_Root));
      Write_To_Console ("Starting Websocket Server");
      AWS.Net.WebSocket.Registry.Register ("/echo", Socket_Type_Create'Access);
      AWS.Net.WebSocket.Registry.Control.Start;

      --  Setup dispatchers

      AWS.Services.Dispatchers.URI.Register
        (Web_Dispatcher,
         URI    => "/css/",
         Action => CSS_Dispatcher,
         Prefix => True);

      AWS.Services.Dispatchers.URI.Register
        (Web_Dispatcher,
         URI    => "/js/",
         Action => JS_Dispatcher,
         Prefix => True);

      AWS.Services.Dispatchers.URI.Register
        (Web_Dispatcher,
         URI    => "/img/",
         Action => Image_Dispatcher,
         Prefix => True);

      AWS.Services.Dispatchers.URI.Register_Default_Callback
        (Web_Dispatcher,
         Action => Default_Dispatcher);
   end Initialize;


   ---------
   -- Run --
   ---------

   procedure Run (Wait_for_Q : in Boolean := True) is
   begin
      --  Start the server

      AWS.Server.Start (Web_Server, Web_Dispatcher, Web_Config);

      if (Wait_For_Q) then
         --  Wait for the Q key
         Write_To_Console ("Press the Q key to close server.");
         AWS.Server.Wait (AWS.Server.Q_Key_Pressed);

         AWS.Server.Shutdown (Web_Server);
      else
         AWS.Server.Wait (AWS.Server.No_Server);
      end if;
   end Run;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      AWS.Server.Shutdown (Web_Server);
   end Stop;

   ----------------------
   -- Default Dispatch --
   ----------------------

   overriding function Dispatch
     (Dispatcher : in Default; Request : AWS.Status.Data) return AWS.Response.Data
   is
      pragma Unreferenced (Dispatcher);

      function Adjusted_URI return String is
         URI : constant String := AWS.Status.URI (Request);
      begin
         if URI = "/" then
            return Ada.Strings.Unbounded.To_String (Boot_HTML);
         else
            return URI;
         end if;
      end Adjusted_URI;

      File : constant String :=
               Ada.Strings.Unbounded.To_String (Web_Root) & "html" & Adjusted_URI;
   begin
      if Ada.Directories.Exists (File) then
         return AWS.Response.File
           (Content_Type => AWS.MIME.Content_Type (File),
            Filename     => File);
      else
         return AWS.Response.Acknowledge (AWS.Messages.S404);
      end if;
   end Dispatch;

   ------------------
   -- CSS Dispatch --
   ------------------

   function Dispatch
     (Dispatcher : in CSS;
      Request    : in AWS.Status.Data) return AWS.Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI  : constant String := AWS.Status.URI (Request);
      File : constant String :=
        Ada.Strings.Unbounded.To_String (Web_Root) &
        URI (URI'First + 1 .. URI'Last);
   begin
      if Ada.Directories.Exists (File) then
         return AWS.Response.File
           (Content_Type => AWS.MIME.Text_CSS,
            Filename     => File);
      else
         return AWS.Response.Acknowledge (AWS.Messages.S404);
      end if;
   end Dispatch;

   -----------------
   -- JS Dispatch --
   -----------------

   function Dispatch
     (Dispatcher : in JS;
      Request    : in AWS.Status.Data) return AWS.Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI  : constant String := AWS.Status.URI (Request);
      File : constant String :=
        Ada.Strings.Unbounded.To_String (Web_Root) &
        URI (URI'First + 1 .. URI'Last);
   begin
      if Ada.Directories.Exists (File) then
	    return AWS.Response.File
	      (Content_Type => AWS.MIME.Text_Javascript,
	       Filename     => File);
      else
         return AWS.Response.Acknowledge (AWS.Messages.S404);
      end if;
   end Dispatch;

   --------------------
   -- Image Dispatch --
   --------------------

   function Dispatch
     (Dispatcher : in Image;
      Request    : in AWS.Status.Data) return AWS.Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI  : constant String := AWS.Status.URI (Request);
      File : constant String :=
        Ada.Strings.Unbounded.To_String (Web_Root) &
        URI (URI'First + 1 .. URI'Last);
   begin
      if Ada.Directories.Exists (File) then
         return AWS.Response.File
           (Content_Type => AWS.MIME.Content_Type (File),
            Filename     => File);
      else
         return AWS.Response.Acknowledge (AWS.Messages.S404);
      end if;
   end Dispatch;

   ----------------------------
   -- Connection_Holder_Type --
   ----------------------------

   protected body Connection_Holder_Type is
      entry Hold when not Connected is
      begin
         null;
         -- Semiphore does not reset itself to a blocking state.
         -- This insures that if Released before Hold that Hold
         -- will not block and connection will be released.
      end;

      procedure Release is
      begin
         Connected := False;
      end;
   end Connection_Holder_Type;

   type Connection_Holder_Access is access all Connection_Holder_Type;

   package Connection_Holder_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Types.Unique_ID, Connection_Holder_Access);

   ------------------------
   -- Connection Manager --
   ------------------------

   package Socket_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Types.Connection_ID, Socket_Type);
   --  Socket Maps are used for the Connection Manager to map connection IDs
   --  to web sockets.

   protected type Connection_Manager_Type is
      procedure Add_Connection (Socket : in  Socket_Type;
                                New_ID : out Gnoga.Types.Connection_ID);
      --  Adds Socket to managed Connections and generates a New_ID.

      procedure Add_Connection_Holder (ID     : in Gnoga.Types.Connection_ID;
                                       Holder : in Connection_Holder_Access);
      --  Adds a connection holder to the connection
      --  Can only be one at any given time.

      procedure Delete_Connection_Holder (ID : in Gnoga.Types.Connection_ID);
      --  Delete connection holder

      procedure Delete_Connection (ID : in Gnoga.Types.Connection_ID);
      --  Delete Connection with ID.
      --  Releases connection holder if present.

      procedure Delete_Connection (Socket : in Socket_Type);
      --  Delete Connection associated with Socket.
      --  Releases connection holder if present.

      function Valid (ID : in Gnoga.Types.Connection_ID) return Boolean;
      --  Return True if ID is in connection map.

      function Connection_Socket (ID : in Gnoga.Types.Connection_ID)
                                  return Socket_Type;
      --  Return the Socket_Type associated with ID
      --  Raises Connection_Error if ID is not Valid

      function Find_Connetion_ID (Socket : Socket_Type)
                                  return Gnoga.Types.Connection_ID;
      --  Find the Connetion_ID related to Socket.
   private
      Socket_Map            : Socket_Maps.Map;
      Socket_Count          : Gnoga.Types.Connection_ID := 0;
      Connection_Holder_Map : Connection_Holder_Maps.Map;
   end Connection_Manager_Type;

   protected body Connection_Manager_Type is
      procedure Add_Connection (Socket : in  Socket_Type;
                                New_ID : out Gnoga.Types.Connection_ID)
      is
      begin
         Socket_Count:= Socket_Count + 1;
         New_ID := Socket_Count;
         Socket_Map.Insert (New_ID, Socket);
      end Add_Connection;

      procedure Add_Connection_Holder (ID     : in Gnoga.Types.Connection_ID;
                                       Holder : in Connection_Holder_Access)
      is
      begin
         Connection_Holder_Map.Insert (ID, Holder);
      end Add_Connection_Holder;

      procedure Delete_Connection_Holder (ID : in Gnoga.Types.Connection_ID)
      is
      begin
         if Connection_Holder_Map.Contains (ID) then
            Connection_Holder_Map.Delete (ID);
         end if;
      end Delete_Connection_Holder;

      procedure Delete_Connection (ID : in Gnoga.Types.Connection_ID) is
      begin
         if (ID > 0) then
            if Connection_Holder_Map.Contains (ID) then
               Connection_Holder_Map.Element (ID).Release;
               Connection_Holder_Map.Delete (ID);
            end if;

            Socket_Map.Delete (ID);
         end if;
      exception
         when others =>
            null;
      end Delete_Connection;

      procedure Delete_Connection (Socket : in Socket_Type) is
      begin
         Delete_Connection (Find_Connetion_ID (Socket));
      end Delete_Connection;

      function Valid (ID : in Gnoga.Types.Connection_ID) return Boolean is
      begin
         return Socket_Map.Contains (ID);
      end Valid;

      function Connection_Socket (ID : in Gnoga.Types.Connection_ID)
                                  return Socket_Type
      is
      begin
         return Socket_Map.Element (ID);
      exception
         when others =>
            raise Connection_Error;
      end Connection_Socket;

      function Find_Connetion_ID (Socket : Socket_Type)
                               return Gnoga.Types.Connection_ID
      is
         use type Socket_Maps.Cursor;

         Cursor : Socket_Maps.Cursor := Socket_Map.First;
      begin
         -- ? This needs optimization

         while Cursor /= Socket_Maps.No_Element loop
            if Socket_Maps.Element (Cursor) = Socket then
               return Socket_Maps.Key (Cursor);
            else
               Socket_Maps.Next (Cursor);
            end if;
         end loop;

         return -1;
      end Find_Connetion_ID;
   end Connection_Manager_Type;

   Connection_Manager : Connection_Manager_Type;

   ---------------------------
   -- Message Queue Manager --
   ---------------------------

   function "=" (Left, Right : Gnoga.Base.Pointer_To_Base_Class)
                 return Boolean
   is
   begin
      return Left.Unique_ID = Right.Unique_ID;
   end "=";

   package Object_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Types.Unique_ID, Gnoga.Base.Pointer_To_Base_Class);
   -- ? needs to be protected

   Object_Map : Object_Maps.Map;

   -------------------
   -- Socket_Create --
   -------------------

   function Socket_Type_Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data)
      return AWS.Net.WebSocket.Object'Class
   is
   begin
      return Socket_Type'(AWS.Net.WebSocket.Object
                          (AWS.Net.WebSocket.Create (Socket, Request))
                          with null record);
   end Socket_Type_Create;

   -------------
   -- On_Open --
   -------------

   task type Event_Task_Type (ID : Gnoga.Types.Connection_ID) is
      entry Start;
   end Event_Task_Type;

   task body Event_Task_Type is
      Connection_Holder : aliased Connection_Holder_Type;
   begin
      accept Start;
      Connection_Manager.Add_Connection_Holder
        (ID, Connection_Holder'Unchecked_Access);

      On_Connect_Event (ID, Connection_Holder'Access);

      Connection_Manager.Delete_Connection_Holder (ID);
   exception
      when E : others =>
         Log ("Connection ID=" & ID'Img);
         Log (Ada.Exceptions.Exception_Name (E) & " - " &
                Ada.Exceptions.Exception_Message (E));
         Log (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end Event_Task_Type;

   New_Event_Task : access Event_Task_Type;
   -- ? Ada.Task_Termination should be used for cleanup of dangling task TCBs
   --  If New_Event_Task is located with in the on_open it will prevent
   --  on_open from completing.

   overriding procedure On_Open
     (Web_Socket : in out Socket_Type; Message : String)
   is
      use type Gnoga.Types.Connection_ID;

      ID : Gnoga.Types.Connection_ID;
   begin
      Connection_Manager.Add_Connection (Socket => Web_Socket,
                                         New_ID => ID);

      if On_Connect_Event /= null then
         New_Event_Task := new Event_Task_Type (ID);
         New_Event_Task.Start;
      end if;
   end On_Open;

   --------------
   -- On_Close --
   --------------

   overriding procedure On_Close
     (Web_Socket : in out Socket_Type;
      Message : String)
   is
   begin
      Connection_Manager.Delete_Connection (Socket => Web_Socket);
   end On_Close;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (Web_Socket : in out Socket_Type;
      Message : String)
   is
   begin
      Connection_Manager.Delete_Connection (Web_Socket);
   end On_Error;

   --------------------
   -- Script_Manager --
   --------------------

   protected type Script_Holder_Type is
      entry Hold;
      procedure Release (Result : in String);
      function Result return String;
   private
      Connected     : Boolean := True;
      Script_Result : Ada.Strings.Unbounded.Unbounded_String;
   end Script_Holder_Type;

   protected body Script_Holder_Type is
      entry Hold when not Connected is
      begin
         null;
         -- Semiphore does not reset itself to a blocking state.
         -- This insures that if Released before Hold that Hold
         -- will not block and connection will be released.
      end;

      procedure Release (Result : in String) is
      begin
         Connected := False;
         Script_Result := Ada.Strings.Unbounded.To_Unbounded_String (Result);
      end;

      function Result return String is
      begin
         return Ada.Strings.Unbounded.To_String (Script_Result);
      end Result;
   end Script_Holder_Type;

   type Script_Holder_Access is access all Script_Holder_Type;

   package Script_Holder_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Types.Unique_ID, Script_Holder_Access);


   protected type Script_Manager_Type is
      procedure Add_Script_Holder (ID     : out Gnoga.Types.Unique_ID;
                                   Holder : in  Script_Holder_Access);
      --  Adds a script holder to wait for script execution to end
      --  and return results;

      procedure Delete_Script_Holder (ID : in Gnoga.Types.Unique_ID);
      --  Delete script holder

      procedure Release_Hold (ID     : in Gnoga.Types.Unique_ID;
                              Result : in String);
      --  Delete conneciton hold with ID.
   private
      Script_Holder_Map : Script_Holder_Maps.Map;
      Script_ID         : Gnoga.Types.Unique_ID := 0;
   end Script_Manager_Type;

   protected body Script_Manager_Type is
      procedure Add_Script_Holder (ID     : out Gnoga.Types.Connection_ID;
                                   Holder : in Script_Holder_Access)
      is
      begin
         Script_ID := Script_ID + 1;
         Script_Holder_Map.Insert (Script_ID, Holder);

         ID := Script_ID;
      end Add_Script_Holder;

      procedure Delete_Script_Holder (ID : in Gnoga.Types.Connection_ID) is
      begin
         Script_Holder_Map.Delete (ID);
      end Delete_Script_Holder;

      procedure Release_Hold (ID     : in Gnoga.Types.Unique_ID;
                              Result : in String)
      is
      begin
         if Script_Holder_Map.Contains (ID) then
            Script_Holder_Map.Element (ID).Release (Result);
         end if;
      end Release_Hold;
   end Script_Manager_Type;

   Script_Manager : Script_Manager_Type;

   ----------------
   -- On_Message --
   ----------------

   task type Dispatch_Task_Type (Object : Gnoga.Base.Pointer_To_Base_Class) is
      entry Start (Event : in String; Data : in String);
   end Dispatch_Task_Type;

   task body Dispatch_Task_Type is
      E : Ada.Strings.Unbounded.Unbounded_String;
      D : Ada.Strings.Unbounded.Unbounded_String;
   begin
      accept Start (Event : in String; Data : in String) do
         E := Ada.Strings.Unbounded.To_Unbounded_String (Event);
         D := Ada.Strings.Unbounded.To_Unbounded_String (Data);
      end Start;

      Object.On_Message (Ada.Strings.Unbounded.To_String (E),
                         Ada.Strings.Unbounded.To_String (D));
   exception
      when E : others =>
         Log ("Dispatch Error");
         Log (Ada.Exceptions.Exception_Name (E) & " - " &
                Ada.Exceptions.Exception_Message (E));
         Log (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end Dispatch_Task_Type;

   New_Dispatch_Task : access Dispatch_Task_Type;
   -- ? Ada.Task_Termination should be used for cleanup of dangling task TCBs


   overriding procedure On_Message
     (Web_Socket : in out Socket_Type;
      Message    : String)
   is
      use Ada.Strings.Fixed;
   begin
      if Message (Message'First) = 'S' then
         declare
            P1 : Integer := Index (Source  => Message,
                                   Pattern => "|");

            UID    : String := Message (Message'First + 2 .. (P1 - 1));
            Result : String := Message ((P1 + 1) .. Message'Last);
         begin
            Script_Manager.Release_Hold (Gnoga.Types.Unique_ID'Value (UID),
                                         Result);
         end;
      else
         declare
            P1 : Integer := Index (Source  => Message,
                                   Pattern => "|");

            P2 : Integer := Index (Source  => Message,
                                   Pattern => "|",
                                   From    => P1 + 1);

            UID        : String := Message (Message'First .. (P1 - 1));
            Event      : String := Message ((P1 + 1) .. (P2 - 1));
            Event_Data : String := Message ((P2 + 1) .. Message'Last);

            Object : Gnoga.Base.Pointer_To_Base_Class :=
              Object_Map.Element (Integer'Value (UID));
         begin
            New_Dispatch_Task := new Dispatch_Task_Type (Object);
            New_Dispatch_Task.Start (Event, Event_Data);
         end;
      end if;
   end On_Message;

   --------------------
   -- Execute_Script --
   --------------------

   -- ? Need some sort of try block in JS to capture JS errors and then trow
   --   exceptions.

   procedure Execute_Script (ID     : in Gnoga.Types.Connection_ID;
                             Script : in String)
   is
   begin
      declare
         Socket  : Socket_Type := Connection_Manager.Connection_Socket(ID);
      begin
         Socket.Send (Script);
      end;
   end Execute_Script;

   function Execute_Script (ID     : in Gnoga.Types.Connection_ID;
                            Script : in String)
                            return String
   is
      Script_Holder : aliased Script_Holder_Type;
   begin
      declare
         Script_ID : Gnoga.Types.Unique_ID;
         Socket    : Socket_Type := Connection_Manager.Connection_Socket(ID);
      begin
         Script_Manager.Add_Script_Holder
           (ID     => Script_ID,
            Holder => Script_Holder'Unchecked_Access);

         declare
            Message : constant String := "ws.send (" &
              """S" & Script_ID'Img & "|""+" &
              "eval (""" & Escape_Quotes (Script) & """)" &
              ");";
            begin
               Socket.Send (Message);
            end;

         select
            delay 3.0;
            Script_Manager.Delete_Script_Holder (Script_ID);
            raise Script_Error;
         then abort
            Script_Holder.Hold;
         end select;

         Script_Manager.Delete_Script_Holder (Script_ID);

         return Script_Holder.Result;
      end;
   end Execute_Script;

   ------------------------
   -- On_Connect_Handler --
   ------------------------

   procedure On_Connect_Handler (Event : in Connect_Event) is
   begin
      On_Connect_Event := Event;
   end On_Connect_Handler;

   -----------
   -- Valid --
   -----------

   function Valid (ID : Gnoga.Types.Connection_ID) return Boolean is
   begin
      return Connection_Manager.Valid (ID);
   end Valid;

   ---------------------
   -- ID_Machine_Type --
   ---------------------

   protected type ID_Machine_Type is
      procedure Next_ID (ID : out Gnoga.Types.Unique_ID);
   private
      Current_ID : Gnoga.Types.Unique_ID := 0;
   end ID_Machine_Type;

   protected body ID_Machine_Type is
      procedure Next_ID (ID : out Gnoga.Types.Unique_ID) is
      begin
         Current_ID := Current_ID + 1;
         ID := Current_ID;
      end;
   end ID_Machine_Type;

   ID_Machine : ID_Machine_Type;

   -------------------
   -- New_Unique_ID --
   -------------------

   procedure New_Unique_ID (New_ID : out Gnoga.Types.Unique_ID) is
   begin
      ID_Machine.Next_ID (New_ID);
   end New_Unique_ID;

   --------------------------
   -- Add_To_Message_Queue --
   --------------------------

   procedure Add_To_Message_Queue (Object : in out Gnoga.Base.Base_Type'Class) is
   begin
      Object_Map.Insert (Key      => Object.Unique_ID,
                         New_Item => Object'Unchecked_Access);
   end Add_To_Message_Queue;

   -------------------------------
   -- Delete_From_Message_Queue --
   -------------------------------

   procedure Delete_From_Message_Queue
     (Object : in out Gnoga.Base.Base_Type'Class) is
   begin
      Object_Map.Delete (Key => Object.Unique_ID);
   end Delete_From_Message_Queue;
end Gnoga.Connections;
