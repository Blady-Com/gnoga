------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                 G N O G A . S E R V E R . C O N N E C I O N              --
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
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;

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
with AWS.Parameters;
with AWS.Net.WebSocket;
with AWS.Net.WebSocket.Registry;
with AWS.Net.WebSocket.Registry.Control;

with Gnoga.Server.Template_Parser.Simple;

package body Gnoga.Server.Connection is
   use type Gnoga.Types.Unique_ID;
   use type Gnoga.Types.Pointer_to_Connection_Data_Class;

   Boot_HTML : Ada.Strings.Unbounded.Unbounded_String;

   Verbose_Output : Boolean := False;

   Web_Server         : AWS.Server.HTTP;
   Web_Config         : AWS.Config.Object;
   Web_Dispatcher     : AWS.Services.Dispatchers.URI.Handler;

   On_Connect_Event : Connect_Event := null;
   On_Post_Event    : Post_Event    := null;

   Exit_Application_Requested : Boolean := False;

   task Watchdog is
      entry Start;
      entry Stop;
   end Watchdog;
   --  Keep alive and check connections

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

   Default_Dispatcher : Default;
   CSS_Dispatcher     : CSS;
   JS_Dispatcher      : JS;
   Image_Dispatcher   : Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Host    : in String  := "";
                         Port    : in Integer := 8080;
                         Boot    : in String  := "boot.html";
                         Verbose : in Boolean := True)
   is
   begin
      Verbose_Output := Verbose;

      --  Setup server
      AWS.Config.Set.Reuse_Address    (Web_Config, True);
      AWS.Config.Set.Server_Host      (Web_Config, Host);
      AWS.Config.Set.Server_Port      (Web_Config, Port);

      --  AWS.Config.Set.Max_WebSocket                (1024);
      --  AWS.Config.Set.Max_WebSocket_Handler        (100);
      --  AWS.Config.Set.WebSocket_Message_Queue_Size (100);

      AWS.Config.Set.Upload_Directory (Web_Config,
                                       Gnoga.Server.Upload_Directory);

      Boot_HTML := Ada.Strings.Unbounded.To_Unbounded_String ("/" & Boot);

      if Verbose then
         Write_To_Console ("Application root :" & Application_Directory);
         Write_To_Console ("Executable at    :" & Executable_Directory);
         Write_To_Console ("HTML root        :" & HTML_Directory);
         Write_To_Console ("Upload directory :" & Upload_Directory);
         Write_To_Console ("Templates root   :" & Templates_Directory);
         Write_To_Console ("/js  at          :" & JS_Directory);
         Write_To_Console ("/css at          :" & CSS_Directory);
         Write_To_Console ("/img at          :" & IMG_Directory);
         Write_To_Console ("Listening on     :" & Host & ":" &
                             Left_Trim (Port'Img));
      end if;

      AWS.Net.WebSocket.Registry.Register ("/gnoga",
                                           Socket_Type_Create'Access);
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

      Watchdog.Start;
   end Initialize;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      --  Start the server

      AWS.Server.Start (Web_Server, Web_Dispatcher, Web_Config);

      AWS.Server.Wait (AWS.Server.No_Server);

      Exit_Application_Requested := True;
   end Run;

   -------------------
   -- Shutting_Down --
   -------------------

   function Shutting_Down return Boolean is
   begin
      return Exit_Application_Requested;
   end Shutting_Down;

   ----------------------
   -- Default Dispatch --
   ----------------------

   overriding function Dispatch
     (Dispatcher : in Default; Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Dispatcher);

      use type AWS.Status.Request_Method;
      use Ada.Strings.Unbounded;

      URI : constant String := AWS.Status.URI (Request);

      function Adjusted_URI return String;

      function Adjusted_URI return String is
      begin
         if URI = "/" then
            return Ada.Strings.Unbounded.To_String (Boot_HTML);
         else
            return URI;
         end if;
      end Adjusted_URI;

      R      : Ada.Strings.Unbounded.Unbounded_String;
      Params : Gnoga.Types.Data_Map_Type;
      V      : Gnoga.Server.Template_Parser.View_Data;
      File   : constant String := HTML_Directory & Adjusted_URI;
   begin
      if AWS.Status.Method (Request) = AWS.Status.POST then
         R := To_Unbounded_String ("--><script>");

         declare
            P : constant AWS.Parameters.List :=
                  AWS.Status.Parameters (Request);

            procedure Add_Param (Name, Value : in String);

            procedure Add_Param (Name, Value : in String) is
            begin
               Params.Insert (Name, Value);
            end Add_Param;
         begin
            if P.Count > 0 then
               P.Iterate_Names ("|", Add_Param'Access);
            end if;
         end;

         if On_Post_Event /= null then
            On_Post_Event (URI, Params);
         end if;

         for C in Gnoga.Types.Data_Maps.Iterate (Params) loop
            R := R  & "params['" & Gnoga.Types.Data_Maps.Key (C) & "']=""" &
              Escape_Quotes (Gnoga.Types.Data_Maps.Element (C)) & """; ";
         end loop;

         R := R & "</script><!--";

         V.Insert ("push_params", To_String (R));

         if Ada.Directories.Exists (File) and
           AWS.MIME.Content_Type (File) = AWS.MIME.Text_HTML
         then
            return AWS.Response.Build
              (Content_Type => AWS.MIME.Text_HTML,
               Message_Body => Gnoga.Server.Template_Parser.Simple.Load_View
                 (File, V));
         else
            return AWS.Response.Build
              (Content_Type => AWS.MIME.Text_HTML,
               Message_Body => Gnoga.Server.Template_Parser.Simple.Load_View
                 (HTML_Directory & To_String (Boot_HTML), V));
         end if;
      end if;

      if Ada.Directories.Exists (File) then
         return AWS.Response.File
           (Content_Type => AWS.MIME.Content_Type (File),
            Filename     => File);
      else
         --  Let application handle files not found in /html
         return AWS.Response.File
           (Content_Type => AWS.MIME.Text_HTML,
            Filename     => HTML_Directory & To_String (Boot_HTML));
      end if;
   end Dispatch;

   ------------------
   -- CSS Dispatch --
   ------------------

   overriding
   function Dispatch
     (Dispatcher : in CSS;
      Request    : in AWS.Status.Data) return AWS.Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI  : constant String := AWS.Status.URI (Request);
      File : constant String :=
               CSS_Directory & URI (URI'First + 5 .. URI'Last);
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

   overriding
   function Dispatch
     (Dispatcher : in JS;
      Request    : in AWS.Status.Data) return AWS.Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI  : constant String := AWS.Status.URI (Request);
      File : constant String :=
               JS_Directory & URI (URI'First + 4 .. URI'Last);
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

   overriding
   function Dispatch
     (Dispatcher : in Image;
      Request    : in AWS.Status.Data) return AWS.Response.Data
   is
      pragma Unreferenced (Dispatcher);
      URI  : constant String := AWS.Status.URI (Request);
      File : constant String :=
               IMG_Directory & URI (URI'First + 5 .. URI'Last);

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
         --  Semiphore does not reset itself to a blocking state.
         --  This ensures that if Released before Hold that Hold
         --  will not block and connection will be released.
         --  It also allows for On_Connect Handler to not have to use
         --  Connection.Hold unless there is a desire code such as to
         --  clean up after a connetion is ended.
      end Hold;

      procedure Release is
      begin
         Connected := False;
      end Release;
   end Connection_Holder_Type;

   type Connection_Holder_Access is access all Connection_Holder_Type;

   package Connection_Holder_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Types.Unique_ID, Connection_Holder_Access);

   package Connection_Data_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Types.Unique_ID, Gnoga.Types.Pointer_to_Connection_Data_Class);

   ---------------------
   -- Event_Task_Type --
   ---------------------

   task type Event_Task_Type (ID : Gnoga.Types.Connection_ID);

   type Event_Task_Access is access all Event_Task_Type;

   procedure Free_Event_Task is
        new Ada.Unchecked_Deallocation (Event_Task_Type,
                                        Event_Task_Access);

   package Event_Task_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Types.Unique_ID, Event_Task_Access);

   ------------------------
   -- Connection Manager --
   ------------------------

   package Socket_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Types.Connection_ID, Socket_Type);
   --  Socket Maps are used for the Connection Manager to map connection IDs
   --  to web sockets.

   Socket_Map  : Socket_Maps.Map;

   protected Connection_Manager is
      procedure Add_Connection (Socket : in  Socket_Type;
                                New_ID : out Gnoga.Types.Connection_ID);
      --  Adds Socket to managed Connections and generates a New_ID.

      procedure Start_Connection (New_ID : in Gnoga.Types.Connection_ID);
      --  Start event task on connection

      procedure Swap_Connection (New_ID : in Gnoga.Types.Connection_ID;
                                 Old_ID : in Gnoga.Types.Connection_ID);
      --  Reconnect old connection

      procedure Add_Connection_Holder (ID     : in Gnoga.Types.Connection_ID;
                                       Holder : in Connection_Holder_Access);
      --  Adds a connection holder to the connection
      --  Can only be one at any given time.

      procedure Add_Connection_Data
        (ID   : in Gnoga.Types.Connection_ID;
         Data : in Gnoga.Types.Pointer_to_Connection_Data_Class);
      --  Adds data to be associated with connection

      function Connection_Data
        (ID : in Gnoga.Types.Connection_ID)
         return Gnoga.Types.Pointer_to_Connection_Data_Class;
      --  Returns the Connection_Data associated with ID

      procedure Delete_Connection_Holder (ID : in Gnoga.Types.Connection_ID);
      --  Delete connection holder

      procedure Delete_Connection (ID : in Gnoga.Types.Connection_ID);
      --  Delete Connection with ID.
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

      procedure Delete_All_Connections;
      --  Called by Stop to close down server
   private
      Socket_Count          : Gnoga.Types.Connection_ID := 0;
      Connection_Holder_Map : Connection_Holder_Maps.Map;
      Connection_Data_Map   : Connection_Data_Maps.Map;
      Event_Task_Map        : Event_Task_Maps.Map;
   end Connection_Manager;

   protected body Connection_Manager is
      procedure Add_Connection (Socket : in  Socket_Type;
                                New_ID : out Gnoga.Types.Connection_ID)
      is
      begin
         Socket_Count := Socket_Count + 1;
         New_ID := Socket_Count;
         Socket_Map.Insert (New_ID, Socket);
      end Add_Connection;

      procedure Start_Connection (New_ID : in Gnoga.Types.Connection_ID)
      is
      begin
         Event_Task_Map.Insert (New_ID, new Event_Task_Type (New_ID));
      end Start_Connection;

      procedure Swap_Connection (New_ID : in Gnoga.Types.Connection_ID;
                                 Old_ID : in Gnoga.Types.Connection_ID)
      is
      begin
         if Socket_Map.Contains (Old_ID) then
            Socket_Map.Replace (Old_ID, Socket_Map.Element (New_ID));
         else
            raise Connection_Error with "Old connection already gone";
         end if;
      end Swap_Connection;

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

      procedure Add_Connection_Data
        (ID   : in Gnoga.Types.Connection_ID;
         Data : in Gnoga.Types.Pointer_to_Connection_Data_Class)
      is
      begin
         Connection_Data_Map.Include (ID, Data);
      end Add_Connection_Data;

      function Connection_Data
        (ID : in Gnoga.Types.Connection_ID)
         return Gnoga.Types.Pointer_to_Connection_Data_Class
      is
      begin
         if Connection_Data_Map.Contains (ID) then
            return Connection_Data_Map.Element (ID);
         else
            return null;
         end if;
      end Connection_Data;

      procedure Delete_Connection (ID : in Gnoga.Types.Connection_ID) is
      begin
         if (ID > 0) then
            if Connection_Holder_Map.Contains (ID) then
               Connection_Holder_Map.Element (ID).Release;
               Connection_Holder_Map.Delete (ID);
            end if;

            if Connection_Data_Map.Contains (ID) then
               Connection_Data_Map.Delete (ID);
            end if;

            Socket_Map.Delete (ID);

            if Event_Task_Map.Contains (ID) then
               declare
                  E  : Event_Task_Access := Event_Task_Map.Element (ID);
               begin
                  Free_Event_Task (E);
                  Event_Task_Map.Delete (ID);
               end;
            end if;
         end if;
      exception
         when others =>
            null;
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
            raise Connection_Error with
              "Connection ID" & ID'Img & " not found in connection map. " &
              "Connection most likely was previously closed.";
      end Connection_Socket;

      function Find_Connetion_ID (Socket : Socket_Type)
                               return Gnoga.Types.Connection_ID
      is
         use type Socket_Maps.Cursor;

         Cursor : Socket_Maps.Cursor := Socket_Map.First;
      begin
         while Cursor /= Socket_Maps.No_Element loop
            if Socket_Maps.Element (Cursor) = Socket then
               return Socket_Maps.Key (Cursor);
            else
               Socket_Maps.Next (Cursor);
            end if;
         end loop;

         return Gnoga.Types.No_Connection;
      end Find_Connetion_ID;

      procedure Delete_All_Connections is
         procedure Do_Delete (C : in Socket_Maps.Cursor);

         procedure Do_Delete (C : in Socket_Maps.Cursor) is
         begin
            Delete_Connection (Socket_Maps.Key (C));
         end Do_Delete;
      begin
         Socket_Map.Iterate (Do_Delete'Access);
      end Delete_All_Connections;
   end Connection_Manager;

   task body Event_Task_Type is
      Connection_Holder : aliased Connection_Holder_Type;
   begin
      Connection_Manager.Add_Connection_Holder
        (ID, Connection_Holder'Unchecked_Access);

      begin
         Execute_Script (ID, "gnoga['Connection_ID']=" & ID'Img);

         Execute_Script (ID, "TRUE=true");
         Execute_Script (ID, "FALSE=false");
         --  By setting the variable TRUE and FALSE it is possible to set
         --  a property or attribute with Boolean'Img which will result
         --  in TRUE or FALSE not the case sensitive true or false
         --  expected.

         On_Connect_Event (ID, Connection_Holder'Access);
      exception
         when Connection_Error =>
            --  Browser was closed by user
            Connection_Holder.Release;
         when E : others =>
            Connection_Holder.Release;

            Log ("Error on Connection ID=" & ID'Img);
            Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
            Log (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      end;

      Connection_Manager.Delete_Connection_Holder (ID);
   end Event_Task_Type;

   --------------
   -- Watchdog --
   --------------

   task body Watchdog is
      procedure Ping (C : in Socket_Maps.Cursor);

      procedure Ping (C : in Socket_Maps.Cursor) is
         ID : Gnoga.Types.Connection_ID := Socket_Maps.Key (C);
      begin
         Execute_Script (ID, "0");
      exception
         when others =>
            null;
      end Ping;
   begin
      accept Start;

      loop
         begin
            Socket_Map.Iterate (Ping'Access);
         end;

         select
            accept Stop;
            exit;
         or
            delay 60.0;
         end select;
      end loop;

   end Watchdog;

   ---------------------------
   -- Message Queue Manager --
   ---------------------------

   function "=" (Left, Right : Gnoga.Gui.Base.Pointer_To_Base_Class)
                 return Boolean;
   --  Properly identify equivelant objects

   function "=" (Left, Right : Gnoga.Gui.Base.Pointer_To_Base_Class)
                 return Boolean
   is
   begin
      return Left.Unique_ID = Right.Unique_ID;
   end "=";

   package Object_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Types.Unique_ID, Gnoga.Gui.Base.Pointer_To_Base_Class);

   protected Object_Manager is
      function Get_Object (Index : Integer)
                           return Gnoga.Gui.Base.Pointer_To_Base_Class;
      procedure Insert
        (ID     : in Gnoga.Types.Unique_ID;
         Object : in Gnoga.Gui.Base.Pointer_To_Base_Class);

      procedure Delete (ID : Gnoga.Types.Unique_ID);
   private
      Object_Map : Object_Maps.Map;
   end Object_Manager;

   protected body Object_Manager is
      function Get_Object (Index : Integer)
                           return Gnoga.Gui.Base.Pointer_To_Base_Class
      is
      begin
         return Object_Map.Element (Index);
      end Get_Object;

      procedure Insert
        (ID     : in Gnoga.Types.Unique_ID;
         Object : in Gnoga.Gui.Base.Pointer_To_Base_Class)
      is
      begin
         Object_Map.Insert (Key      => ID,
                            New_Item => Object);
      end Insert;

      procedure Delete (ID : Gnoga.Types.Unique_ID) is
      begin
         Object_Map.Delete (ID);
      end Delete;
   end Object_Manager;

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

   overriding procedure On_Open
     (Web_Socket : in out Socket_Type; Message : String)
   is
      use type Gnoga.Types.Connection_ID;

      ID     : Gnoga.Types.Connection_ID := Gnoga.Types.No_Connection;
      Old_ID : String := AWS.Status.Parameter (Web_Socket.Request, "Old_ID");
   begin
      if On_Connect_Event /= null then
         Connection_Manager.Add_Connection (Socket => Web_Socket,
                                            New_ID => ID);
         if Old_ID /= "" then
            if Verbose_Output then
               Gnoga.Log ("Swaping connections " & ID'Img & " => " & Old_ID);
            end if;

            Connection_Manager.Swap_Connection
              (ID, Gnoga.Types.Connection_ID'Value (Old_ID));

            Connection_Manager.Delete_Connection (ID);
         else
            Connection_Manager.Start_Connection (ID);

            if Verbose_Output then
               Gnoga.Log ("New connection - ID" & ID'Img);
            end if;
         end if;
      else
         Web_Socket.Close ("No connection event set");
      end if;
   end On_Open;

   --------------
   -- On_Close --
   --------------

   overriding
   procedure On_Close (Web_Socket : in out Socket_Type; Message : in String)
   is
      ID : Gnoga.Types.Connection_ID :=
             Connection_Manager.Find_Connetion_ID (Web_Socket);
   begin
      if ID /= Gnoga.Types.No_Connection then
         Connection_Manager.Delete_Connection (ID);

         if Verbose_Output then
            Gnoga.Log ("Connection disconnected - ID" & ID'Img &
                         " with message : " & Message);
         end if;
      end if;
   end On_Close;

   --------------
   -- On_Error --
   --------------

   overriding
   procedure On_Error (Web_Socket : in out Socket_Type; Message : in String)
   is
      ID : Gnoga.Types.Connection_ID :=
             Connection_Manager.Find_Connetion_ID (Web_Socket);
   begin
      Gnoga.Log ("Connection error ID" & ID'Img &
                   " with message : " & Message);
      --  Need to add a check for deleting connections that have not
      --  reconnected
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
         --  Semiphore does not reset itself to a blocking state.
         --  This ensures that if Released before Hold that Hold
         --  will not block and connection will be released.
      end Hold;

      procedure Release (Result : in String) is
      begin
         Connected := False;
         Script_Result := Ada.Strings.Unbounded.To_Unbounded_String (Result);
      end Release;

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

   task type Dispatch_Task_Type
     (Object : Gnoga.Gui.Base.Pointer_To_Base_Class)
   is
      entry Start (Event : in String;
                   Data  : in String;
                   ID    : in Gnoga.Types.Unique_ID);
   end Dispatch_Task_Type;

   type Dispatch_Task_Access is access all Dispatch_Task_Type;

   procedure Free_Dispatch_Task is
        new Ada.Unchecked_Deallocation (Dispatch_Task_Type,
                                        Dispatch_Task_Access);

   package Dispatch_Task_Maps is new Ada.Containers.Ordered_Maps
     (Gnoga.Types.Unique_ID, Dispatch_Task_Access);

   protected Dispatch_Task_Objects is
      procedure Add_Dispatch_Task (ID            : in Gnoga.Types.Unique_ID;
                                   Dispatch_Task : in Dispatch_Task_Access);

      function Object (ID : Gnoga.Types.Unique_ID) return Dispatch_Task_Access;

      procedure Delete_Dispatch_Task (ID : in Gnoga.Types.Unique_ID);
   private
      Dispatch_Task_Map : Dispatch_Task_Maps.Map;
   end Dispatch_Task_Objects;

   protected body Dispatch_Task_Objects is
      procedure Add_Dispatch_Task (ID            : in Gnoga.Types.Unique_ID;
                                   Dispatch_Task : in Dispatch_Task_Access)
      is
      begin
         Dispatch_Task_Map.Insert (ID, Dispatch_Task);
      end Add_Dispatch_Task;

      function Object (ID : Gnoga.Types.Unique_ID) return Dispatch_Task_Access
      is
      begin
         return Dispatch_Task_Map.Element (ID);
      end Object;

      procedure Delete_Dispatch_Task (ID : in Gnoga.Types.Unique_ID)
      is
         T : Dispatch_Task_Access := Dispatch_Task_Map.Element (ID);
      begin
         Free_Dispatch_Task (T);
         --  http://adacore.com/developers/development-log/NF-65-H911-007-gnat
         --  This will cause T to free upon task termination.
         Dispatch_Task_Map.Delete (ID);
      end Delete_Dispatch_Task;
   end Dispatch_Task_Objects;

   task body Dispatch_Task_Type is
      E : Ada.Strings.Unbounded.Unbounded_String;
      D : Ada.Strings.Unbounded.Unbounded_String;
      I : Gnoga.Types.Unique_ID;
   begin
      accept Start (Event : in String;
                    Data  : in String;
                    ID    : in Gnoga.Types.Unique_ID)
      do
         E := Ada.Strings.Unbounded.To_Unbounded_String (Event);
         D := Ada.Strings.Unbounded.To_Unbounded_String (Data);
         I := ID;
      end Start;

      declare
         Continue : Boolean;

         Event    : constant String  := Ada.Strings.Unbounded.To_String (E);
         Data     : constant String  := Ada.Strings.Unbounded.To_String (D);
      begin
         Object.Fire_On_Message (Event, Data, Continue);

         if Continue then
            Object.On_Message (Event, Data);
         end if;
      end;

      Dispatch_Task_Objects.Delete_Dispatch_Task (I);
   exception
      when E : others =>
         Log ("Dispatch Error");
         Log (Ada.Exceptions.Exception_Name (E) & " - " &
                Ada.Exceptions.Exception_Message (E));
         Log (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end Dispatch_Task_Type;

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

            Object : Gnoga.Gui.Base.Pointer_To_Base_Class :=
                       Object_Manager.Get_Object (Integer'Value (UID));

            New_ID : Gnoga.Types.Unique_ID;
         begin
            New_Unique_ID (New_ID);

            Dispatch_Task_Objects.Add_Dispatch_Task
              (New_ID, new Dispatch_Task_Type (Object));
            Dispatch_Task_Objects.Object (New_ID).Start
              (Event, Event_Data, New_ID);
         end;
      end if;
   end On_Message;

   --------------------
   -- Execute_Script --
   --------------------

   procedure Execute_Script (ID     : in Gnoga.Types.Connection_ID;
                             Script : in String)
   is
   begin
      declare
         Socket  : Socket_Type := Connection_Manager.Connection_Socket (ID);
      begin
         Socket.Send (Script);
      end;
   exception
      when AWS.Net.Socket_Error =>
         raise Connection_Error with
           "Socket Error during execute of : " & Script;
   end Execute_Script;

   function Execute_Script (ID     : in Gnoga.Types.Connection_ID;
                            Script : in String)
                            return String
   is
      function Try_Execute return String;

      function Try_Execute return String is
         Script_Holder : aliased Script_Holder_Type;
      begin
         declare
            Script_ID : Gnoga.Types.Unique_ID;
            Socket    : Socket_Type :=
                          Connection_Manager.Connection_Socket (ID);
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

               select
                  delay 3.0; --  Timeout for browser to return answer

                  Script_Manager.Delete_Script_Holder (Script_ID);

                  raise Script_Error with
                    "Timeout error, no browser response for: " & Message;
               then abort
                  Script_Holder.Hold;
               end select;
            end;

            Script_Manager.Delete_Script_Holder (Script_ID);

            return Script_Holder.Result;
         end;
      exception
         when AWS.Net.Socket_Error =>
            raise Connection_Error with
              "Socket Error, Browser dropped connection";
      end Try_Execute;
   begin
      begin
         return Try_Execute;
      exception
         when others =>
            begin
               delay 2.0;
               return Try_Execute;
            end;
      end;
   end Execute_Script;

   ---------------------
   -- Connection_Data --
   ---------------------

   procedure Connection_Data
     (ID   : in     Gnoga.Types.Connection_ID;
      Data : access Gnoga.Types.Connection_Data_Type'Class)
   is
   begin
      Connection_Manager.Add_Connection_Data
        (ID,
         Gnoga.Types.Pointer_to_Connection_Data_Class (Data));
   end Connection_Data;

   function Connection_Data
     (ID : in Gnoga.Types.Connection_ID)
      return Gnoga.Types.Pointer_to_Connection_Data_Class
   is
   begin
      return Connection_Manager.Connection_Data (ID);
   end Connection_Data;

   ------------------------
   -- On_Connect_Handler --
   ------------------------

   procedure On_Connect_Handler (Event : in Connect_Event) is
   begin
      On_Connect_Event := Event;
   end On_Connect_Handler;

   ---------------------
   -- On_Post_Handler --
   ---------------------

   procedure On_Post_Handler (Event : in Post_Event) is
   begin
      On_Post_Event := Event;
   end On_Post_Handler;

   ----------------------
   -- Search_Parameter --
   ----------------------

   function Form_Parameter (ID   : Gnoga.Types.Connection_ID;
                              Name : String)
                              return String
   is
   begin
      return Execute_Script (ID, "params['" & Name & "'];");
   end Form_Parameter;

   -----------
   -- Valid --
   -----------

   function Valid (ID : Gnoga.Types.Connection_ID) return Boolean is
   begin
      return Connection_Manager.Valid (ID);
   end Valid;

   -----------
   -- Close --
   -----------

   procedure Close (ID : Gnoga.Types.Connection_ID) is
   begin
      Execute_Script (ID, "ws.close();");
   end Close;

   -------------------
   -- HTML_On_Close --
   -------------------

   procedure HTML_On_Close (ID   : in Gnoga.Types.Connection_ID;
                            HTML : in String)
   is
   begin
      Execute_Script (ID     => ID,
                      Script => "gnoga['html_on_close']=""" &
                        Escape_Quotes (HTML) & """;");
   end HTML_On_Close;

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
      end Next_ID;
   end ID_Machine_Type;

   ID_Machine : ID_Machine_Type;

   -------------------
   -- New_Unique_ID --
   -------------------

   procedure New_Unique_ID (New_ID : out Gnoga.Types.Unique_ID) is
   begin
      ID_Machine.Next_ID (New_ID);
   end New_Unique_ID;

   --------------
   -- New_GID --
   --------------

   function New_GID return String is
      New_ID : Gnoga.Types.Unique_ID;
   begin
      New_Unique_ID (New_ID);

      return "g" & Left_Trim (New_ID'Img);
   end New_GID;

   --------------------------
   -- Add_To_Message_Queue --
   --------------------------

   procedure Add_To_Message_Queue
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
   begin
      Object_Manager.Insert (Object.Unique_ID, Object'Unchecked_Access);
   end Add_To_Message_Queue;

   -------------------------------
   -- Delete_From_Message_Queue --
   -------------------------------

   procedure Delete_From_Message_Queue
     (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      Object_Manager.Delete (Object.Unique_ID);
   end Delete_From_Message_Queue;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Exit_Application_Requested := True;
      Watchdog.Stop;
      Connection_Manager.Delete_All_Connections;
      AWS.Server.Shutdown (Web_Server);
   end Stop;

end Gnoga.Server.Connection;
