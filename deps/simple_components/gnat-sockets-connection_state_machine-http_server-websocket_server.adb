--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     HTTP_Server.WebSocket_Server                Spring, 2017       --
--  Implementation                                                    --
--                                Last revision :  14:07 11 Nov 2019  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body GNAT.Sockets.Connection_State_Machine.HTTP_Server.
             WebSocket_Server is

   function Get_WebSocket_Client
            (  Client : HTTP_WebSocket_Client
            )  return Handle is
   begin
      return Client.WebSocket_Client;
   end Get_WebSocket_Client;

   function Get_Error_Code
            (  Client : HTTP_WebSocket_Client
            )  return Positive is
   begin
      return 400;
   end Get_Error_Code;

   function Get_Error_Reason
            (  Client : HTTP_WebSocket_Client
            )  return String is
   begin
      return "Bad request";
   end Get_Error_Reason;

   function Get_Protocols
            (  Client : HTTP_WebSocket_Client
            )  return String is
   begin
      return "";
   end Get_Protocols;

   procedure Initialize (Listener : in out WebSockets_Server) is
   begin
      null; -- Not creating selectors, not starting tasks
   end Initialize;

   procedure Send_Socket
             (  Listener : in out WebSockets_Server;
                Client   : in out Connection'Class;
                Data     : Stream_Element_Array;
                Last     : out Stream_Element_Offset
             )  is
   begin
      if Listener.HTTP_Client.Textual then
         WebSocket_Send
         (  Listener.HTTP_Client.all,
            To_String (Data)
         );
      else
         WebSocket_Send
         (  Listener.HTTP_Client.all,
            Data
         );
      end if;
      Last := Data'Last;
      begin
         Sent (Client);
      exception
         when Connection_Error =>
            WebSocket_Close (Listener.HTTP_Client.all);
            Invalidate (Listener.HTTP_Client.WebSocket_Client);
         when Error : others =>
            Trace_Error
            (  Listener.Factory.all,
               "Processing WebSockets sent notification",
               Error
            );
            WebSocket_Close (Listener.HTTP_Client.all);
            Invalidate (Listener.HTTP_Client.WebSocket_Client);
      end;
   end Send_Socket;

   procedure Shutdown
             (  Listener : in out WebSockets_Server;
                Client   : in out Connection'Class
             )  is
   begin
      Shutdown (Connections_Server (Listener), Client);
      WebSocket_Close (Listener.HTTP_Client.all);
   end Shutdown;

   procedure Unblock_Send
             (  Listener : in out WebSockets_Server;
                Client   : in out Connection'Class
             )  is
      Block : Boolean;
   begin
      Write (Client, Listener.Factory.all, Block); -- Pump output
      Unblock_Send (Connections_Server (Listener), Client);
   end Unblock_Send;

   procedure WebSocket_Finalize
             (  Client : in out HTTP_WebSocket_Client
             )  is
      This : constant Connection_Ptr := Ptr (Client.WebSocket_Client);
   begin
      if This = null then
         return;
      end if;
      if Get_Session_State (This.all) in Session_Connected..Session_Busy
      then
         begin -- Disconnected
            Disconnected (Client.WebSocket_Listener, This.all);
         exception
            when Connection_Error =>
               null;
            when Error : others =>
               Trace_Error
               (  Client.Factory.all,
                  "Disconnected (WebSocket server)",
                  Error
               );
         end;
         begin -- Connected
            Disconnected (This.all);
         exception
            when Connection_Error =>
               null;
            when Error : others =>
               Trace_Error
               (  Client.Factory.all,
                  "Disconnected (WebSocket client)",
                  Error
               );
         end;
      end if;
      begin
         Downed (Client.WebSocket_Listener, This.all);
      exception
         when Error : others =>
            Trace_Error
            (  Client.Factory.all,
               "Downed (WebSockets server)",
               Error
            );
      end;
      begin
         Downed (This.all);
      exception
         when Error : others =>
            Trace_Error
            (  Client.Factory.all,
               "Downed (WebSockets client)",
               Error
            );
      end;
      Invalidate (Client.WebSocket_Client);
   exception
      when Error : others =>
         Trace_Error
         (  Client.Factory.all,
            "Stopping (WebSockets)",
            Error
         );
   end WebSocket_Finalize;

   procedure WebSocket_Initialize
             (  Client : in out HTTP_WebSocket_Client
             )  is
      This : Connection'Class renames Ptr (Client.WebSocket_Client).all;
   begin
      begin
         Connected (This);
         Connected (Client.WebSocket_Listener, This);
      exception
         when Error : others =>
            Trace_Error
            (  Client.Factory.all,
               "Connected (WebSockets)",
               Error
            );
            raise;
      end;
      WebSocket_Initialize (HTTP_Client (Client));
   end WebSocket_Initialize;

   function WebSocket_Open
            (  Client : access HTTP_WebSocket_Client
            )  return WebSocket_Accept is
      Parent : HTTP_WebSocket_Client'Class renames
               HTTP_WebSocket_Client'Class (Client.all);
      WebSocket_Client : constant Connection_Ptr :=
               Create
               (  Client.Factory,
                  Client.WebSocket_Listener'Unchecked_Access,
                  Get_Client_Address (Parent)
               );
   begin
      if WebSocket_Client = null then
         declare
            Reason : constant String := Get_Error_Reason (Parent);
         begin
            return
            (  Accepted => False,
               Length   => Reason'Length,
               Code     => Get_Error_Code (Parent),
               Reason   => Reason
            );
         end;
      else
         Set (Client.WebSocket_Client, WebSocket_Client);
         declare
            Protocols : constant String := Get_Protocols (Parent);
         begin
            Set_Client_Data
            (  WebSocket_Client.all,
               Get_Client_Address (Client.all),
               Client.WebSocket_Listener'Unchecked_Access
            );
            return
            (  Accepted  => True,
               Length    => Protocols'Length,
               Size      => Client.Buffer_Size,
               Duplex    => False,
               Chunked   => True,
               Protocols => Protocols
            );
         end;
      end if;
   end WebSocket_Open;

   procedure WebSocket_Received
             (  Client  : in out HTTP_WebSocket_Client;
                Message : Stream_Element_Array
             )  is
      Receiver : Connection'Class renames
                 Ptr (Client.WebSocket_Client).all;
      First    : Stream_Element_Offset := Message'First;
      Last     : Stream_Element_Offset;
      Block    : Boolean;
   begin
      Client.Textual := False;
      while First <= Message'Last loop
         Last := Message'Last + 1;
         Received (Receiver, Message (First..Message'Last), Last);
         if Last < First then -- Nothing accepted
            Raise_Exception
            (  Data_Error'Identity,
               "Nothing accepted"
            );
         end if;
         First := Last + 1;
      end loop;
      Write (Receiver, Client.Listener.Factory.all, Block);
   end WebSocket_Received;

   procedure WebSocket_Received
             (  Client  : in out HTTP_WebSocket_Client;
                Message : String
             )  is
      Receiver : Connection'Class renames
                 Ptr (Client.WebSocket_Client).all;
      Data     : constant Stream_Element_Array := From_String (Message);
      First    : Stream_Element_Offset := Data'First;
      Last     : Stream_Element_Offset;
      Block    : Boolean;
   begin
      Client.Textual := True;
      while First <= Data'Last loop
         Last := Data'Last + 1;
         Received (Receiver, Data (First..Data'Last), Last);
         if Last <= First then -- Nothing accepted
            Raise_Exception
            (  Data_Error'Identity,
               "Nothing accepted"
            );
         end if;
         First := Last;
      end loop;
      Write (Receiver, Client.Listener.Factory.all, Block);
   end WebSocket_Received;

   procedure WebSocket_Received_Part
             (  Client  : in out HTTP_WebSocket_Client;
                Message : Stream_Element_Array
             )  is
   begin
      WebSocket_Received
      (  HTTP_WebSocket_Client'Class (Client),
         Message
      );
   end WebSocket_Received_Part;

   procedure WebSocket_Received_Part
             (  Client  : in out HTTP_WebSocket_Client;
                Message : String
             )  is
   begin
      WebSocket_Received
      (  HTTP_WebSocket_Client'Class (Client),
         Message
      );
   end WebSocket_Received_Part;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : WebSockets_Server
             )  is
   begin
      null;
   end Write;

end GNAT.Sockets.Connection_State_Machine.HTTP_Server.WebSocket_Server;
