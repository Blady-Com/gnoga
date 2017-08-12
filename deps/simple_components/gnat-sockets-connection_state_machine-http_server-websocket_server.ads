--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     HTTP_Server.WebSocket_Server                Spring, 2017       --
--  Interface                                                         --
--                                Last revision :  21:11 16 Apr 2017  --
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

with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;

package GNAT.Sockets.Connection_State_Machine.HTTP_Server.
        WebSocket_Server is
--
-- HTTP_WebSocket_Client -- A server  handling  connection  objects that
--                          use WebSocket communication
--
--    Listener       - The connection object
--    Request_Length - The maximum length of one request line
--    Input_Size     - The input buffer size
--    Output_Size    - The output buffer size
--    Factory        - The WebSocket connection objects factory
--    Buffer_Size    - The WebSocket message size
--
-- The implementations  of  connection  objects  can be  used  both  for
-- for normal sockets and WebSockets.  In the later  case a  HTTP client
-- derived  from this type  handles  the connection  object istead of an
-- object derived from Connections_Server.
--
   type HTTP_WebSocket_Client
        (  Listener       : access Connections_Server'Class;
           Request_Length : Positive;
           Input_Size     : Buffer_Length;
           Output_Size    : Buffer_Length;
           Factory        : access Connections_Factory'Class;
           Buffer_Size    : Buffer_Length
        )  is new HTTP_Client with private;
--
-- Get_WebSocket_Client -- Get connection object running over WebSocket
--
--    Client - The HTTP connection object
--
-- Returns :
--
--    A handle to the aWebSocket connection object
--
   function Get_WebSocket_Client
            (  Client : HTTP_WebSocket_Client
            )  return Handle;
--
-- Get_Error_Code -- Get error code returned when connection refused
--
--    Client - The HTTP connection object
--
-- When the factory of WebSocket  connection objects refuses connection,
-- the result of this  function is used to  report  the error code.  The
-- default implementation returns 400.
--
-- Returns :
--
--    The error code
--
   function Get_Error_Code
            (  Client : HTTP_WebSocket_Client
            )  return Positive;
--
-- Get_Error_Reason -- Get error reason returned when connection refused
--
--    Client - The HTTP connection object
--
-- When the factory of WebSocket  connection objects refuses connection,
-- the result of this  function is used to report  the reason text.  The
-- default implementation returns "Bad request".
--
-- Returns :
--
--    The error code
--
   function Get_Error_Reason
            (  Client : HTTP_WebSocket_Client
            )  return String;
--
-- Get_Protocols -- Get the protocols string
--
--    Client - The HTTP connection object
--
-- When accepted  the client  receives a list of supported  protocols as
-- returned  by this  function.  When the result  is  empty the value of
-- Sec-WebSocket-Protocol  is used instead.  The default  implementation
-- returns empty string.

-- Returns :
--
--    The error code
--
   function Get_Protocols
            (  Client : HTTP_WebSocket_Client
            )  return String;
--
-- Overridden operations
--
   procedure WebSocket_Finalize
             (  Client : in out HTTP_WebSocket_Client
             );
   function WebSocket_Open
            (  Client : access HTTP_WebSocket_Client
            )  return WebSocket_Accept;
   procedure WebSocket_Initialize
             (  Client : in out HTTP_WebSocket_Client
             );
   procedure WebSocket_Received
             (  Client  : in out HTTP_WebSocket_Client;
                Message : Stream_Element_Array
             );
   procedure WebSocket_Received
             (  Client  : in out HTTP_WebSocket_Client;
                Message : String
             );
   procedure WebSocket_Received_Part
             (  Client  : in out HTTP_WebSocket_Client;
                Message : Stream_Element_Array
             );
   procedure WebSocket_Received_Part
             (  Client  : in out HTTP_WebSocket_Client;
                Message : String
             );
private
   type WebSockets_Server
        (  HTTP_Client : access HTTP_WebSocket_Client'Class;
           Factory     : access Connections_Factory'Class
        )  is new Connections_Server
                  (  Factory => Factory,
                     Port    => 0
                  )  with null record;

   procedure Initialize (Listener : in out WebSockets_Server);
   procedure Shutdown
             (  Listener : in out WebSockets_Server;
                Client   : in out Connection'Class
             );
   procedure Send_Socket
             (  Listener : in out WebSockets_Server;
                Client   : in out Connection'Class;
                Data     : Stream_Element_Array;
                Last     : out Stream_Element_Offset
             );
   procedure Unblock_Send
             (  Listener : in out WebSockets_Server;
                Client   : in out Connection'Class
             );
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : WebSockets_Server
             );
   for WebSockets_Server'Write use Write;

   type HTTP_WebSocket_Client
        (  Listener       : access Connections_Server'Class;
           Request_Length : Positive;
           Input_Size     : Buffer_Length;
           Output_Size    : Buffer_Length;
           Factory        : access Connections_Factory'Class;
           Buffer_Size    : Buffer_Length
        )  is new HTTP_Client
           (  Listener       => Listener,
              Request_Length => Request_Length,
              Input_Size     => Input_Size,
              Output_Size    => Output_Size
           )  with
   record
      Textual            : Boolean := False;
      WebSocket_Client   : Handle;
      WebSocket_Listener : aliased
         WebSockets_Server
         (  HTTP_Client => HTTP_WebSocket_Client'Unchecked_Access,
            Factory     => Factory
         );
   end record;

end GNAT.Sockets.Connection_State_Machine.HTTP_Server.WebSocket_Server;
