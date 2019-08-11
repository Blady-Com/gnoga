--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.Blocking                Luebeck            --
--  Interface                                      Winter, 2018       --
--                                                                    --
--                                Last revision :  13:37 23 Jun 2019  --
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

with Generic_FIFO;
with GNAT.Sockets.Server.Handles;

package GNAT.Sockets.Server.Blocking is
--
-- Blocking_Server -- Blocking server
--
--     Factory       - The factory object
--     Input_Stream  - The stream to read incoming data
--     Output_Stream - The stream to write outgoing data
--     Input_Size    - Read buffer size
--
-- A blocking server handles only one permanent connection,  e.g. over a
-- serial port.  It is provided in order  to use connection objects with
-- transports different from TCP sockets.
--
   type Blocking_Server
        (  Factory       : access Connections_Factory'Class;
           Input_Stream  : access Root_Stream_Type'Class;
           Output_Stream : access Root_Stream_Type'Class;
           Input_Size    : Positive
        )  is new Connections_Server with private;
--
-- Cancel_IO -- Abort pending I/O
--
--    Listener - The server object
--    Client   - The client connection object
--
-- This procedure  is called to abort  any pending I/O  in the course of
-- finalization,  typically in order to break blocking I/O.  The default
-- implementation does nothing.
--
   procedure Cancel_IO
             (  Listener : in out Blocking_Server;
                Client   : in out Connection'Class
             );
--
-- Connect -- Connect to a server
--
--    Listener       - The server object
--    Client         - The client connection object
--    Host           - The host name or IP address
--    Port           - The port number
--    Max_Connect_No - Maximal number of connection attempts
--    Overlapped     - The overlapped read size (full-duplex by default)
--
-- This procedure  is used  to create  the client for the server.  There
-- can be only one. Note that the connection object is passed by pointer
-- an maintained by Listener.
--
-- Exceptions :
--
--    Host_Error - Invalid host name (any not empty string)
--    Use_Error  - Client is already in use
--
   procedure Connect
             (  Listener       : in out Blocking_Server;
                Client         : Connection_Ptr;
                Host           : String    := "";
                Port           : Port_Type := No_Port;
                Max_Connect_No : Positive  := Positive'Last;
                Overlapped     : Stream_Element_Count :=
                                 Stream_Element_Count'Last
             );
--
-- Finalize -- Destruction
--
--    Listener - The server object
--
-- The derived type must call this procedure when it overrides this.
--
   procedure Finalize (Listener : in out Blocking_Server);
--
-- Get_Clients_Count -- Number of clients connected
--
--    Listener - The server object
--
-- Returns :
--
--    Number of connected clients
--
   function Get_Clients_Count (Listener : Blocking_Server)
      return Natural;
--
-- Get_Peer -- The connection object
--
--    Listener - The server object
--
-- Returns :
--
--    A handle to the connection object
--
   function Get_Peer (Listener : Blocking_Server)
      return GNAT.Sockets.Server.Handles.Handle;
--
-- Get_Server_Address -- Get the server socket address
--
--    Listener - The server object
--
-- Returns :
--
--    No address
--
   function Get_Server_Address
            (  Listener : Blocking_Server
            )  return Sock_Addr_Type;
--
-- Initialize -- Construction
--
--    Listener - The server object
--
-- The derived  type must  call this  procedure from  its implementation
-- when it replaces it.
--
   procedure Initialize (Listener : in out Blocking_Server);

private
   task type Writer
             (  Listener : access Blocking_Server'Class;
                Client   : access Connection'Class
             );
   type Writer_Ptr is access Writer;

   package Stream_FIFO is new Generic_FIFO (Stream_Element);
   use Stream_FIFO;

   protected type IO_Buffer_Event is
      function Is_Exiting return Boolean;
      procedure Quit;
      procedure Set_Empty (Value : Boolean);
      procedure Set_Full (Value : Boolean);
      entry Wait_Not_Empty;
      entry Wait_Not_Full;
   private
      Exiting : Boolean := False;
      Empty   : Boolean := True;
      Full    : Boolean := False;
   end IO_Buffer_Event;

   type Blocking_Server
        (  Factory       : access Connections_Factory'Class;
           Input_Stream  : access Root_Stream_Type'Class;
           Output_Stream : access Root_Stream_Type'Class;
           Input_Size    : Positive
        )  is new Connections_Server
                  (  Factory => Factory,
                     Port    => No_Port
                  )  with
   record
      Peer   : GNAT.Sockets.Server.Handles.Handle;
      Writer : Writer_Ptr;
      Event  : IO_Buffer_Event;
      Input  : Stream_FIFO.FIFO (Input_Size);
   end record;

   procedure Receive_Socket
             (  Listener : in out Blocking_Server;
                Client   : in out Connection'Class;
                Data     : in out Stream_Element_Array;
                Last     : out Stream_Element_Offset
             );
   procedure Request_Disconnect
             (  Listener  : in out Blocking_Server;
                Client    : in out Connection'Class;
                Reconnect : Boolean
             );
   procedure Send_Socket
             (  Listener : in out Blocking_Server;
                Client   : in out Connection'Class;
                Data     : Stream_Element_Array;
                Last     : out Stream_Element_Offset
             );

end GNAT.Sockets.Server.Blocking;
