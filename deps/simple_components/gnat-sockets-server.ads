--                                                                    --
--  package GNAT.Sockets.Server     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  13:01 14 Dec 2014  --
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

with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Streams;     use Ada.Streams;

with Ada.Finalization;
with Ada.Text_IO;
with Object.Handle.Generic_Unbounded_Array;
with Strings_Edit.Integer_Edit;

package GNAT.Sockets.Server is
   Connection_Error : exception;

   subtype Buffer_Length is
      Stream_Element_Offset range 1..Stream_Element_Offset'Last;
--
-- Connection -- To derive custom object handling a client connection
--
--    Input_Size  - Input buffer size
--    Output_Size - Output buffer size
--
-- The input buffer determines  maximal  number  of octets received from
-- the client  in one piece.  Usually  it is around  the average  packet
-- size, though one  must not expect that packets will be delivered as a
-- whole, per single call to Received.  The output buffer is the maximal
-- number of octets  which can be sent in one piece  without  postponing
-- sending.
--
   type Connection
        (  Input_Size  : Buffer_Length;
           Output_Size : Buffer_Length
        )  is abstract new Object.Entity with private;
   type Connection_Ptr is access all Connection'Class;
--
-- Connections_Factory -- Factory of client connection objects
--
   type Connections_Factory is abstract
      new Ada.Finalization.Limited_Controlled with private;
--
-- Server -- To derive a custom multiple connections server from
--
--    Port - The port to listen
--
   type Connections_Server
        (  Factory : access Connections_Factory'Class;
           Port    : Port_Type
        )  is new Ada.Finalization.Limited_Controlled with private;
--
-- Available_To_Process -- Stream elements available to process
--
--    Client - The client connection object
--
-- Returns :
--
--    Count of received but not yet processed stream elements
--
   function Available_To_Process (Client : Connection)
      return Stream_Element_Count;
--
-- Available_To_Send -- Stream elements available to send
--
--    Client - The client connection object
--
-- The result is the maximum  number of stream  elements  which  Send is
-- guaranteed to accept.  Larger number may cause Send returning Pointer
-- less or equal to Data'Last.
--
-- Returns :
--
--    Stream elements count
--
   function Available_To_Send (Client : Connection)
      return Stream_Element_Count;
--
-- Connected -- The first operation called on connection object
--
--    Client - The client connection object
--
-- The default  implementation  does nothing.  Typically  the server may
-- set some socket options here.
--
   procedure Connected (Client : in out Connection);
--
-- Create -- Client connection object
--
--    Factory  - The factory object
--    Listener - The server object
--    From     - The client address
--
-- This function is  called  to accept  connection  from  a client.  The
-- implementation  may refuse connection  in which case it returns null.
-- When  connection  is  accepted  the  implementation  allocates  a new
-- connection  object and returns a pointer to it.  Note that  it is the
-- server object responsibility to free the object.  The  implementation
-- may deploy client  filtering based on the address From and/or  number
-- of connections.
--
-- Returns :
--
--    Pointer to the connection object or null
--
   function Create
            (  Factory  : access Connections_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is abstract;
--
-- Disconnected -- Server notification about client disconnection
--
--    Listener - The server object
--    Client   - The client object being deleted
--
-- The server may do some bookkeeping here.
--
   procedure Disconnected
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             );
--
-- Finalize -- Destruction
--
--    Listener - The server object
--
-- The derived type must call this procedure when it overrides this.
--
   procedure Finalize (Listener : in out Connections_Server);
--
-- Finalize -- Destruction
--
--    Client - The connection object
--
-- The derived type must call this procedure when it overrides this.
--
   procedure Finalize (Client : in out Connection);
--
-- From_String -- Conversion from string
--
--    Data - To convert
--
-- Returns :
--
--    The result
--
   function From_String (Data : String) return Stream_Element_Array;
--
-- Get_Clients_Count -- Number of clients connected
--
--    Listener - The server object
--
-- Returns :
--
--    Number of connected clients
--
   function Get_Clients_Count (Listener : Connections_Server)
      return Natural;
--
-- Get_Client_Address -- Get address of the client
--
--    Client - The client connection object
--
-- Returns :
--
--    The client's address
--
   function Get_Client_Address (Client : Connection)
      return Sock_Addr_Type;
--
-- Get_IO_Timeout -- The I/O timeout used by connections server
--
--    Factory - The factory object
--
-- When the connections server  waits for a socket to become readable or
-- writable  this value specifies waiting timeout.  Upon the timeout the
-- server re-enters the waiting.  The default value is 20ms.  It  can be
-- changed by overriding this function.
--
-- Returns :
--
--    The timeout when waiting for sockets events
--
   function Get_IO_Timeout (Factory : Connections_Factory)
      return Duration;
--
-- Get_Occurrence -- Get save client error
--
--    Client - The client connection object
--    Source - Saved exception occurence
--
   procedure Get_Occurrence
             (  Client : Connection;
                Source : out Exception_Occurrence
             );
--
-- Get_Overlapped_Size -- Get read policy
--
--    Client - The client connection object
--
-- See Set_Overlapped_Size.
--
-- Returns :
--
--    Maximum number of elements queued to send before blocking receive
--
   function Get_Overlapped_Size (Client : Connection)
      return Stream_Element_Count;
--
-- Get_Server_Address -- Get the server socket address
--
--    Listener - The server object
--
-- This function is called  before  the  server  starts  listening.  The
-- result  is the address to listen.  The default implementation returns
-- an addressof  the  INET  family with  any  address and the port taken
-- from the argument's port discriminant.
--
-- Returns :
--
--    The address to listen
--
   function Get_Server_Address
            (  Listener : Connections_Server
            )  return Sock_Addr_Type;
--
-- Get_Socket -- Get the socket
--
--    Client - The client connection object
--
-- Returns :
--
--    The socket used
--
   function Get_Socket (Client : Connection) return Socket_Type;
--
-- Has_Data -- Check if there is input data ready to process
--
--    Client - The client connection object
--
-- Returns :
--
--    True if the are data to process
--
   function Has_Data (Client : Connection) return Boolean;
--
-- Image -- Printable representation of stream array
--
--    Data - To convert
--
-- Returns :
--
--    Printable representation of
--
   function Image (Data : Stream_Element_Array) return String;
--
-- Initialize -- Construction
--
--    Listener - The server object
--
-- The derived  type must  call this  procedure from  its implementation
-- when it replaces it.
--
   procedure Initialize (Listener : in out Connections_Server);
--
-- Keep_On_Sending -- Delay stopping sending
--
--    Client - The client connection object
--
-- This procedure  is called  to hint  the connections  server  that  it
-- should  not stop polling the socket for being writable,  because some
-- content to send is about to come.
--
   procedure Keep_On_Sending (Client : in out Connection);
--
-- Process_Packet -- Packet processing
--
--    Client - The connection client
--
-- This procedure  is  called  when all fields  of Client, e.g. with the
-- types derived from Data_Item have  been  received  from  the  client.
-- The default implementation does nothing.
--
   procedure Process_Packet (Client : in out Connection);
--
-- Queued_To_Send -- Stream elements queued to send
--
--    Client - The client connection object
--
-- Returns :
--
--    Stream elements count
--
   function Queued_To_Send (Client : Connection)
      return Stream_Element_Count;
--
-- Received -- Data received notification
--
--    Client  - The client connection object
--    Data    - The data
--    Pointer - First unprocessed element
--
-- This  procedure  is called  when  a portion  of data is read from the
-- socket. The parameter Pointer is Data'Last + 1.  It can be changed to
-- indicate the  first  unprocessed  element.  Data (Pointer..Data'Last)
-- stay in the buffer until a next call to Received.
--
-- Exceptions :
--
--    Connection_Error - Propagated to close connection
--
   procedure Received
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is abstract;
--
-- Receive_Error -- Data receive error
--
--    Client     - The client connection object
--    Occurrence - The socket error
--
-- This  procedure  is called  upon  socket  receive  error  before  the
-- connection is dropped. The default implementation does nothing.
--
   procedure Receive_Error
             (  Client     : in out Connection;
                Occurrence : Exception_Occurrence
             );
--
-- Save_Occurrence -- Save client error occurence
--
--    Client - The client connection object
--    Source - The exception occurence to save
--
   procedure Save_Occurrence
             (  Client : in out Connection;
                Source : Exception_Occurrence
             );
--
-- Send -- Data to the client
--
--    Client  - The client connection object
--    Data    - The data to send
--    Pointer - The first element to send
--
-- This  procedure does not block.  When no send is pending  it sends as
-- much data the socket accepts without blocking. The rest is queued for
-- later which is also the case when some data  are pending.  The number
-- of elements  available  for queuing is returned by Available_To_Send.
-- The elements  which cannot  be queued  are Data (Pointer..Data'Last).
-- When this happens they should be kept until Send is called and passed
-- to Send again from there.
--
-- Exceptions :
--
--    Socket_Error - Send error
--    Layout_Error - Pointer is not in Data'First..Data'Last + 1
--
   procedure Send
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Send
             (  Client  : in out Connection;
                Data    : String;
                Pointer : in out Integer
             );
--
-- Send -- Data to the client
--
--    Client        - The client connection object
--    Stream        - To get data from to send
--  [ Count ]       - Maximal number of elements to send
--  [ Reserve       - Number of elements to reserve
--    Get_Prefix    - Called to get prefix
--    Get_Suffix ]  - Called to get suffix
--    End_Of_Stream - True if end of stream was reached
--
-- These procedures do not block. End_Of_Stream is set to False when the
-- caller should attempt later.  The procedures  read as  much data from
-- Stream as possible and send it to the client. The length of the chunk
-- is determined  by free contiguous space  in the output buffer and the
-- number  of  elements  returned  by  Stream.   The  parameter  Reserve
-- specifies the  maximum elements  to reserve in the  output buffer for
-- the prefix and suffix  returned by  Get_Prefix  and  Get_Suffix.  The
-- prefix  is sent  before and the suffix after the  chunk of  data read
-- from  Stream.  When the parameter Count is specified,  it limits  the
-- number  of elements sent.  The procedure  ends when  Count reaches 0.
-- When Stream  ends  prematurely  End_Of_Stream  is True  and  Count is
-- non-zero. Count value does not limit prefix and suffix when these are
-- used.
--
-- Exceptions :
--
--    Data_Error   - Output buffer is too small for prefix and suffix
--    Socket_Error - Send error
--
   procedure Send
             (  Client : in out Connection;
                Stream : in out Root_Stream_Type'Class;
                End_Of_Stream : out Boolean
             );
   procedure Send
             (  Client : in out Connection;
                Stream : in out Root_Stream_Type'Class;
                Count  : in out Stream_Element_Count;
                End_Of_Stream : out Boolean
             );
   type Create_Stream_Element_Array is access function
        (  Client : access Connection'Class;
           Data   : Stream_Element_Array;
           End_Of_Stream : Boolean
        )  return Stream_Element_Array;
   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Count         : in out Stream_Element_Count;
                Reserve       : Stream_Element_Count;
                Get_Prefix    : Create_Stream_Element_Array;
                Get_Suffix    : Create_Stream_Element_Array;
                End_Of_Stream : out Boolean
             );
   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Reserve       : Stream_Element_Count;
                Get_Prefix    : Create_Stream_Element_Array;
                Get_Suffix    : Create_Stream_Element_Array;
                End_Of_Stream : out Boolean
             );
   type Create_String is access function
        (  Client : access Connection'Class;
           Data   : Stream_Element_Array;
           End_Of_Stream : Boolean
        )  return String;
   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Reserve       : Natural;
                Get_Prefix    : Create_String;
                Get_Suffix    : Create_String;
                End_Of_Stream : out Boolean
             );
   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Count         : in out Stream_Element_Count;
                Reserve       : Natural;
                Get_Prefix    : Create_String;
                Get_Suffix    : Create_String;
                End_Of_Stream : out Boolean
             );
--
-- Send_Error -- Data send error
--
--    Client     - The client connection object
--    Occurrence - The socket error
--
-- This procedure is called upon socket send error before the connection
-- is dropped. The default implementation does nothing.
--
   procedure Send_Error
             (  Client     : in out Connection;
                Occurrence : Exception_Occurrence
             );
--
-- Sent -- Data sent notification
--
--    Client - The client connection object
--
-- This procedure  is called  when some portion of data was successfully
-- sent leaving free space in the output buffer.  The implementation may
-- try to send data  pending  after the  last  call  to Send,  e.g.  the
-- contents  of  Data (Pointer..Data'Last).  The default  implementation
-- does nothing.
--
   procedure Sent (Client : in out Connection);
--
-- Set_Expected_Count -- Set minimal elements to read
--
--    Client - The client connection object
--    Count  - The number of elements to read before calling Received
--
-- This procedure  sets  the number  of elements  to  accumulate  before
-- calling Received.  When Count is 0 any number of read elements causes
-- a call to Received. When Count  is larger than the input buffer size,
-- Received is called for each full buffer. Note that Set_Expected_Count
-- has effect only once.  When all elements are read the count is set to
-- 0.
--
   procedure Set_Expected_Count
             (  Client : in out Connection;
                Count  : Stream_Element_Count
             );
--
-- Set_Overlapped_Size -- Read policy
--
--    Client - The client connection object
--    Size   - Maximum sent elements queued when read from socket
--
-- This procedure sets the read socket policy when there are  sent  data
-- pending.  The  parameter  Size  specifies  the maximum amount of data
-- queued for send without blocking read. The default value is 0 meaning
-- strictly  half-duplex  behavior.  That is when nothing is read before
-- the  client  accepts  all  data.  Typically  for  a   packet-oriented
-- protocol,  the  server  reads  a  packet  completely and then sends a
-- response packet or a set of packets back. Doing that it stops reading
-- new packets. To implement such policy Set_Overlapped_Size is set to 0
-- and the output buffer size is set to the maximum packet length.  This
-- would  guarantee  that Send called from Received would always be able
-- to queue a complete packet.
--
   procedure Set_Overlapped_Size
             (  Client : in out Connection;
                Size   : Stream_Element_Count
             );
--
-- Trace -- Tracing facility
--
--    Factory - The factory object
--    Message - Text description of the error context
--
-- This procedure is called to trace message.
--
   procedure Trace
             (  Factory : in out Connections_Factory;
                Message : String
             );
--
-- Trace_Off -- Disable tracing
--
--    Factory - The factory object
--
   procedure Trace_Off (Factory : in out Connections_Factory);
--
-- Trace_On -- Enable tracing onto strandard output
--
--    Factory  - The factory object
--    Received - Trace incoming data
--    Sent     - Trace outgoing data
--
   procedure Trace_On
             (  Factory  : in out Connections_Factory;
                Received : Boolean := False;
                Sent     : Boolean := False
             );
--
-- Trace_On -- Enable tracing onto a file
--
--    Factory  - The factory object
--    Name     - The trace file name
--    Received - Trace incoming data
--    Sent     - Trace outgoing data
--
-- If  there is already trace file,  it is closed.  Then  the  procedure
-- opens or creates the file Name.
--
-- Exceptions :
--
--    I/O errors upon closing and opening files
--
   procedure Trace_On
             (  Factory  : in out Connections_Factory;
                Name     : String;
                Received : Boolean := False;
                Sent     : Boolean := False
             );
--
-- Trace_Error -- Error tracing
--
--    Factory    - The factory object
--    Context    - Text description of the error context
--    Occurrence - The error occurrence
--
-- This procedure  is called when  an unanticipated exception is caught.
-- The default implementation calls to Trace.
--
   procedure Trace_Error
             (  Factory    : in out Connections_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             );
--
-- Trace_Received -- Tracing facility
--
--    Factory - The factory object
--    Client  - The client
--    Data    - The client's input buffer
--    From    - The first element received in the buffer
--    To      - The last element received in the buffer
--
-- This procedure  is called when  tracing incoming data is active.  The
-- default implementation calls to Trace.
--
   procedure Trace_Received
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset
             );
--
-- Trace_Sending -- Tracing facility
--
--    Factory - The factory object
--    Client  - The client
--    Enabled - Polling socket for writing is enabled/disabled
--
-- This  procedure is  called when the socket is enabled or disabled for
-- polling.  Polling is  disabled  when  there  is nothing  to send  and
-- enabled  when  output buffer is filled.  The  default  implementation
-- calls to Trace.
--
   procedure Trace_Sending
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Enabled : Boolean
             );
--
-- Trace_Sent -- Tracing facility
--
--    Factory - The factory object
--    Client  - The client
--    Data    - The client's output buffer
--    From    - The first element sent in the buffer
--    To      - The last element sent in the buffer
--
-- This procedure is called when tracing outgoing data is active. The
-- default implementation calls to Trace.
--
   procedure Trace_Sent
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset
             );
--
-- To_String -- Conversion to string
--
--    Data - To convert
--
-- Returns :
--
--    The result
--
   function To_String (Data : Stream_Element_Array) return String;

   package Stream_Element_Offset_Edit is
      new Strings_Edit.Integer_Edit (Stream_Element_Offset);
   use Stream_Element_Offset_Edit;
------------------------------------------------------------------------
--
-- Internal low-level socket I/O operations
--
-- Read -- Socket read
--
--    Client  - The client
--    Factory - The factory object
--
   procedure Read
             (  Client  : in out Connection;
                Factory : in out Connections_Factory'Class
             );
--
-- Write -- Socket write
--
--    Client  - The client
--    Factory - The factory object
--    Blocked - Nothing to send, should block writing
--
   procedure Write
             (  Client  : in out Connection;
                Factory : in out Connections_Factory'Class;
                Blocked : out Boolean
             );
private
   pragma Inline (Available_To_Process);
   pragma Inline (Available_To_Send);
   pragma Inline (Has_Data);
   pragma Inline (Queued_To_Send);

   type Connections_Server_Ptr is access all Connections_Server'Class;
   type Connection
        (  Input_Size  : Buffer_Length;
           Output_Size : Buffer_Length
        )  is abstract new Object.Entity with
   record
      Socket           : Socket_Type := No_Socket;
      Overlapped_Read  : Stream_Element_Count  := 0;
      Expected         : Stream_Element_Offset := 0;
      First_Read       : Stream_Element_Offset := 0;
      First_Written    : Stream_Element_Offset := 0;
      Free_To_Read     : Stream_Element_Offset := 0;
      Free_To_Write    : Stream_Element_Offset := 0;
      Failed           : Boolean := False;
      External_Action  : Boolean := False;
      Data_Sent        : Boolean := False;
      Send_Blocked     : Boolean := False;
      Dont_Block       : Boolean := False;
      Predecessor      : Connection_Ptr;
      Successor        : Connection_Ptr;
      Listener         : Connections_Server_Ptr;
      Last_Error       : Exception_Occurrence;
      Client_Address   : Sock_Addr_Type;
      Read             : Stream_Element_Array (0..Input_Size);
      Written          : Stream_Element_Array (0..Output_Size);

      pragma Atomic (Data_Sent);
      pragma Atomic (Failed);
      pragma Atomic (First_Read);
      pragma Atomic (First_Written);
      pragma Atomic (Free_To_Read);
      pragma Atomic (Free_To_Write);
      pragma Atomic (Send_Blocked);
      pragma Atomic (Dont_Block);
   end record;
--
-- Data_Sent -- Data sent notification
--
--    Listener - The server object
--    Client   - The client connection to handle
--
-- This procedure is used internally to handle outgoing data.
--
   procedure Data_Sent
             (  Listener : in out Connections_Server;
                Client   : Connection_Ptr
             );
--
-- Process -- Input data processing
--
--    Listener  - The server object
--    Client    - The client connection to handle
--    Data_Left - Unprocessed input left
--
-- This procedure is used internally to handle incoming data.
--
   procedure Process
             (  Listener  : in out Connections_Server;
                Client    : Connection_Ptr;
                Data_Left : out Boolean
             );

   procedure Queue
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Service_Postponed -- Postponed input data processing
--
--    Listener - The server object
--
   procedure Service_Postponed (Listener : in out Connections_Server);
--
-- Stop -- Stop servicing a socket
--
--    Listener - The server object
--    Socket   - The socket
--
   procedure Stop
             (  Listener : in out Connections_Server'Class;
                Socket   : Socket_Type
             );

   package Connection_Handles is
      new Object.Handle (Connection, Connection_Ptr);
   use Connection_Handles;

   package Connection_Arrays is
      new Connection_Handles.Generic_Unbounded_Array
          (  Index_Type  => Socket_Type,
             Handle_Type => Connection_Handles.Handle
          );
   use Connection_Arrays;

   task type Worker (Listener : access Connections_Server'Class);
   type Worker_Ptr is access Worker;

   type Connections_Factory is abstract
      new Ada.Finalization.Limited_Controlled with
   record
      Standard_Output : Boolean := False;
      Trace_Sent      : Boolean := False;
      Trace_Received  : Boolean := False;
      Trace_File      : Ada.Text_IO.File_Type;

      pragma Atomic (Standard_Output);
      pragma Atomic (Trace_Sent);
      pragma Atomic (Trace_Received);
   end record;

   type Connections_Server
        (  Factory : access Connections_Factory'Class;
           Port    : Port_Type
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Clients         : Natural := 0;
      Selector        : Selector_Type;
      Read_Sockets    : Socket_Set_Type;
      Write_Sockets   : Socket_Set_Type;
      Blocked_Sockets : Socket_Set_Type;
      Postponed       : Connection_Ptr;
      IO_Timeout      : Duration := 0.02;
      Ready_To_End    : Boolean  := False;
      Unblock_Send    : Boolean  := False;
      Connections     : Unbounded_Array;
      Doer            : Worker_Ptr;

      pragma Atomic (Unblock_Send);
   end record;
--
-- Queue operations
--
   procedure Append
             (  List  : in out Connection_Ptr;
                Item  : Connection_Ptr;
                Count : in out Integer
             );
   procedure Remove
             (  List  : in out Connection_Ptr;
                Item  : in out Connection'Class;
                Count : in out Integer
             );
   pragma Inline (Append);
   pragma Inline (Remove);

end GNAT.Sockets.Server;
