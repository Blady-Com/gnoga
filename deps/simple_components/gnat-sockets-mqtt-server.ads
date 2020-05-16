--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.MQTT.Server                    Luebeck            --
--  Interface                                      Spring, 2016       --
--                                                                    --
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

with Ada.Calendar;             use Ada.Calendar;
with Synchronization.Mutexes;  use Synchronization.Mutexes;

with Ada.Finalization;
with Generic_FIFO;
with Generic_Map;
with Generic_Set;
with GNAT.Sockets.Server.Handles;
with Object.Handle.Generic_Set;
with Tables;

package GNAT.Sockets.MQTT.Server is

   type MQTT_Trace_Flags is mod 2**5;
   Trace_Acknowledgement : constant MQTT_Trace_Flags := 2**0;
   Trace_Sessions        : constant MQTT_Trace_Flags := 2**1;
   Trace_Subscriptions   : constant MQTT_Trace_Flags := 2**2;
   Trace_Ping            : constant MQTT_Trace_Flags := 2**3;
   Trace_Pubishing       : constant MQTT_Trace_Flags := 2**4;
   Trace_All             : constant MQTT_Trace_Flags :=
                                                  MQTT_Trace_Flags'Last;
--
-- Message_Type -- Message handling policy
--
   type Message_Type is
        (  Transient, -- The message is not retained
           Retained,  -- The message always replaces the retained one
           Updated,   -- Ignored if same as the retained one
           Initial,   -- Ignored if already retained
           Ignored    -- Ignored message
        );
------------------------------------------------------------------------
-- MQTT_Server -- Server is shared between several sessions.  The topics
--                are published  on the server  and become available for
-- all sessions. Each MQTT client has a session. The session may outlive
-- the client.  When the client connects again, messages retained in the
-- last session become available to it.
--
   type MQTT_Server is
      new Ada.Finalization.Limited_Controlled with private;
--
-- MQTT_Connection -- MQTT session server
--
--    Server               - The server used with the session
--    Listener             - The connections server object
--    Input_Size           - Input buffer size
--    Output_Size          - Output buffer size
--    Max_Subscribe_Topics - In a subscribe request
--
-- The connection handles a client connected to the server. A client has
-- a session  associated with it.  The client  sessions are kept  by the
-- server.
--
    type MQTT_Connection
         (  Server               : access MQTT_Server'Class;
            Listener             : access Connections_Server'Class;
            Input_Size           : Buffer_Length;
            Output_Size          : Buffer_Length;
            Max_Subscribe_Topics : Positive
         )  is new MQTT_Peer with private;
--
-- Drop -- Session
--
--    Server       - The MQTT server
--    Name / Index - The session name or number 1..Get_Messages_Number
--
-- This procedure removes a session by its name or index.
--
-- Exceptions :
--
--    Constraint_Error - Invalid session number
--
   procedure Drop
             (  Server : in out MQTT_Server;
                Name   : String
             );
   procedure Drop
             (  Server : in out MQTT_Server;
                Index  : Positive
             );
--
-- Get_Messages_Number -- Get total number of retained messages
--
--    Server - The MQTT server
--
-- Returns :
--
--    The number of retained messages
--
   function Get_Messages_Number
            (  Server : MQTT_Server
            )  return Natural;
--
-- Get_Message -- Get retained message by its number
--
--    Server - The MQTT server
--    Index  - The message number 1..Get_Messages_Number
--
-- Returns :
--
--    The retained message or message index
--
-- Exceptions :
--
--    Constraint_Error - Invalid message number
--    End_Error        - There is no such message or the result is 0
--
   function Get_Message
            (  Server : MQTT_Server;
               Index  : Positive
            )  return MQTT_Message;
--
-- Get_Message -- Get retained message by its topic
--
--    Server - The MQTT server
--    Topic  - The message topic
--
-- Returns :
--
--    The retained message
--
-- Exceptions :
--
--    Constraint_Error - Invalid topic
--    End_Error        - There is no such message
--
   function Get_Message
            (  Server : MQTT_Server;
               Topic  : String
            )  return MQTT_Message;
   function Get_Message
            (  Server : MQTT_Server;
               Topic  : String
            )  return Integer;
--
-- Get_Name -- The client session name
--
--    Client - The connection object
--
-- Returns :
--
--    The session name, empty if anonymous
--
   function Get_Name (Client : MQTT_Connection) return String;
--
-- Get_Queue_Size -- Get maximum number of queued messages
--
--    Server - The MQTT server
--
-- Returns :
--
--    The maximum number of queued pubished messages
--
   function Get_Queue_Size
            (  Server : MQTT_Server
            )  return Positive;
--
-- Get_Session -- Get session by its name
--
--    Server - The MQTT server
--    Name   - The session name
--
-- Returns :
--
--    The session number or 0
--
   function Get_Session
            (  Server : MQTT_Server;
               Name   : String
            )  return Integer;
--
-- Get_Session_Name -- Get name of a session
--
--    Server - The MQTT server
--    Index  - The session number 1..Get_Sessions_Number
--
-- Returns :
--
--    The name of the session, empty if anonymous
--
-- Exceptions :
--
--    End_Error - Wrong index number
--
   function Get_Session_Name
            (  Server : MQTT_Server;
               Index  : Positive
            )  return String;
--
-- Get_Session_Time -- Get the last time a session was active
--
--    Server       - The MQTT server
--    Name / Index - The session name or number 1..Get_Sessions_Number
--
-- Returns :
--
--    The last time the session was active
--
-- Exceptions :
--
--    End_Error - No such session or a wrong index number
--
   function Get_Session_Time
            (  Server : MQTT_Server;
               Name   : String
            )  return Time;
   function Get_Session_Time
            (  Server : MQTT_Server;
               Index  : Positive
            )  return Time;
--
-- Get_Sessions_Number -- Get number of sessions
--
--    Server - The MQTT server
--
-- Returns :
--
--    The number of sessions, active or not
--
   function Get_Sessions_Number
            (  Server : MQTT_Server
            )  return Natural;
--
-- Get_Tracing_Flags -- Get tracing flags
--
--    Server - The MQTT server
--
-- Returns :
--
--    Server tracing flags
--
   function Get_Tracing_Flags
            (  Server : MQTT_Server
            )  return MQTT_Trace_Flags;
--
-- Is_Tracing_On -- Check if one of given tracing flags enabled
--
--    Server - The MQTT server
--    Flags  - A combination of flags
--
-- Returns :
--
--    Server tracing flags
--
   function Is_Tracing_On
            (  Server : MQTT_Server;
               Flags  : MQTT_Trace_Flags
            )  return Boolean;
--
-- Is_Session_Active -- Check if a session is active
--
--    Server       - The MQTT server
--    Name / Index - The session name or number 1..Get_Sessions_Number
--
-- Returns :
--
--    True if the session is active
--
-- Exceptions :
--
--    End_Error - No such session or a wrong index number
--
   function Is_Session_Active
            (  Server : MQTT_Server;
               Name   : String
            )  return Boolean;
   function Is_Session_Active
            (  Server : MQTT_Server;
               Index  : Positive
            )  return Boolean;
--
-- Publish -- Publish a message
--
--    Server  - The MQTT server
--    Topic   - The topic published
--    Message - The message
--    QoS     - The minimal QoS level
--    Policy  - The message handling policy
--
-- Exceptions:
--
--    Name_Error - Illegal topic
--
   procedure Publish
             (  Server  : in out MQTT_Server;
                Topic   : String;
                Message : Stream_Element_Array;
                QoS     : QoS_Level    := At_Most_Once;
                Policy  : Message_Type := Transient
             );
   procedure Publish
             (  Server  : in out MQTT_Server;
                Topic   : String;
                Message : String;
                QoS     : QoS_Level    := At_Most_Once;
                Policy  : Message_Type := Transient
             );
--
-- Publish -- Publish a couple of messages
--
--    Server     - The MQTT server
--    Message[s] - The message or an array of
--    QoS        - The minimal QoS level
--    Policy     - The messages handling policy
--
-- Invalid elements of Messages are ignored.
--
   type MQTT_Messages_Array is
      array (Positive range <>) of MQTT_Message;
   procedure Publish
             (  Server  : in out MQTT_Server;
                Message : MQTT_Message;
                QoS     : QoS_Level    := At_Most_Once;
                Policy  : Message_Type := Transient
             );
   procedure Publish
             (  Server   : in out MQTT_Server;
                Messages : MQTT_Messages_Array;
                QoS      : QoS_Level    := At_Most_Once;
                Policy   : Message_Type := Transient
             );
--
-- Received -- Incoming message notification
--
--    Server  - The MQTT server
--    Client  - The client
--    Topic   - The topic published
--    Message - The message
--    QoS     - The minimal QoS level
--    Policy  - To use with the message
--
-- The default implementation  does  not change Policy.  An  alternative
-- implementation may filter messages it wishes to propagate further.
--
   procedure Received
             (  Server  : in out MQTT_Server;
                Client  : in out MQTT_Connection'Class;
                Topic   : String;
                Message : Stream_Element_Array;
                QoS     : QoS_Level;
                Policy  : in out Message_Type
             );
--
-- Remove -- Retained messages
--
--    Server - The MQTT server
--    Topic  - The topics to remove (patterns allowed)
--
-- This procedure removes retained messages matched by Topic.
--
   procedure Remove
             (  Server : in out MQTT_Server;
                Topic  : String
             );
--
-- Remove -- Retained messages
--
--    Server - The MQTT server
--    Index  - The message number 1..Get_Messages_Number
--
-- This procedure removes a retained message by its index.
--
-- Exceptions :
--
--    Constraint_Error - Invalid message number
--
   procedure Remove
             (  Server : in out MQTT_Server;
                Index  : Positive
             );
--
-- Set_Queue_Size -- Set maximum number of queued messages
--
--    Server - The MQTT server
--    Size   - The maximum published messages queue size
--
-- The call influences future connections to the server
--
   procedure Set_Queue_Size
             (  Server : in out MQTT_Server;
                Size   : Positive
             );
--
-- Set_Tracing_Flags -- Enable or disable tracing of server's actions
--
--    Server - The MQTT server
--    Flags  - Tracing flags
--
   procedure Set_Tracing_Flags
             (  Server : in out MQTT_Server;
                Flags  : MQTT_Trace_Flags
             );
private
   pragma Inline (Is_Tracing_On);

   type MQTT_Connection_Ptr is access all MQTT_Connection'Class;

   function Less  (Left, Right : Message_Object_Ptr) return Boolean;
   function Equal (Left, Right : Message_Object_Ptr) return Boolean;

   function To_Object_Ptr (Ptr : Message_Object_Ptr)
      return Message_Object_Ptr;

   package Messages_Sets is
      new Generic_Set
          (  Object_Type  => Message_Object_Ptr,
             Null_Element => null,
             "="          => Equal,
             "<"          => Less
          );
   type Messages_Set is record
      Set : Messages_Sets.Set;
   end record;
   procedure Erase (Set : in out Messages_Set);
   procedure Remove
             (  Set   : in out Messages_Set;
                Index : Positive
             );
------------------------------------------------------------------------
   type Message_Flags is mod 2**5;
   At_Least_Once : constant Message_Flags := 2**0;
   Exactly_Once  : constant Message_Flags := 2**1;
   Retain        : constant Message_Flags := 2**2;
   Duplicate     : constant Message_Flags := 2**3;
   Incoming      : constant Message_Flags := 2**4;
   QoS_Mask      : constant Message_Flags :=
                                          At_Least_Once or Exactly_Once;
   type Messages_Queue_Item is record
      Flags   : Message_Flags;
      Packet  : Packet_Identifier;
      Message : Message_Object_Ptr;
   end record;
   package Messages_Queue is new Generic_FIFO (Messages_Queue_Item);
   use Messages_Queue;

   type Message_Response_Item is record
      Flags   : Message_Flags;
      Message : Message_Object_Ptr;
   end record;
   package Messages_Maps is
      new Generic_Map (Packet_Identifier, Message_Response_Item);
   use Messages_Maps;

   package Topic_Tables is new Tables (QoS_Level);
   use Topic_Tables;

   type Subscription_List is record
      Singular : Topic_Tables.Table;
      Patterns : Topic_Tables.Table;
   end record;
------------------------------------------------------------------------
-- MQTT_Session -- MQTT session for a client
--
   type MQTT_Session
        (  Length     : Natural;
           Queue_Size : Positive
        )  is new Object.Entity with
   record
      Client        : MQTT_Connection_Ptr;
      Reference     : GNAT.Sockets.Server.Handles.Handle;
      Packet        : Packet_Identifier := 0;
      Used          : Natural := 0; -- Number of times it was used
      Persistent    : Boolean := False;
      Disconnected  : Boolean := False;
      Will_Retain   : Boolean := False;
      Will_QoS      : QoS_Level;
      Will_Message  : Messages_Handles.Handle;
      Timeout       : Duration;
      Last          : Time;
      Subscribtions : Subscription_List;
      Input         : Messages_Maps.Map; -- Awaiting
      Output        : FIFO (Queue_Size); -- To be send
      Name          : String (1..Length);
   end record;
   type MQTT_Session_Ptr is access MQTT_Session'Class;

   procedure Erase (Session : in out MQTT_Session);
   procedure Finalize (Session : in out MQTT_Session);
   procedure Pump_Unchecked (Session : in out MQTT_Session);
   procedure Push
             (  Session   : in out MQTT_Session;
                Topic     : String;
                Message   : Stream_Element_Array;
                Retain    : Boolean;
                Duplicate : Boolean;
                Handle    : in out Messages_Handles.Handle
             );

   package MQTT_Session_Handles is
      new Object.Handle (MQTT_Session, MQTT_Session_Ptr);
   use MQTT_Session_Handles;

   package MQTT_Session_Sets is new MQTT_Session_Handles.Generic_Set;
   use MQTT_Session_Sets;

   package MQTT_Session_Tables is
      new Tables (MQTT_Session_Handles.Handle);
   use MQTT_Session_Tables;
------------------------------------------------------------------------
   type MQTT_Connection
        (  Server               : access MQTT_Server'Class;
           Listener             : access Connections_Server'Class;
           Input_Size           : Buffer_Length;
           Output_Size          : Buffer_Length;
           Max_Subscribe_Topics : Positive
        )  is new MQTT_Peer
                  (  Listener             => Listener,
                     Input_Size           => Input_Size,
                     Output_Size          => Output_Size,
                     Max_Subscribe_Topics => Max_Subscribe_Topics
                  )  with
   record
      Session : MQTT_Session_Handles.Handle;
   end record;
--
-- Overriding GNAT.Sockets.Server...
--
   procedure Disconnected (Client : in out MQTT_Connection);
--
-- Overriding GNAT.Sockets.MQTT...
--
   procedure On_Acknowledge
             (  Client  : in out MQTT_Connection;
                Request : Acknowledge_Type;
                Packet  : Packet_Identifier
             );
   procedure On_Connect
             (  Client       : in out MQTT_Connection;
                Name         : String;
                Clean        : Boolean;
                Will_Topic   : String;
                Will_Message : Stream_Element_Array;
                Will_QoS     : QoS_Level;
                Will_Retain  : Boolean;
                User_Name    : String;
                Password     : String;
                Keep_Alive   : Duration
             );
   procedure On_Disconnect (Client : in out MQTT_Connection);
   procedure On_Ping (Client : in out MQTT_Connection);
   procedure On_Ping_Response (Client : in out MQTT_Connection);
   procedure On_Publish
             (  Client    : in out MQTT_Connection;
                Topic     : String;
                Message   : Stream_Element_Array;
                Packet    : Packet_Identification;
                Duplicate : Boolean;
                Retain    : Boolean
             );
   procedure On_Subscribe
             (  Client        : in out MQTT_Connection;
                Packet        : Packet_Identifier;
                Topics_Number : Positive
             );
   procedure On_Unsubscribe
             (  Client        : in out MQTT_Connection;
                Packet        : Packet_Identifier;
                Topics_Number : Positive
             );
   procedure Send
             (  Client : in out MQTT_Connection;
                Data   : Stream_Element_Array
             );
   procedure Send_Acknowledge
             (  Client  : in out MQTT_Connection;
                Request : Acknowledge_Type;
                Packet  : Packet_Identifier
             );
   procedure Sent (Client : in out MQTT_Connection);
------------------------------------------------------------------------
   type MQTT_Server_Ptr is access all MQTT_Server'Class;
   type MQTT_Server is
      new Ada.Finalization.Limited_Controlled with
   record
      Self       : MQTT_Server_Ptr  := MQTT_Server'Unchecked_Access;
      Lock       : aliased Mutex;
      Queue_Size : Positive         := 1024 * 10;
      Flags      : MQTT_Trace_Flags := 0;
      Last       : Positive         := 1;
      Retained   : Messages_Set; -- Retained messages
      Named      : MQTT_Session_Tables.Table;
      Anonymous  : MQTT_Session_Sets.Set;

      pragma Atomic (Flags);
      pragma Atomic (Queue_Size);
   end record;

   procedure Finalize (Server : in out MQTT_Server);
   procedure Publish_Unchecked
             (  Server  : in out MQTT_Server;
                Topic   : String;
                Message : Stream_Element_Array;
                QoS     : QoS_Level;
                Policy  : Message_Type
             );
   procedure Publish_Unchecked
             (  Server : in out MQTT_Server;
                Handle : in out Messages_Handles.Handle;
                QoS    : QoS_Level;
                Policy : Message_Type
             );
   procedure Publish_Unchecked
             (  Server   : in out MQTT_Server;
                Messages : MQTT_Messages_Array;
                QoS      : QoS_Level;
                Policy   : Message_Type
             );

   pragma Inline (Equal);
   pragma Inline (Less);
   pragma Inline (Remove);

end GNAT.Sockets.MQTT.Server;
