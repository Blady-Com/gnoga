--                                                                    --
--  package GNAT.Sockets.MQTT       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2016       --
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

with Ada.Streams;          use Ada.Streams;
with GNAT.Sockets.Server;  use GNAT.Sockets.Server;
with Interfaces;           use Interfaces;

with Generic_Unbounded_Ptr_Array;
with Object.Handle;

package GNAT.Sockets.MQTT is
--
-- MQTT_Port -- The default MQTT port
--
   MQTT_Port : constant := 1883;
--
-- QoS_Level -- Quality of service
--
--    At_Most_Once  - The  message  is  delivered   according   to   the
--                    capabilities  of  the   underlying   network.   No
--                    response is sent by the receiver and no  retry  is
--                    performed  by  the  sender. The message arrives at
--                    the receiver either once or not at all.
--    At_Least_Once - The message arrives at the receiver at least once.
--    Exactly_Once  - The highest  quality  of  service,  for  use  when
--                    neither  loss  nor  duplication  of  messages  are
--                    acceptable.  There  is   an   increased   overhead
--                    associated with this quality of service.
--
   type QoS_Level is (At_Most_Once, At_Least_Once, Exactly_Once);
   type QoS_Level_Array is array (Positive range <>) of QoS_Level;
--
-- Image -- QoS level representation
--
--    QoS - The QoS level
--
-- Returns :
--
--    Textual representation
--
   function Image (QoS : QoS_Level) return String;
--
-- + -- Merge two QoS levels
--
--    Left  - QoS level
--    Right - Another QoS
--
-- Returns :
--
--    Maximum of the arguments
--
   function "+" (Left, Right : QoS_Level) return QoS_Level;
--
-- Acknowledge_Type - Identifies the request being acknowledged
--
   type Acknowledge_Type is
        (  Publish_Level_1,
           Publish_Level_2_Received,
           Publish_Level_2_Release,
           Publish_Level_2_Complete,
           Unsubscribed
        );
   function Image (Value : Acknowledge_Type) return String;
--
-- Connect_Response -- Reasons for connect request rejection
--
   type Connect_Response is range 1..255;
   Unacceptable_Protocol_Version : constant Connect_Response := 1;
   Identifier_Rejected           : constant Connect_Response := 2;
   Server_Unavailable            : constant Connect_Response := 3;
   Bad_User_Name_Or_Password     : constant Connect_Response := 4;
   Not_Authorized                : constant Connect_Response := 5;
   function Image (Code : Connect_Response) return String;
--
-- Packet_Identifier -- Identifies a request.  When QoS  is At_Most_Once
--                      identification is absent
--
   type Packet_Identifier is new Unsigned_16;
   type Packet_Identification
        (  QoS : QoS_Level := At_Most_Once
        )  is
   record
      case QoS is
         when At_Most_Once =>
            null;
         when At_Least_Once | Exactly_Once =>
            ID : Packet_Identifier;
      end case;
   end record;
--
-- Return_Code -- Returned in a subscription response
--
   type Return_Code (Success : Boolean := False) is record
      case Success is
         when True =>
            QoS : QoS_Level;
         when False =>
            null;
      end case;
   end record;
   type Return_Code_List is array (Positive range <>) of Return_Code;
--
-- Topics lists
--
   type Topics_List (<>) is private;
   function Get_Length (List : Topics_List) return Natural;
   function Get_Topic (List : Topics_List; Index : Positive)
      return String;
   function "+" (Left : String) return Topics_List;
   function "/" (Left : String; Right : String) return Topics_List;
   function "/" (Left : Topics_List; Right : String) return Topics_List;
------------------------------------------------------------------------
-- MQTT_Messages -- A composite message object
--
   type MQTT_Message is tagged private;
--
-- Compose -- Create a message
--
--    Topic   - The topic
--    Message - The message content
--
-- Returns :
--
--    The message
--
-- Exceptions :
--
--    Constraint_Error - Invalid topic
--
   function Compose
            (  Topic   : String;
               Message : Stream_Element_Array
            )  return MQTT_Message;
   function Compose
            (  Topic   : String;
               Message : String
            )  return MQTT_Message;
--
-- Get_Message -- Get message content
--
--    Message - The message object
--
-- Returns :
--
--    The message content
--
-- Exceptions :
--
--    Constraint_Error - Invalid message object
--
   function Get_Message (Message : MQTT_Message)
      return Stream_Element_Array;
   function Get_Message (Message : MQTT_Message) return String;
--
-- Get_Topic -- Get topic
--
--    Message - The message object
--
-- Returns :
--
--    The topic
--
-- Exceptions :
--
--    Constraint_Error - Invalid message object
--
   function Get_Topic (Message : MQTT_Message) return String;
--
-- Set_Message -- Set message content
--
--    Message - The message object
--    Content - The message content
--
-- Exceptions :
--
--    Constraint_Error - Invalid message object
--
   procedure Set_Message
             (  Message : in out MQTT_Message;
                Content : Stream_Element_Array
             );
   procedure Set_Message
             (  Message : in out MQTT_Message;
                Content : String
             );
--
-- Set_Size -- Set message content size
--
--    Message - The message object
--    Size    - The maximum message content size
--
-- Exceptions :
--
--    Constraint_Error - Invalid message object
--
   procedure Set_Size
             (  Message : in out MQTT_Message;
                Size    : Stream_Element_Count
             );
--
-- Set_Topic -- Set message topic
--
--    Message - The message object
--    Topic   - The message topic
--
-- Exceptions :
--
--    Constraint_Error - Invalid message topic
--
   procedure Set_Topic
             (  Message : in out MQTT_Message;
                Topic   : String
             );
------------------------------------------------------------------------
-- MQTT_Peer -- Server or client of MQTT protocol
--
--    Listener             - The connections server object
--    Max_Subscribe_Topics - In a single subscribe request
--    Input_Size           - Input buffer size
--    Output_Size          - Output buffer size
--
-- This type  is suitable as  an ancestor  for a custom  MQTT  client or
-- server.  For a server  see also  GNAT.Sockets.MQTT.Server.MQTT_Server
-- capable of  maintaining the hierachy  of topics  subscription and QoS
-- protocol.
--
   type MQTT_Peer
        (  Listener             : access Connections_Server'Class;
           Max_Subscribe_Topics : Positive;
           Input_Size           : Buffer_Length;
           Output_Size          : Buffer_Length
        )  is new Connection with private;
--
-- Finalize -- Overrides GNAT.Sockets.Server...
--
--    Peer - The MQTT connection object
--
-- This  procedure  must  be  called  from  the  new  implementation  if
-- overridden
--
   procedure Finalize (Peer : in out MQTT_Peer);
--
-- Check_Topic -- Check topic for validity
--
--    Topic - The topic
--
-- Returns :
--
--    True if the topic contains wildcards
--
-- Exceptions :
--
--    Constraint_Error - Invalid topic
--
   function Check_Topic (Topic : String) return Boolean;
--
-- Get_Max_Message_Size -- Get maximum message data size
--
--    Peer - The MQTT connection object
--
-- Returns :
--
--    The maximum size of a message
--
   function Get_Max_Message_Size (Peer : MQTT_Peer)
      return Stream_Element_Count;
--
-- Get_Max_Secondary_Buffer_Size -- The secondary buffer size limit
--
--    Peer - The MQTT connection object
--
-- Returns :
--
--    The maximum buffer size, zero if not limited
--
   function Get_Max_Secondary_Buffer_Size
            (  Peer : MQTT_Peer
            )  return Stream_Element_Count;
--
-- Get_QoS -- Get topic QoS
--
--    Peer  - The MQTT connection object
--    Index - The topic number 1..Topics_Number
--
-- Returns :
--
--    The QoS level
--
-- Exceptions :
--
--    Constraint_Error - Wrong topic index
--    Use_Error        - The lates response has no topics
--
   function Get_QoS
            (  Peer  : MQTT_Peer;
               Index : Positive
            )  return QoS_Level;
--
-- Get_Secondary_Buffer_Size -- Get size of the secondary buffer
--
--    Peer - The MQTT connection object
--
-- Returns :
--
--    The actual size of the secondary buffer
--
   function Get_Secondary_Buffer_Size
            (  Peer : MQTT_Peer
            )  return Stream_Element_Count;
--
-- Get_Topic -- Get topic
--
--    Peer  - The MQTT connection object
--    Index - The topic number 1..Topics_Number
--
-- Returns :
--
--    The topic name
--
-- Exceptions :
--
--    Constraint_Error - Wrong topic index
--    Use_Error        - The lates response has no topics
--
   function Get_Topic
            (  Peer  : MQTT_Peer;
               Index : Positive
            )  return String;
--
-- Match_Topic -- Match topic against a pattern
--
--    Topic   - The topic
--    Pattern - The pattern to match against
--
-- The implementation follows the rule that topics starting with '$' are
-- not matched by patterns beginning with '+' or '#'.
--
-- Returns :
--
--    True if the topic matched
--
-- Exceptions :
--
--    Constraint_Error - Invalid topic or pattern
--
   function Match_Topic
            (  Topic   : String;
               Pattern : String
            )  return Boolean;
--
-- Received -- Overrides GNAT.Sockets.Server...
--
   procedure Received
             (  Peer    : in out MQTT_Peer;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Sent -- Overrides GNAT.Sockets.Server...
--
--    Peer - The MQTT connection object
--
-- This  procedure  must  be  called  from  the  new  implementation  if
-- overridden
--
   procedure Sent (Peer : in out MQTT_Peer);
--
-- Set_Max_Data_Size -- Set maximum message size
--
--    Peer - The MQTT connection object
--    Size - The maximum message size
--
   procedure Set_Max_Message_Size
             (  Peer : in out MQTT_Peer;
                Size : Stream_Element_Count
             );
--
-- Set_Max_Secondary_Buffer_Size -- Set maximum secodary buffer size
--
--    Peer - The MQTT connection object
--    Size - The maximum secondary buffer size
--
   procedure Set_Max_Secondary_Buffer_Size
             (  Peer : in out MQTT_Peer;
                Size : Stream_Element_Count := 0
             );
------------------------------------------------------------------------
-- Requests and responses sent to a Peer
--
-- Send_Acknowledge -- Acknowledge publish or unsubscribe request
--
--    Peer    - The MQTT connection object
--    Request - The request being acknowledged
--    Packet  - The packet identifier
--
-- Exceptions :
--
--    Data_Error - Secondary buffer overflow, if limited
--    Use_Error  - Not connected
--
   procedure Send_Acknowledge
             (  Peer    : in out MQTT_Peer;
                Request : Acknowledge_Type;
                Packet  : Packet_Identifier
             );
--
-- Send_Connect -- Connection initiation
--
--    Peer         - The MQTT connection object
--    Client       - The client identification
--    Clean        - Resume the current session when false
--    Will_Topic   - The topic published at the session end
--    Will_Message - The message of the will topic
--    Will_QoS     - The quality of service of the will topic
--    Will_Retain  - Retain the will message
--    User_Name    - The user name
--    Password     - The password
--    Keep_Alive   - The interval of ping packets sent
--
-- This is the first request sent to the server after connecting to it.
--
-- Exceptions :
--
--    Constraint_Error - Invalid parameter
--    Data_Error       - Secondary buffer overflow, if limited
--    Use_Error        - Not connected
--
   procedure Send_Connect
             (  Peer         : in out MQTT_Peer;
                Client       : String;
                Clean        : Boolean              := True;
                Will_Topic   : String               := "";
                Will_Message : Stream_Element_Array := (1..0 => 0);
                Will_QoS     : QoS_Level            := At_Most_Once;
                Will_Retain  : Boolean              := False;
                User_Name    : String               := "";
                Password     : String               := "";
                Keep_Alive   : Duration             := 0.0
             );
--
-- Send_Connect_Accepted -- Send connection acknowledge response
--
--    Peer            - The MQTT connection object
--    Session_Present - The old session is present
--
-- Exceptions :
--
--    Data_Error - Secondary buffer overflow, if limited
--    Use_Error  - Not connected
--
   procedure Send_Connect_Accepted
             (  Peer            : in out MQTT_Peer;
                Session_Present : Boolean := False
             );
--
-- Send_Connect_Rejected -- Send connection rejection response
--
--    Peer     - The MQTT connection object
--    Response - Connect return code
--
-- Exceptions :
--
--    Data_Error - Secondary buffer overflow, if limited
--    Use_Error  - Not connected
--
   procedure Send_Connect_Rejected
             (  Peer     : in out MQTT_Peer;
                Response : Connect_Response
             );
--
-- Send_Disconnect -- Send disconnect
--
--    Peer - The MQTT connection object
--
-- The final packet sent to the server.
--
-- Exceptions :
--
--    Data_Error - Secondary buffer overflow, if limited
--    Use_Error  - Not connected
--
   procedure Send_Disconnect (Peer : in out MQTT_Peer);
--
-- Send_Ping -- Send ping request
--
--    Peer - The MQTT connection object
--
-- Exceptions :
--
--    Data_Error - Secondary buffer overflow, if limited
--    Use_Error  - Not connected
--
   procedure Send_Ping (Peer : in out MQTT_Peer);
--
-- Send_Ping_Response -- Send response to a ping request
--
--    Peer - The MQTT connection object
--
-- Exceptions :
--
--    Data_Error - Secondary buffer overflow, if limited
--    Use_Error  - Not connected
--
   procedure Send_Ping_Response (Peer : in out MQTT_Peer);
--
-- Send_Publish -- Publish message
--
--    Peer      - The MQTT connection object
--    Topic     - The topic published
--    Message   - The message
--    Packet    - The packet identification
--    Duplicate - True if this is not the first attempt
--    Retain    - The message must be retained if true
--
-- Exceptions :
--
--    Constraint_Error - Invalid parameter
--    Data_Error       - Secondary buffer overflow, if limited
--    Use_Error        - Not connected
--
   procedure Send_Publish
             (  Peer      : in out MQTT_Peer;
                Topic     : String;
                Message   : Stream_Element_Array;
                Packet    : Packet_Identification;
                Duplicate : Boolean := False;
                Retain    : Boolean := False
             );
   procedure Send_Publish
             (  Peer      : in out MQTT_Peer;
                Topic     : String;
                Message   : String;
                Packet    : Packet_Identification;
                Duplicate : Boolean := False;
                Retain    : Boolean := False
             );
--
-- Send_Publish -- Publish message
--
--    Peer      - The MQTT connection object
--    Message   - The message as a composite object
--    Packet    - The packet identification
--    Duplicate - True if this is not the first attempt
--    Retain    - The message must be retained if true
--
-- Exceptions :
--
--    Constraint_Error - Invalid parameter
--    Data_Error       - Secondary buffer overflow, if limited
--    Use_Error        - Not connected
--
   procedure Send_Publish
             (  Peer      : in out MQTT_Peer;
                Message   : MQTT_Message'Class;
                Packet    : Packet_Identification;
                Duplicate : Boolean := False;
                Retain    : Boolean := False
             );
--
-- Send_Subscribe -- Subscribe to a list of topics
--
--    Peer   - The MQTT connection object
--    Packet - The packet identifier
--    Topics - The list of topics to subscribe
--    QoS    - The list of correponding QoS
--
-- Exceptions :
--
--    Constraint_Error - Invalid parameter
--    Data_Error       - Secondary buffer overflow, if limited
--    Use_Error        - Not connected
--
   procedure Send_Subscribe
             (  Peer   : in out MQTT_Peer;
                Packet : Packet_Identifier;
                Topics : Topics_List;
                QoS    : QoS_Level_Array
             );
   procedure Send_Subscribe
             (  Peer   : in out MQTT_Peer;
                Packet : Packet_Identifier;
                Topic  : String;
                QoS    : QoS_Level
             );
--
-- Send_Subscribe_Acknowledgement -- Send subscribe acknowledgement
--
--    Peer   - The MQTT connection object
--    Packet - The packet identifier
--    Codes  - The list topic's return codes
--
-- Exceptions :
--
--    Constraint_Error - Invalid parameter
--    Data_Error       - Secondary buffer overflow, if limited
--    Use_Error        - Not connected
--
   procedure Send_Subscribe_Acknowledgement
             (  Peer   : in out MQTT_Peer;
                Packet : Packet_Identifier;
                Codes  : Return_Code_List
             );
--
-- Send_Unsubscribe -- Send subscribe acknowledgement
--
--    Peer   - The MQTT connection object
--    Packet - The packet identifier
--    Topics - The list of topics
--
-- Exceptions :
--
--    Constraint_Error - Invalid parameter
--    Data_Error       - Secondary buffer overflow, if limited
--    Use_Error        - Not connected
--
   procedure Send_Unsubscribe
             (  Peer   : in out MQTT_Peer;
                Packet : Packet_Identifier;
                Topics : Topics_List
             );
------------------------------------------------------------------------
-- Request notification callbacks
--
-- On_Acknowledge -- Acknowledge notification
--
--    Peer    - The MQTT connection object
--    Request - The request being acknowledged
--    Packet  - The packet identifier
--
-- The implementation confirms  QoS level 2 acknowledge packets with the
-- corresponding responses:
--
--    Publish_Level_1          - Ignored
--    Publish_Level_2_Received - Publish_Level_2_Release sent back
--    Publish_Level_2_Release  - Publish_Level_2_Complete sent back
--    Publish_Level_2_Complete - Ignored
--    Unsubscribe              - Ignored
--
-- When this callback is overridden  the new implementation  should call
-- this one.
--
   procedure On_Acknowledge
             (  Peer    : in out MQTT_Peer;
                Request : Acknowledge_Type;
                Packet  : Packet_Identifier
             );
--
-- On_Connect -- Connect notification
--
--    Peer         - The MQTT connection object
--    Client       - The client identification
--    Clean        - Start a new session if the old one persists
--    Will_Topic   - The topic published if the client exits prematurely
--    Will_Message - The last will message to publish
--    Will_QoS     - The QoS of the message
--    Will_Retain  - Keep the message between sessions
--    User_Name    - To use for authentication
--    Password     - The password to use
--    Keep_Alive   - Time interval for ping requests
--
-- The default implementation rejects all connections.
--
   procedure On_Connect
             (  Peer         : in out MQTT_Peer;
                Client       : String;
                Clean        : Boolean;
                Will_Topic   : String;
                Will_Message : Stream_Element_Array;
                Will_QoS     : QoS_Level;
                Will_Retain  : Boolean;
                User_Name    : String;
                Password     : String;
                Keep_Alive   : Duration
             );
--
-- On_Connect_Accepted -- Connect acknowledgement notification
--
--    Peer            - The MQTT connection object
--    Session_Present - The old session is present
--
-- This  procedure is called when  the server  accepts  connection.  The
-- default implementation does nothing.
--
   procedure On_Connect_Accepted
             (  Peer            : in out MQTT_Peer;
                Session_Present : Boolean
             );
--
-- On_Connect_Rejected -- Connect acknowledgement notification
--
--    Peer     - The MQTT connection object
--    Response - The error
--
-- This  procedure is called when  the server  rejects  connection.  The
-- default implementation does nothing.
--
   procedure On_Connect_Rejected
             (  Peer     : in out MQTT_Peer;
                Response : Connect_Response
             );
--
-- On_Disconnect -- Disconnect notification from the client
--
--    Peer - The MQTT connection object
--
-- When received,  the last will message  is not published.  The default
-- implementation does nothing.
--
   procedure On_Disconnect (Peer : in out MQTT_Peer);
--
-- On_Ping -- Ping request notification
--
--    Peer - The MQTT connection object
--
-- The default implementation responds with a ping reponse.
--
   procedure On_Ping (Peer : in out MQTT_Peer);
--
-- On_Ping_Response -- Ping response notification
--
--    Peer - The MQTT connection object
--
-- The default implementation does nothing.
--
   procedure On_Ping_Response (Peer : in out MQTT_Peer);
--
-- On_Publish -- Incoming message
--
--    Peer      - The MQTT connection object
--    Topic     - The message token
--    Message   - The message
--    Packet    - The packet identifier and QoS
--    Duplicate - True if this is another attempt to publish
--    Retain    - True if the server must retain the message
--
-- The default implementation  confirms delivery for the packages of the
-- QoS level is higher  than 0 (At_Most_Once).  When overridden  the new
-- implementation should call the parent's one.
--
   procedure On_Publish
             (  Peer      : in out MQTT_Peer;
                Topic     : String;
                Message   : Stream_Element_Array;
                Packet    : Packet_Identification;
                Duplicate : Boolean;
                Retain    : Boolean
             );
--
-- On_Subscribe -- Subscription request
--
--    Peer          - The MQTT connection object
--    Packet        - The packet identifier
--    Topics_Number - Number of topics
--
-- The parameter Topics_Number specifies the number of topics requested.
-- The topic and its requested QoS  can be obtained using  Get_Topic and
-- Get_QoS calls. The default implementation rejects all topics.
--
   procedure On_Subscribe
             (  Peer          : in out MQTT_Peer;
                Packet        : Packet_Identifier;
                Topics_Number : Positive
             );
--
-- On_Subscribe_Acknowledgement -- Confirmation of subscription
--
--    Peer   - The MQTT connection object
--    Packet - The subscription request packet identifier
--    Codes  - The list of confirmed QoS of subscribed topics
--
-- The default implementation does nothing.
--
   procedure On_Subscribe_Acknowledgement
             (  Peer   : in out MQTT_Peer;
                Packet : Packet_Identifier;
                Codes  : Return_Code_List
             );
--
-- On_Unsubscribe -- Unsubscription acknowledgement
--
--    Peer          - The MQTT connection object
--    Packet        - The packet identifier
--    Topics_Number - Number of topics
--
-- The parameter Topics_Number specifies the number of topics requested.
-- The topic can be obtained using Get_Topic. The default implementation
-- acknowledges the request.
--
   procedure On_Unsubscribe
             (  Peer          : in out MQTT_Peer;
                Packet        : Packet_Identifier;
                Topics_Number : Positive
             );
--
-- Trace -- Higher level tracing
--
--    Peer    - The MQTT connection object
--    Session - The session name
--    Message - The message
--    Kind_Of - The type of the message
--
   type Trace_Message_Type is (Received, Sent, Action);
   procedure Trace
             (  Peer    : in out MQTT_Peer;
                Session : String;
                Message : String;
                Kind_Of : Trace_Message_Type
             );
private
   Max_Message_Size : constant := 1024; -- Default

   type MQTT_State is
        (  MQTT_Header,
           MQTT_Length,
           MQTT_Packet_ID_MSB,
           MQTT_Packet_ID_LSB,
           MQTT_Connect_Version,
           MQTT_Connect_Flags,
           MQTT_Connect_Duration_MSB,
           MQTT_Connect_Duration_LSB,
           MQTT_Connect_Acknowledge_Flags,
           MQTT_Connect_Return,
           MQTT_String_Length_MSB,
           MQTT_String_Length_LSB,
           MQTT_String_Body,
           MQTT_QoS,
           MQTT_Return_Code,
           MQTT_Data
        );
   type MQTT_String (Size : Stream_Element_Count) is record
      Length  : Stream_Element_Count;
      QoS     : QoS_Level;
      Failure : Boolean;
      Data    : Stream_Element_Array (1..Size);
   end record;
   type MQTT_String_Ptr is access MQTT_String;
   type MQTT_String_Ptr_Array is
      array (Positive range <>) of MQTT_String_Ptr;

   type Output_Buffer (Size : Stream_Element_Count) is record
      First : Stream_Element_Count := 1;
      Last  : Stream_Element_Count := 0;
      Data  : Stream_Element_Array (1..Size);
   end record;
   type Output_Buffer_Ptr is access Output_Buffer;

   package MQTT_String_Cache is
      new Generic_Unbounded_Ptr_Array
          (  Index_Type            => Positive,
             Object_Type           => MQTT_String,
             Object_Ptr_Type       => MQTT_String_Ptr,
             Object_Ptr_Array_Type => MQTT_String_Ptr_Array,
             Minimal_Size          => 8,
             Increment             => 8
          );
   use MQTT_String_Cache;
   type Stream_Element_Array_Ptr is access Stream_Element_Array;
   type MQTT_Peer
        (  Listener             : access Connections_Server'Class;
           Max_Subscribe_Topics : Positive;
           Input_Size           : Buffer_Length;
           Output_Size          : Buffer_Length
        )  is new Connection
                  (  Input_Size  => Input_Size,
                     Output_Size => Output_Size
                  )  with
   record
      State       : MQTT_State := MQTT_Header;
      Count       : Stream_Element_Count := 0;
      Length      : Stream_Element_Count := 0;
      Max_Size    : Stream_Element_Count := 0;
      Header      : Stream_Element       := 0;
      Version     : Stream_Element;
      Flags       : Stream_Element;
      Keep_Alive  : Duration;
      QoS         : QoS_Level;
      Packet_ID   : Packet_Identifier;
      List_Length : Natural := 0;
      Secondary   : Output_Buffer_Ptr;
      List        : Unbounded_Ptr_Array;
      Data        : Stream_Element_Array_Ptr :=
                       new Stream_Element_Array (1..Max_Message_Size);
   end record;

   function Get_Message
            (  Peer  : MQTT_Peer;
               Index : Positive
            )  return Stream_Element_Array;
   function Get_String
            (  Peer  : MQTT_Peer;
               Index : Positive
            )  return String;
   function Get_Length
            (  Value : Stream_Element_Count
            )  return Stream_Element_Count;
   function Get_Size
            (  Length : Stream_Element_Count
            )  return Stream_Element_Count;
   procedure Put_Length
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Stream_Element_Count
             );
   procedure Put_String
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             );
   procedure Put_String
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Stream_Element_Array
             );
   procedure Reset_String
             (  Peer  : in out MQTT_Peer;
                Index : Positive
             );
   procedure Send
             (  Peer : in out MQTT_Peer;
                Data : Stream_Element_Array
             );

   function String_Name
            (  Header : Stream_Element;
               Index  : Positive
            )  return String;

   type Topic_Item_Data (Length : Natural) is
      new Object.Entity with
   record
      Topic : String (1..Length);
   end record;
   type Topic_Item_Data_Ptr is access Topic_Item_Data'Class;

   package Topic_Item_Handles is
      new Object.Handle (Topic_Item_Data, Topic_Item_Data_Ptr);
   use Topic_Item_Handles;

   type Topic_Item is new Topic_Item_Handles.Handle with null record;
   function Ref (Thing : Topic_Item_Data_Ptr) return Topic_Item;

   type Topics_List is array (Positive range <>) of Topic_Item;

   type Message_Object
        (  Length : Positive;
           Size   : Stream_Element_Count
        )  is new Object.Entity with
   record
      Count   : Stream_Element_Count := 0;
      Topic   : String (1..Length);
      Content : Stream_Element_Array (1..Size);
   end record;
   type Message_Object_Ptr is access all Message_Object'Class;

   package Messages_Handles is
      new Object.Handle (Message_Object, Message_Object_Ptr);
   use Messages_Handles;

   procedure Release (Ptr : Message_Object_Ptr);
   procedure Set_Size
             (  Handle : in out Messages_Handles.Handle;
                Size   : Stream_Element_Count
             );

   type MQTT_Message is tagged record
      Reference : Messages_Handles.Handle;
   end record;

   pragma Inline (Get_Message);
   pragma Inline (Get_Topic);
   pragma Inline (Release);

end GNAT.Sockets.MQTT;
