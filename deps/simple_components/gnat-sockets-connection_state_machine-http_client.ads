--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     HTTP_Client                                 Spring, 2015       --
--  Interface                                                         --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Calendar;    use Ada.Calendar;
with Ada.Exceptions;  use Ada.Exceptions;

with Ada.Finalization;
with GNAT.Sockets.Connection_State_Machine.Expected_Sequence;
with GNAT.Sockets.Connection_State_Machine.HTTP_Server;
with GNAT.Sockets.Connection_State_Machine.Terminated_Strings;

with Stack_Storage;
with Tables.Names;

package GNAT.Sockets.Connection_State_Machine.HTTP_Client is

   Post_MIME : constant String := "application/x-www-form-urlencoded";
   Put_MIME  : constant String := "text/html; charset=utf-8";
--
-- Response_Header -- Response header fields
--
   type Response_Header is
        (  Access_Control_Allow_Origin_Header,
           Accept_Patch_Header,
           Accept_Ranges_Header,
           Age_Header,
           Allow_Header,
           Cache_Control_Header,
           Content_Disposition_Header,
           Content_Encoding_Header,
           Content_Language_Header,
           Content_Location_Header,
           Content_MD5_Header,
           Content_Range_Header,
           Content_Type_Header,
           ETag_Header,
           Link_Header,
           Location_Header,
           P3P_Header,
           Pragma_Header,
           Proxy_Authenticate_Header,
           Public_Key_Pins_Header,
           Refresh_Header,
           Permanent_Header,
           Server_Header,
           Set_Cookie_Header,
           Status_Header,
           Strict_Transport_Security_Header,
           Trailer_Header,
           Transfer_Encoding_Header,
           Upgrade_Header,
           Vary_Header,
           Via_Header,
           Warning_Header,
           WWW_Authenticate_Header,
           X_Frame_Options_Header,

           Content_Length_Header,
           Connection_Header,

           Date_Header,
           Expires_Header,
           Last_Modified_Header,
           Retry_After_Header
        );
--
-- Image -- Header name
--
--    Header - The header type
--
-- Returns :
--
--    The corresponding name, e.g. Keep-Alive
--
   function Image (Header : Response_Header) return String;
--
-- Header_Value -- String to header type conversion
--
--    Text - The header name (case-insensitive)
--
-- Returns :
--
--    The header type
--
   type Header_Value (None : Boolean := True) is record
      case None is
         when True =>
            null;
         when False =>
            Header : Response_Header;
      end case;
   end record;
   function Value (Text : String) return Header_Value;

   subtype Text_Header is Response_Header
      range Access_Control_Allow_Origin_Header..X_Frame_Options_Header;
--
-- HTTP_Session -- An  object  implementing  HTTP 1.1  connection  to  a
--                 server
--
--    Listener        - The connections server object
--    Response_Length - The maximum length of one response header line
--    Input_Size      - The input buffer size
--    Output_Size     - The output buffer size
--
-- The output  buffer  size should  be large  enough  to accommodate all
-- headers  of a response  and its status line and the body when that is
-- sent as a single string.  The response body sent from  a stream  or a
-- content source  using chunked transfer are  not bound  by t he output
-- buffer size.
--
   type HTTP_Session
        (  Listener        : access Connections_Server'Class;
           Response_Length : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length
        )  is new State_Machine with private;
--
-- Connect_Parameters_Set -- Overriding GNAT.Sockets.Server...
--
   procedure Connect_Parameters_Set
             (  Session        : in out HTTP_Session;
                Host           : String;
                Address        : Sock_Addr_Type;
                Max_Connect_No : Positive
             );
--
-- Connected -- Notification
--
--    Session - The connection object
--
-- This  procedure  shall  be called  from  the  new  implementation  if
-- overridden by the derived type.
--
   procedure Connected (Session : in out HTTP_Session);
--
-- Completed -- Method completion callback
--
--    Session   - The connection object
--    Method    - The method completed
--    Status    - The status code
--  [ Message ] - Received from the server
--
-- This procedure is called upon completion of a method processing.  The
-- default  implementations  do  nothing.  The  variant  without Message
-- parameter is used when there is no message  or else  when the message
-- was received but ignored.  The former happens for HEAD method or upon
-- server responses that do no have message  - all  1xx (informational),
-- 204  (no content),  and 304  (not modified)  responses.   The  latter
-- happens when part of the message was ignored due to store errors.  In
-- this case Message_Store_Error is always called prior to Complete.
--
   procedure Completed
             (  Session : in out HTTP_Session;
                Method  : HTTP_Server.HTTP_Method;
                Status  : Positive
             );
   procedure Completed
             (  Session : in out HTTP_Session;
                Method  : HTTP_Server.HTTP_Method;
                Status  : Positive;
                Message : in out Root_Stream_Type'Class
             );
   procedure Completed
             (  Session : in out HTTP_Session;
                Method  : HTTP_Server.HTTP_Method;
                Status  : Positive;
                Message : in out HTTP_Server.Content_Destination'Class
             );
--
-- Delete -- Query the server using the DELETE method
--
--    Session   - The connection object
--    Name      - The page name to delete
--  [ Message ] - Where to store response's message
--
-- Exceptions :
--
--    Use_Error - Not connected or else another request is pending
--
   procedure Delete
             (  Session : in out HTTP_Session;
                Name    : String
             );
   procedure Delete
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access Root_Stream_Type'Class
             );
   procedure Delete
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access HTTP_Server.Content_Destination'Class
             );
--
-- Finalize -- Destruction
--
--    Session - The connection object
--
-- This  procedure  shall  be called  from  the  new  implementation  if
-- overridden by the derived type.
--
   procedure Finalize (Session : in out HTTP_Session);
--
-- Get -- Query the server using the GET method
--
--    Session - The connection object
--    Name    - The page name to get
--    Message - Where to store response's message
--  [ From    - The first byte of requested content 0..
--    To ]    - The last byte of requested content From..
--
-- Exceptions :
--
--    Use_Error - Not connected or else another request is pending
--
   procedure Get
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access Root_Stream_Type'Class;
                From    : Stream_Element_Count := 1;
                To      : Stream_Element_Count := 0
             );
   procedure Get
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access HTTP_Server.Content_Destination'Class;
                From    : Stream_Element_Count := 1;
                To      : Stream_Element_Count := 0
             );
--
-- Get_Keep_Alive -- Get connection keep-alive
--
--    Session - The connection object
--
-- Returns :
--
--    True if connection header is "keep-alive", otherwise it is "close"
--
   function Get_Keep_Alive (Session : HTTP_Session) return Boolean;
--
-- Get_Request_Header -- The request header specified as a text
--
--    Session - The connection object
--    Header  - Request header
--
-- Returns :
--
--    The header value used in requests or empty string
--
   function Get_Request_Header
            (  Session : HTTP_Session;
               Header  : HTTP_Server.Text_Header
            )  return String;
--
-- Get_Request_Method -- The last request method
--
--    Session - The connection object
--
-- Returns :
--
--    The method used in the last request
--
   function Get_Request_Method
            (  Session : HTTP_Session
            )  return HTTP_Server.HTTP_Method;
--
-- Get_Request_Range -- Get content range header
--
--    Session - The connection object
--    From    - The first byte of requested content 0..
--    To      - The last byte of requested content From..
--
-- When From > To, no range header is used in the last request.
--
   procedure Get_Request_Range
             (  Session : HTTP_Session;
                From    : out Stream_Element_Count;
                To      : out Stream_Element_Count
             );
--
-- Get_Response_Code -- The response status code
--
--    Session - The connection object
--
-- Returns :
--
--    The status code as returned with the last response
--
   function Get_Response_Code (Session : HTTP_Session) return Positive;
--
-- Get_Response_Connection_Flags -- The response connection flags
--
--    Session - The connection object
--
-- Note that when the server returns Connection: close, the client drops
-- the session.  The client must  explicitly connect to the server again
-- by calling Connect.
--
-- Returns :
--
--    The connection header as returned with the last response
--
   function Get_Response_Connection_Flags
            (  Session : HTTP_Session
            )  return HTTP_Server.Connection_Flags;
--
-- Get_Response_Header -- The response header specified as a text
--
--    Session - The connection object
--    Header  - Response header
--
-- Returns :
--
--    The header value used in requests or empty string
--
   function Get_Response_Header
            (  Session : HTTP_Session;
               Header  : Text_Header
            )  return String;
--
-- Get_Response_Reason -- The response reason text
--
--    Session - The connection object
--
-- Returns :
--
--    The reason text as returned with the last response
--
   function Get_Response_Reason (Session : HTTP_Session) return String;
--
-- Get_Response_Version -- The server's version
--
--    Session  - The connection object
--
-- Returns :
--
--    Server's HTTP version as returned in the last response
--
   function Get_Response_Version (Session : HTTP_Session)
      return HTTP_Server.HTTP_Version;
--
-- Head -- Query the server using the HEAD method
--
--    Session - The connection object
--    Name    - The page name to get
--  [ From    - The first byte of requested content 0..
--    To ]    - The last byte of requested content From..
--
-- Exceptions :
--
--    Use_Error - Not connected or else another request is pending
--
   procedure Head
             (  Session : in out HTTP_Session;
                Name    : String;
                From    : Stream_Element_Count := 1;
                To      : Stream_Element_Count := 0
             );
--
-- Initialize -- Construction
--
--    Session - The connection object
--
-- This  procedure  shall  be called  from  the  new  implementation  if
-- overridden by the derived type.
--
   procedure Initialize (Session : in out HTTP_Session);
--
-- Is_Active -- Check session state
--
--    Session - The connection object
--
-- Returns :
--
--    True if there is a request pending
--
   function Is_Active (Session : HTTP_Session) return Boolean;
--
-- Message_Store_Error -- Called on data receipt error
--
--    Session          - The HTTP connection object
--    Stream / Content - Used during transfer
--    Error            - The error occurence
--
-- This procedure is called  on  an error  occurred  during  storing the
-- response  data.  The  method caused  the error can be  obtained using
-- Get_Request_Method.  The  default  implementations  raise  Data_Error
-- exception.
--
   procedure Message_Store_Error
             (  Session : in out HTTP_Session;
                Stream  : in out Root_Stream_Type'Class;
                Error   : Exception_Occurrence
             );
   procedure Message_Store_Error
             (  Session : in out HTTP_Session;
                Content : in out HTTP_Server.Content_Destination'Class;
                Error   : Exception_Occurrence
             );
--
-- Options -- Query the server using the OPTIONS method
--
--    Session   - The connection object
--    Name      - The page name to get
--  [ Message ] - Where to store response's message
--
-- Exceptions :
--
--    Use_Error - Not connected or else another request is pending
--
   procedure Options
             (  Session : in out HTTP_Session;
                Name    : String
             );
   procedure Options
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access Root_Stream_Type'Class
             );
   procedure Options
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access HTTP_Server.Content_Destination'Class
             );
--
-- Post -- Query the server using the POST method
--
--    Session   - The connection object
--    Name      - The page name to get
--    Content   - To send
--  [ Length  ] - The number of bytes in the content
--  [ Message ] - Where to store response's message
--  [ MIME    ] - Content type
--
-- Exceptions :
--
--    Use_Error - Not connected or else another request is pending
--
   procedure Post -- String content
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             );

   procedure Post -- Stream element array content
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             );

   procedure Post -- Stream content chunked
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             );

   procedure Post -- Stream content fixed length
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             );

   procedure Post -- Custom content, chunked
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             );

   procedure Post -- Custom content, fixed length
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             );
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             );
--
-- Put -- Query the server using the PUT method
--
--    Session   - The connection object
--    Name      - The page name to get
--    Content   - To send
--  [ Length  ] - The number of bytes in the content
--  [ Message ] - Where to store response's message
--  [ MIME    ] - Content type
--
-- Exceptions :
--
--    Use_Error - Not connected or else another request is pending
--
   procedure Put -- String content
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             );

   procedure Put -- Stream element array content
             (  Session   : in out HTTP_Session;
                Name      : String;
                Content   : Stream_Element_Array;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             );

   procedure Put -- Stream content chunked
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             );

   procedure Put -- Stream content fixed length
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             );

   procedure Put -- Custom content, chunked
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             );

   procedure Put -- Custom content, fixed length
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             );
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             );
--
-- Received -- Overrides GNAT.Sockets.Server...
--
   procedure Received
             (  Session  : in out HTTP_Session;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Receive_Header_Tracing -- Enable tracing
--
--    Session - The HTTP connection object
--    Enable  - Trace receiving response header
--
   procedure Receive_Header_Tracing
             (  Session : in out HTTP_Session;
                Enable  : Boolean
             );
--
-- Receive_Message_Tracing -- Enable tracing
--
--    Session - The HTTP connection object
--    Enable  - Trace receiving response data
--
   procedure Receive_Message_Tracing
             (  Session : in out HTTP_Session;
                Enable : Boolean
             );
--
-- Sent -- Overrides GNAT.Sockets.Server...
--
   procedure Sent (Session : in out HTTP_Session);
--
-- Set_Keep_Alive -- Set keep-alive
--
--    Session - The connection object
--    Enable  - The keep-alive mode
--
-- Normally  when  a query  is completed the client disconnects from the
-- server.  The connection object is collected. This behavior is matched
-- by the request header 'Connection' set to "close".  Alternatively the
-- client may stay  connected in which case the header is set to  "keep-
-- alive". This procedure is used to change the behavior.
--
   procedure Set_Keep_Alive
             (  Session : in out HTTP_Session;
                Enable  : Boolean
             );
--
-- Set_Request_Date -- Enable date in request header
--
--    Session - The connection object
--    Enable  - True if the header should be used
--
   procedure Set_Request_Date
             (  Session : in out HTTP_Session;
                Enable  : Boolean
             );
--
-- Set_Request_Header -- Set request header specified as a text
--
--    Session - The connection object
--    Header  - Request header
--    Value   - The value to set
--
-- This procedure  sets the header  value to be used in a request.  When
-- the value is empty string the header is not sent.
--
   procedure Set_Request_Header
             (  Session : in out HTTP_Session;
                Header  : HTTP_Server.Text_Header;
                Value   : String := ""
             );
--
-- Set_Request_If_Modified_Since -- Enable a request header
--
--    Session - The connection object
--  [ Date ]  - To use in if-modified-since header
--
-- When Date is omitted no if-modified-since header is used.
--
-- Exceptions :
--
--    Time_Error - Invalid date
--
   procedure Set_Request_If_Modified_Since
             (  Session : in out HTTP_Session;
                Date    : Time
             );
   procedure Set_Request_If_Modified_Since
             (  Session : in out HTTP_Session
             );
--
-- Set_Request_If_Unmodified_Since -- Enable a request header
--
--    Session - The connection object
--  [ Date ]  - To use in if-unmodified-since header
--
-- When Date is omitted no if-unmodified-since header is used.
--
-- Exceptions :
--
--    Time_Error - Invalid date
--
   procedure Set_Request_If_Unmodified_Since
             (  Session : in out HTTP_Session;
                Date    : Time
             );
   procedure Set_Request_If_Unmodified_Since
             (  Session : in out HTTP_Session
             );
--
-- Trace -- Tracing
--
--    Session - The connection object
--    Message - To write into the trace
--
   procedure Trace
             (  Session : in out HTTP_Session;
                Message : String
             );

private
   use GNAT.Sockets.Connection_State_Machine.Expected_Sequence;
   use GNAT.Sockets.Connection_State_Machine.HTTP_Server;
   use GNAT.Sockets.Connection_State_Machine.Terminated_Strings;

   type Stream_Ptr is access all Root_Stream_Type'Class;
   type Content_Source_Ptr is access all Content_Source'Class;
   type Content_Destination_Ptr is access all Content_Destination'Class;

   type Action is access
      procedure (Session : in out HTTP_Session'Class);

--     type Data_Pool is new Stack_Storage.Pool with null record;
--     procedure Write
--               (  Stream : access Root_Stream_Type'Class;
--                  Item   : Data_Pool
--               );
--     for Data_Pool'Write use Write;

   type String_Data (Size : Natural) is record
      Length : Natural := 0;
      Text   : String (1..Size);
   end record;
   type String_Data_Ptr is access String_Data;
   type Text_Data_Ptr is access constant String_Data;
   type Stream_Data_Ptr is access constant Stream_Element_Array;
   function Get (Data : String_Data_Ptr) return String;
   procedure Set (Data : in out String_Data_Ptr; Text : String);
   type Request_Header_Array is
      array (Request_Header) of String_Data_Ptr;

   type Slice is record
      Length : Stream_Element_Offset;
      Text   : HTTP_Server.String_Ptr;
   end record;
   type Slice_Ptr is access constant Slice;

   type Send_Type is
        (  Sending_None,
           Sending_String,
           Sending_Literal_String,
           Sending_Literal_Stream,
           Sending_Headers,
           Sending_Chain,
           Sending_Stream,
           Sending_Chunked_Stream,
           Sending_Content,
           Sending_Chunked_Content,
           Sending_Slice,
           Sending_Clean_And_Jump
        );
   type Send_Data;
   type Send_Data_Ptr is access Send_Data;
   type Send_Data (Kind_Of : Send_Type := Sending_None) is record
      Count : Stream_Element_Offset;
      case Kind_Of is
         when Sending_None =>
            null;
         when Sending_String =>
            Data : Text_Data_Ptr;
         when Sending_Literal_String =>
            Text : HTTP_Server.String_Ptr;
         when Sending_Literal_Stream =>
            Stream : Stream_Data_Ptr;
         when Sending_Headers =>
            Header : Natural;
         when Sending_Chain =>
            Chain : Action;
         when Sending_Stream =>
            Stream_Source : Stream_Ptr;
         when Sending_Chunked_Stream =>
            Chunked_Stream_Source : Stream_Ptr;
         when Sending_Content =>
            Content_Source : Content_Source_Ptr;
         when Sending_Chunked_Content =>
            Chunked_Content_Source : Content_Source_Ptr;
         when Sending_Slice =>
            Slice : Slice_Ptr;
         when Sending_Clean_And_Jump =>
            Stub : HTTP_Server.String_Ptr;
      end case;
   end record;
   type Send_Data_Array is array (Positive range <>) of Send_Data;
   package Send_Data_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Send_Data,
             Object_Array_Type => Send_Data_Array,
             Null_Element      => (Sending_None, 0)
          );
   type Send_Data_List is
      new Send_Data_Arrays.Unbounded_Array with null record;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Send_Data_List
             );
   for Send_Data_List'Write use Write;

   type Data_Pool is new Stack_Storage.Pool with null record;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Pool
             );
   for Data_Pool'Write use Write;

   type Text_Ptr is access constant String;
   type Response_Header_Array is array (Text_Header) of Text_Ptr;
--
-- Request_Line_Type -- Types of request lines/processing states
--
   type Response_Line_Type is
        (  Nothing,         -- No request active
           Response_Line,   -- Response status line
           Header_Line,     -- Response header field line
           Response_Data,   -- Response data
           Chunk_Header,    -- Chunk header
           Chunk_Data,      -- Chunk data
           Chunk_Data_CRLF, -- CRLF
           Chunk_Footer     -- Chunk footer
        );

   subtype Specific_Header is Response_Header
      range Date_Header..Retry_After_Header;
   type Specific_Header_Array is array (Specific_Header) of Boolean;

   type HTTP_Session
        (  Listener        : access Connections_Server'Class;
           Response_Length : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length
        )  is new State_Machine (Input_Size, Output_Size) with
   record
      Expecting        : Response_Line_Type   := Nothing;
      Data_Length      : Stream_Element_Count := 0;
      Version          : HTTP_Version         := 1.1;
      Code             : Positive             := 200;
      Method           : HTTP_Method          := HTTP_GET;
      Request_Headers  : Request_Header_Array;
      Response_Headers : Response_Header_Array;
      Specific         : Specific_Header_Array; -- Specific headers
      Trace_Header     : Boolean := False;
      Trace_Message    : Boolean := False;
      Keep_Alive       : Boolean := False;
      Send_Date        : Boolean := True;
      Connection       : Connection_Flags := Connection_Close;
      If_Modified      : String_Data_Ptr;
      If_Unmodified    : String_Data_Ptr;
      Reason           : Text_Ptr;
      Page_Name        : String_Data_Ptr;
      Range_From       : Stream_Element_Count := 1;
      Range_To         : Stream_Element_Count := 0;
         -- Data
      Stream           : Stream_Ptr;
      Destination      : Content_Destination_Ptr;
         -- Strings to send
      Send_List        : Send_Data_List;
      Send_Index       : Integer := 1;
      Send_Length      : Natural := 0;
         -- Time header fields
      Date             : Time := Clock;
      Expires          : Time := Clock;
      Last_Modified    : Time := Clock;
      Retry_After      : Time := Clock;
         -- Parsed line, CR LF terminated
      Line             : Dynamic_String_Data_Item;
      LF               : Expected_Item (1);

      Pool             : Data_Pool (0, 4);
      pragma Atomic (Expecting);
   end record;

   function Get
            (  Session : HTTP_Session;
               Header  : Request_Header
            )  return String;
   function Get_Session_State (Session : HTTP_Session)
      return Session_State;
   procedure End_Of_Query (Session : in out HTTP_Session);

   procedure Queue_Action
             (  Session : in out HTTP_Session'Class;
                Chain   : Action
             );
   procedure Queue_Headers (Session : in out HTTP_Session'Class);
   procedure Queue_None (Session : in out HTTP_Session'Class);
   procedure Queue_String
             (  Session : in out HTTP_Session'Class;
                Text    : Text_Data_Ptr
             );
   procedure Queue_String
             (  Session : in out HTTP_Session'Class;
                Text    : String
             );
   procedure Queue_String
             (  Session : in out HTTP_Session'Class;
                Text    : Stream_Element_Array
             );
   procedure Send
             (  Session : in out HTTP_Session'Class;
                Message : String
             );
   procedure Send
             (  Session : in out HTTP_Session'Class;
                Message : Stream_Element_Array
             );
--
-- Set_Request_Range -- Set content range header
--
--    Session - The connection object
--    From    - The first byte of requested content 0..
--    To      - The last byte of requested content From..
--
-- When From > To, no range header is used in the requests.
--
   procedure Set_Request_Range
             (  Session : in out HTTP_Session;
                From    : Stream_Element_Count := 1;
                To      : Stream_Element_Count := 0
             );

   procedure Check_Spelling (Name : String);
   function Check_Matched
            (  Source  : String;
               Pointer : Integer
            )  return Boolean;

   package Response_Header_Tables_Raw is new Tables (Response_Header);
   package Response_Header_Tables is
      new Response_Header_Tables_Raw.Names;
   use Response_Header_Tables;

   procedure Skip_Blanks (Source : String; Pointer : in out Integer);

   Response_Headers : Response_Header_Tables.Dictionary;

end GNAT.Sockets.Connection_State_Machine.HTTP_Client;
