--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.SMTP.Client                    Luebeck            --
--  Interface                                      Summer, 2016       --
--                                                                    --
--                                Last revision :  17:51 21 Jun 2016  --
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

with Ada.Calendar;                   use Ada.Calendar;
with Ada.Streams.Stream_IO;          use Ada.Streams;
with GNAT.Sockets.Server;            use GNAT.Sockets.Server;
with Strings_Edit.Time_Conversions;  use Strings_Edit.Time_Conversions;

with Ada.Finalization;
with Object.Handle;
with Persistent;
with Tables.Names;

package GNAT.Sockets.SMTP.Client is
--
-- Mail_Header -- Headers of an E-mail
--
   type Mail_Header is
        (  Mail_From,
           Mail_Message_ID,
           Mail_In_Reply_To,
           Mail_Subject,
           Mail_MIME_Version,
           Mail_Content_Type,
           Mail_Precedence,
           Mail_References,
           Mail_Reply_To,
           Mail_Sender,
           Mail_Archived_At,

           Mail_To,
           Mail_Bcc,
           Mail_Cc,

           Mail_Date,

           Mail_Body
        );
   function Image (Header : Mail_Header) return String;
   subtype Text_Header is Mail_Header range Mail_From..Mail_Archived_At;
   subtype List_Header is Mail_Header range Mail_To..Mail_Cc;
   subtype Date_Header is Mail_Header range Mail_Date..Mail_Date;

   type Mail_Status is
        (  Mail_Pending,
           Mail_Sent,
           Mail_Rejected
        );
------------------------------------------------------------------------
-- Mail_Address_List -- A list of addresses
--
   type Mail_Address_List is tagged private;
--
-- Add_Address -- Add an E-mail address to the list
--
--    List    - The address list
--    Address - The E-mail address to add
--
-- Nothing happens if Address is already in the list or Address is empty
--
   procedure Add_Address
             (  List    : in out Mail_Address_List;
                Address : String
             );
--
-- Empty -- Mail address list
--
-- Returns :
--
--    An empty list
--
   function Empty return Mail_Address_List;
--
-- Find -- Get element index
--
--    List    - The address list
--    Address - The E-mail address
--
-- Returns :
--
--    The index of the address in the list or 0
--
   function Find
            (  List    : Mail_Address_List;
               Address : String
            )  return Natural;
--
-- From_String -- Convert a comma separated list to an object
--
--    List - The list of addresses separated by comma
--
-- Returns :
--
--    The list object
--
   function From_String (List : String) return Mail_Address_List;
--
-- Get_Address -- Get list item
--
--    List  - The address list
--    Index - The list element 1..
--
-- Returns :
--
--    The list element by its position
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Get_Address
            (  List  : Mail_Address_List;
               Index : Positive
            )  return String;
--
-- Get_Length -- Get list length
--
--    List - The address list
--
-- Returns :
--
--    The number of addresses in the list
--
   function Get_Length (List : Mail_Address_List) return Natural;
--
-- Image -- Get comma separated list of addresses
--
--    List - The address list
--
-- Returns :
--
--    The comma separated list of addresses
--
   function Image (List  : Mail_Address_List) return String;
--
-- Is_Empty -- Check if address list is empty
--
--    List - The address list
--
-- Returns :
--
--    True if the list is empty
--
   function Is_Empty (List : Mail_Address_List) return Boolean;
--
-- Is_In -- Check if address in the list
--
--    List    - The address list
--    Address - The E-mail address
--
-- Returns :
--
--    True if Address is in the list
--
   function Is_In
            (  List    : Mail_Address_List;
               Address : String
            )  return Boolean;
--
-- Remove -- Remove E-mail address from the list
--
--    List    - The address list
--    Address - The E-mail address to add
--
-- Nothing happens if Address is not in the list
--
   procedure Remove
             (  List    : in out Mail_Address_List;
                Address : String
             );
--
-- Remove -- Remove E-mail address from the list by index
--
--    List  - The address list
--    Index - In the list
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Remove
             (  List  : in out Mail_Address_List;
                Index : Positive
             );
--
-- / -- Concatenation
--
   function "/" (Left, Right : String) return Mail_Address_List;
   function "/"
             (  List    : Mail_Address_List;
                Address : String
             )  return Mail_Address_List;
--
-- and, or, xor -- Set theoretic operations
--
   function "and" (Left, Right : Mail_Address_List)
      return Mail_Address_List;
   function "or" (Left, Right : Mail_Address_List)
      return Mail_Address_List;
   function "xor" (Left, Right : Mail_Address_List)
      return Mail_Address_List;
------------------------------------------------------------------------
-- Mail -- An object containing E-mail
--
-- The mail object has referential semantics. Assignment does not create
-- a new copy, it is shallow. The  application  should  avoid concurrent
-- access to the object.  The resources  allocated  for  the  object are
-- freed automaticaly.
--
   type Mail is private;
   type Mail_Array is array (Positive range <>) of Mail;
--
-- Attach_{File|Stream|String} -- Add an attachment to the mail
--
--    Message      - The object
--    Contents     - The text, stream or else file name
--    Content_Type - The type of content
--    Description  - The content disposition
--    Disposition  - The content disposition
--
-- When Stream is attached,  there are two variants.  The variant  using
-- access type onws the object and frees it when no more used.  When the
-- object is passed it must exist as long as the mail object does.
--
-- Exceptions :
--
--    Errors opening the file
--
   procedure Attach_File
             (  Message      : in out Mail;
                Contents     : String;
                Content_Type : String := "text/plain";
                Disposition  : String := "";
                Description  : String := ""
             );
   procedure Attach_Stream
             (  Message      : in out Mail;
                Contents     : access Root_Stream_Type'Class;
                Content_Type : String := "text/plain";
                Disposition  : String := "";
                Description  : String := ""
             );
   procedure Attach_Stream
             (  Message      : in out Mail;
                Contents     : in out Root_Stream_Type'Class;
                Content_Type : String := "text/plain";
                Disposition  : String := "";
                Description  : String := ""
             );
   procedure Attach_String
             (  Message      : in out Mail;
                Contents     : String;
                Content_Type : String := "text/plain";
                Disposition  : String := "";
                Description  : String := ""
             );
--
-- Check - E-mail consistency
--
--    Message - The object to check
--
-- Exceptions :
--
--    Use_Error - Invalid mail
--
   procedure Check (Message : Mail);
--
-- Create -- Create an new E-mail object
--
--    From       - The sender's E-mail address
--    Subject    - Of the message
--    To         - The recipient address
--  [ Contents ] - The mail body
--    Cc         - The recipient address list
--    Bcc        - The recipient address list
--    Date       - Of the message
--  [ MIME ]     - Force MIME even if has no attachments
--
-- When  the  mail body is a string it is copied and kept until the mail
-- is  sent.  When  the mail body is a pointer to the stream, the target
-- must exists  until  sending  or  rejecting  mail  is  confirmed.  The
-- parameter MIME when not empty forces  MIME  multipart  body  even  if
-- there is no attachments in the mail. In this case MIME is the content
-- type, e.g.  text/plain.  When  Stream  is  attached,  there  are  two
-- variants.  The variant using access type onws the object and frees it
-- when no more used. When the object is passed it must exist as long as
-- the mail object does.
--
   function Create
            (  From     : String;
               Subject  : String;
               To       : Mail_Address_List'Class;
               Contents : String;
               Cc       : Mail_Address_List'Class := Empty;
               Bcc      : Mail_Address_List'Class := Empty;
               Date     : Time   := Clock;
               MIME     : String := "text/plain"
            )  return Mail;
   function Create
            (  From     : String;
               Subject  : String;
               To       : String;
               Contents : String;
               Cc       : Mail_Address_List'Class := Empty;
               Bcc      : Mail_Address_List'Class := Empty;
               Date     : Time   := Clock;
               MIME     : String := "text/plain"
            )  return Mail;
   function Create
            (  From     : String;
               Subject  : String;
               To       : Mail_Address_List'Class;
               Cc       : Mail_Address_List'Class := Empty;
               Bcc      : Mail_Address_List'Class := Empty;
               Date     : Time := Clock
            )  return Mail;
   function Create
            (  From     : String;
               Subject  : String;
               To       : String;
               Cc       : Mail_Address_List'Class := Empty;
               Bcc      : Mail_Address_List'Class := Empty;
               Date     : Time := Clock
            )  return Mail;
--
-- Erase -- Clean up an E-mail object
--
--    Message - The object to erase
--
   procedure Erase (Message : in out Mail);
--
-- Get -- E-mail header
--
--    Message - The object
--    Header  - The header to set
--
-- Returns :
--
--    The header value or empty string if not set
--
   function Get
            (  Message : Mail;
               Header  : Text_Header
            )  return String;
   function Get
            (  Message : Mail;
               Header  : List_Header
            )  return Mail_Address_List;
   function Get
            (  Message : Mail;
               Header  : Date_Header
            )  return Time;
--
-- Get_Status -- Mail status
--
--    Message - The object
--
-- Returns :
--
--    The mail status
--
   function Get_Status (Message : Mail) return Mail_Status;
--
-- Set -- E-mail header
--
--    Message            - The object
--    Header             - The header to set
--    Text / List / Date - The header value
--
   procedure Set
             (  Message : in out Mail;
                Header  : Text_Header;
                Text    : String
             );
   procedure Set
             (  Message : in out Mail;
                Header  : List_Header;
                List    : Mail_Address_List'Class
             );
   procedure Set
             (  Message : in out Mail;
                Header  : Date_Header;
                Date    : Time
             );
------------------------------------------------------------------------
-- SMTP_Client -- SMTP session Client
--
--    Listener     - The connections server object
--    Input_Size   - Input buffer size
--    Output_Size  - Output buffer size
--    Reply_Length - The maximum server's reply length
--
-- The reply  length does  not include  the reply  code and  CR LF.  The
-- output buffer must be large enough to contain one request line,  e.g.
-- an E-mail  address or a domain name.  That does  not count  for  mail
-- content, only for requests.
--
   type SMTP_Client
        (  Listener     : access Connections_Server'Class;
           Input_Size   : Buffer_Length;
           Output_Size  : Buffer_Length;
           Reply_Length : Positive
        )  is new Connection with private;
--
-- Completed -- Mail queue empty notification
--
--    Client - The client
--
-- This procedure  is called when all mails are sent.  An implementation
-- may queue  further  mails to send.  The default  implementation  does
-- nothing.
--
   procedure Completed (Client : in out SMTP_Client);
--
-- Connected -- Overrides GNAT.Sockets.Server...
--
   procedure Connected (Client : in out SMTP_Client);
--
-- Disconnected -- Overrides GNAT.Sockets.Server...
--
   procedure Disconnected (Client : in out SMTP_Client);
--
-- Elevated -- Overrides GNAT.Sockets.Server...
--
   procedure Elevated (Client : in out SMTP_Client);
--
-- Finalize -- Overrides GNAT.Sockets.Server...
--
   procedure Finalize (Client : in out SMTP_Client);
--
-- Get_Accepted -- The authenication methods the client can use
--
--    Client - The client
--
-- Returns :
--
--    The methods list
--
   function Get_Accepted
            (  Client : SMTP_Client
            )  return SMTP_AUTH_Mechanism;
--
-- Get_Authentication -- Get authentication methods supported
--
--    Client - The client
--
-- When  called  right  after the server responds to the EHLO request of
-- the   client   returns  the  combination  of  values  indicating  the
-- authentication  methods  supported  by  the   server.   When   called
-- afterwards, it returns the method selected.
--
-- Returns :
--
--    The authentication methods supported by the server
--
   function Get_Authentication
            (  Client : SMTP_Client
            )  return SMTP_AUTH_Mechanism;
--
-- Get_Enhanced -- Get force enhanced mode flag
--
--    Client - The client
--
-- Normally the client uses enhanced mode when the server greets it with
-- a relpy containing "ESMTP" keyword.  When this flag is set the client
-- will use the enhanced mode regardless the greeting.
--
-- Returns :
--
--    True if enhanced mode is forced
--
   function Get_Enhanced (Client : SMTP_Client) return Boolean;
--
-- Get_Extension -- SMTP extension support
--
--    Client    - The client
--    Extension - SMTP_Extension
--
-- When the  client  is  connected  to the  server  the  returned  value
-- indicates if the server  announced the corresponding SMTP enhancement
-- support in the greeting.
--
-- Returns :
--
--    True if enhanced mode is supported
--
   function Get_Extension
            (  Client    : SMTP_Client;
               Extension : SMTP_Extension
            )  return Boolean;
--
-- Get_Mail_Size -- Get maximum mail size
--
--    Client - The client
--
-- This function  when called  after  the server  responds  to  the EHLO
-- request  of the client  returns the  combination of values indicating
-- the authentication methods supported by the server.
--
-- Returns :
--
--    The value returned in the SIZE response
--
   function Get_Mail_Size (Client : SMTP_Client) return Natural;
--
-- Get_Password -- Get user password used to login
--
--    Client - The client
--
-- Returns :
--
--    The password
--
   function Get_Password (Client : SMTP_Client) return String;
--
-- Get_TLS -- Get TLS support
--
--    Client - The client
--
-- Note that TLS is not engaged when the factory does not support it.
--
-- Returns :
--
--    True if TLS offer is accepted
--
   function Get_TLS (Client : SMTP_Client) return Boolean;
--
-- Get_User -- Get user name used to login
--
--    Client - The client
--
-- Returns :
--
--    The user name
--
   function Get_User (Client : SMTP_Client) return String;
--
-- Is_Opportunistic -- Overrides GNAT.Sockets.Server...
--
   function Is_Opportunistic (Client : SMTP_Client) return Boolean;
--
-- Received -- Overrides GNAT.Sockets.Server...
--
   procedure Received
             (  Client  : in out SMTP_Client;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Send -- Send mail
--
--    Client     - The client
--    Message[s] - The message or messages to send
--
-- This procedure initiates sending E-mail to the server. Effectively it
-- queues  the message  to the list of mails to send.  The operation  is
-- completed with a call to Send_Abandoned,  Send_Success or Send_Error.
-- with the mail in it.  Usually the  client is  not yet connected  when
-- Send is called.  Send can be called multiple  times  in order to send
-- several mails.
--
-- Exception :
--
--    Use_Error - Invalid mail
--
   procedure Send
             (  Client  : in out SMTP_Client;
                Message : Mail
             );
   procedure Send
             (  Client   : in out SMTP_Client;
                Messages : Mail_Array
             );
--
-- Send_Abandoned -- Mail dropped notification
--
--    Client  - The client
--    Message - Successfully sent
--
-- This procedure is called when some mails were not sent due to earlier
-- errors.  The parameter  Messages contains unsent mails.  The  default
-- implementation traces abandoned mails.
--
   procedure Send_Abandoned
             (  Client   : in out SMTP_Client;
                Messages : Mail_Array
             );
--
-- Send_Error -- Error response notification
--
--    Client    - The client
--    Code      - The error code sent by the server
--    Context   - The request responded
--    Reply     - The server's reply
--  [ Message ] - Failed to send
--
-- This procedure is called  when the server responds with an error code
-- indicating an  unrecoverable error.  Code is the responded code.  The
-- parameter Context specifies the failed command.  When  Message is not
-- given no attempt is made to recover. The connection is dropped.  When
-- Message is  passed the error is limited to this message.  The  client
-- will  attempt  to send  other  messages  in  the queue.  The  default
-- implementation traces the error.
--
   procedure Send_Error
             (  Client  : in out SMTP_Client;
                Code    : Error_Code;
                Context : SMTP_Command;
                Reply   : String
             );
   procedure Send_Error
             (  Client  : in out SMTP_Client;
                Code    : Error_Code;
                Context : SMTP_Command;
                Reply   : String;
                Message : Mail
             );
--
-- Send_Success -- Mail sent notification
--
--    Client  - The client
--    Message - Successfully sent
--
-- This procedure  is  called  when a mail  was  accepted by the server.
-- The default implementation does tracing.
--
   procedure Send_Success
             (  Client  : in out SMTP_Client;
                Message : Mail
             );
--
-- Sent -- Overrides GNAT.Sockets.Server...
--
   procedure Sent (Client : in out SMTP_Client);
--
-- Set_Credentials -- Set credentials to login
--
--    Client   - The client
--    User     - The user name
--    Password - The password
--    Accepted - Authentication methods to use
--
-- This  procedure  sets the  user name  and  password  to log  into the
-- server . It must be  called  before  a connection  to the  server  is
-- established.  The parameter Accepted lists the authentication methods
-- the client  can use  when the  server  offers them.  If the parameter
-- specifies more than one method the client prefers one with the bigger
-- value.
--
   procedure Set_Credentials
             (  Client   : in out SMTP_Client;
                User     : String;
                Password : String;
                Accepted : SMTP_AUTH_Mechanism :=
                           SMTP_AUTH_Mechanism'Last
             );
--
-- Set_Enhanced -- Set force enhanced mode flag
--
--    Client   - The client
--    Enhanced - The flag
--
-- Normally the client uses enhanced mode when the server greets it with
-- a line  containing "ESMTP" keyword.  When this flag is set the client
-- will  use the enhanced mode regardless the greeting.  This  procedure
-- has the effect  only  when the client  is not  yet  connected  to the
-- server.
--
   procedure Set_Enhanced
             (  Client   : in out SMTP_Client;
                Enhanced : Boolean
             );
--
-- Set_TLS -- Set TLS support
--
--    Client - The client
--    Enable - True to engage TLS if the server offers it
--
   procedure Set_TLS
             (  Client : in out SMTP_Client;
                Enable : Boolean
             );

private
   type String_Ptr is access String;
   function Value (Text : String_Ptr) return String;

   type Stream_Ptr is access all Root_Stream_Type'Class;

   type Mail_Address_List is tagged record
      Set : Persistent.Catalogue.Set;
   end record;

   type Text_Array is array (Text_Header) of String_Ptr;
   type List_Array is array (List_Header) of Mail_Address_List;
   type Date_Array is array (Date_Header) of Time;
   type Mail_Object;
   type Mail_Object_Ptr is access all Mail_Object'Class;
   procedure Delete (Message : in out Mail_Object_Ptr);
------------------------------------------------------------------------
-- Attachments
--
   type Attachment_Object;
   type Attachment_Object_Ptr is access Attachment_Object'Class;

   type Attachment_Object (Header_Length : Positive) is
      abstract new Ada.Finalization.Limited_Controlled with
   record
      Next   : Attachment_Object_Ptr;
      Header : String (1..Header_Length);
   end record;
   procedure Close (Attachment : in out Attachment_Object);
   procedure Read
             (  Attachment : in out Attachment_Object;
                Buffer     : in out Stream_Element_Array;
                Last       : out Stream_Element_Offset
             )  is abstract;

   type Attachment_Stream is new Attachment_Object with record
      Stream : Stream_Ptr;
      Owned  : Boolean := False;
   end record;
   procedure Finalize (Attachment : in out Attachment_Stream);
   procedure Read
             (  Attachment : in out Attachment_Stream;
                Buffer     : in out Stream_Element_Array;
                Last       : out Stream_Element_Offset
             );

   type Attachment_String
        (  Length        : Positive;
           Header_Length : Positive
        )  is new Attachment_Object (Header_Length) with
   record
      Index  : Integer := 1;
      Buffer : String (1..Length);
   end record;
   procedure Read
             (  Attachment : in out Attachment_String;
                Buffer     : in out Stream_Element_Array;
                Last       : out Stream_Element_Offset
             );

   type Attachment_File
        (  Name_Length   : Positive;
           Header_Length : Positive
        )  is new Attachment_Object (Header_Length) with
   record
      Open : Boolean := False;
      File : Stream_IO.File_Type;
      Name : String (1..Name_Length);
   end record;
   procedure Close (Attachment : in out Attachment_File);
   procedure Finalize (Attachment : in out Attachment_File);
   procedure Read
             (  Attachment : in out Attachment_File;
                Buffer     : in out Stream_Element_Array;
                Last       : out Stream_Element_Offset
             );
------------------------------------------------------------------------
   type Mail_Object is new Object.Entity with record
      Status    : Mail_Status := Mail_Pending;
      Previous  : Mail_Object_Ptr;
      Next      : Mail_Object_Ptr;
      Contents  : String_Ptr;
      First     : Attachment_Object_Ptr;
      Last      : Attachment_Object_Ptr;
      Texts     : Text_Array;
      Addresses : List_Array;
      Dates     : Date_Array := (others => Clock);
      pragma Atomic (Status);
   end record;
   procedure Append
             (  Object     : in out Mail_Object;
                Attachment : Attachment_Object_Ptr
             );
   function Create_String_Attachment
            (  Contents     : String;
               Content_Type : String := "text/plain";
               Disposition  : String := "";
               Description  : String := ""
            )  return Attachment_Object_Ptr;
   function Create_Stream_Attachment
            (  Contents     : Stream_Ptr;
               Owned        : Boolean;
               Content_Type : String := "text/plain";
               Disposition  : String := "";
               Description  : String := ""
            )  return Attachment_Object_Ptr;
   procedure Erase    (Object : in out Mail_Object);
   procedure Finalize (Object : in out Mail_Object);
   procedure Prepend
             (  Object     : in out Mail_Object;
                Attachment : Attachment_Object_Ptr
             );
   procedure Set
             (  Object : in out Mail_Object;
                Header : Text_Header;
                Text   : String
             );
   procedure Set_MIME (Object : in out Mail_Object'Class);

   package Mail_Handles is
      new Object.Handle (Mail_Object, Mail_Object_Ptr);

   type Mail is record
      Reference : Mail_Handles.Handle;
   end record;

   type SMTP_Receive_State is
        (  Receive_Code,
           Receive_Class,
           Receive_Subject,
           Receive_Detail,
           Receive_Line,
           Receive_LF
        );
   type SMTP_Send_State is
        (  Send_Command,
           Send_Header,
           Send_Colon,
           Send_Value,
           Send_CRLF,
           Send_String_Body,
           Send_String_Dot,
           Send_Data_End,
           Send_Multipart_End,
           Send_Boundary_First,
           Send_Boundary_Middle,
           Send_Boundary_Last,
           Send_Content_Header,
           Send_Content
        );
   package SMTP_Extension_Tables_Raw is new Tables (SMTP_Extension);
   package SMTP_Extension_Tables is new SMTP_Extension_Tables_Raw.Names;
   use SMTP_Extension_Tables;

   package SMTP_AUTH_Tables_Raw is new Tables (SMTP_AUTH_Mechanism);
   package SMTP_AUTH_Tables is new SMTP_AUTH_Tables_Raw.Names;
   use SMTP_AUTH_Tables;

   Extensions      : SMTP_Extension_Tables.Dictionary;
   Authentications : SMTP_AUTH_Tables.Dictionary;

   type Extension_Flags is array (SMTP_Extension) of Boolean;
------------------------------------------------------------------------
   type SMTP_Client
        (  Listener     : access Connections_Server'Class;
           Input_Size   : Buffer_Length;
           Output_Size  : Buffer_Length;
           Reply_Length : Positive
        )  is new Connection
                  (  Input_Size  => Input_Size,
                     Output_Size => Output_Size
                  )  with
   record
      Send_State     : SMTP_Send_State      := Send_Command;
      Receive_State  : SMTP_Receive_State   := Receive_Code;
      Count          : Stream_Element_Count := 0;
      Authentication : SMTP_AUTH_Mechanism  := 0;
      Supported      : SMTP_AUTH_Mechanism  := SMTP_AUTH_Mechanism'Last;
      Mail_Size      : Natural              := Natural'Last;
      Header         : Mail_Header;
      Index          : Natural;
      ESMTP          : Boolean;
      Offered_TLS    : Boolean := False;
      Accept_TLS     : Boolean := True;
      Force_ESMTP    : Boolean := False;
      Attachment     : Attachment_Object_Ptr;
      User           : String_Ptr;
      Password       : String_Ptr;
         -- Mail queue
      Queue          : Mail_Object_Ptr;
         -- Current command
      Command        : SMTP_Command;
      Code           : Error_Code;
         -- The reply line
      End_Of_Reply   : Boolean;
      Pointer        : Integer;
      Encoded        : String_Ptr;
      Encoded_Last   : Integer;
      Extensions     : Extension_Flags;
      Reply          : String (1..Reply_Length);
   end record;

   procedure On_Mail (Client : in out SMTP_Client'Class);
   procedure Send_Data
             (  Client : in out SMTP_Client'Class;
                Data   : Stream_Element_Array
             );
   procedure Send_Text
             (  Client : in out SMTP_Client'Class;
                Text   : String
             );

   function Get_Content_Type        (Value : String) return String;
   function Get_Content_Disposition (Value : String) return String;
   function Get_Content_Description (Value : String) return String;

end GNAT.Sockets.SMTP.Client;
