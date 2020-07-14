--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     LDAP.Client                                 Summer, 2019       --
--  Interface                                                         --
--                                Last revision :  21:39 03 Aug 2019  --
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

with Ada.Task_Identification;  use Ada.Task_Identification;

package GNAT.Sockets.Connection_State_Machine.LDAP.Client is
--
-- Search_Result -- The result of a search operation
--
   type Search_Result (<>) is private;
--
-- Get_Entries_Number -- The number of found entries
--
--    Result - The search result
--
-- Returns :
--
--    The number of found entries
--
   function Get_Entries_Number (Result : Search_Result) return Natural;
--
-- Get_Entry_Attributes -- The attributes list of a found entry
--
--    Result - The search result
--    Index  - The index of the entry 1..Get_Entries_Number
--
-- Returns :
--
--    The entry attributes list
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get_Entry_Attributes
            (  Result : Search_Result;
               Index  : Positive
            )  return Attributes_List;
--
-- Get_Entry_Name -- The name of a found entry
--
--    Result - The search result
--    Index  - The index of the entry 1..Get_Entries_Number
--
-- Returns :
--
--    The entry's distinguished name
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get_Entry_Name
            (  Result : Search_Result;
               Index  : Positive
            )  return Distinguished_Name;
--
-- Get_References -- The reference URI in the search result
--
--    Result - The search result
--
-- Returns :
--
--    The list of URIs
--
   function Get_References (Result : Search_Result) return Values_List;
------------------------------------------------------------------------
--
-- LDAP_Client -- LDAP client
--
--    Listener           - The connection object
--    Message_Length     - The maximum output LDAP message length
--    Incoming_Data_Size - The arena pool size  to allocate incoming
--                         dynamic LDAP objects and string bodies
--    Outgoing_Data_Size - The arena pool size for outgoing LDAP objects
--    Input_Size         - The input buffer size
--    Output_Size        - The output buffer size
--
-- Message_Length must be larger than the encoded length of any outgoing
-- LDAP message.  Outgoing_Data_Size  determines  the size  of the arena
-- pool where are variable-length  values allocated.  This includes LDAP
-- objects  defined recursively or as variable-sized sets and sequences.
-- The arena must accommodate  data for any outgoing LDAP  message built
-- there before getting encoded. Incoming_Data_Size  determines the size
-- of the arena pool for the incoming LDAP message.
--
   type LDAP_Client is new LDAP_Peer with private;
--
-- Cancel -- Waiting for connection
--
--    Client - The LDAP client
--
-- This  procedure  is used to cancel Connect  or Wait  pending in other
-- tasks. It does not influence the communication itself. The operations
-- will propagate Cancel_Error if not yet completed.
--
   procedure Cancel (Client : in out LDAP_Client);
--
-- Connect -- Connect to a server
--
--    Client         - The LDAP client
--    Host           - The host name or IP address
--    Port           - The port number
--    Max_Connect_No - Maximal number of connection attempts
--    Timeout        - For the operation
--
-- This procedure  is used  to connect to a remote server.  When already
-- connected  the current connection  is dropped  and another initiated.
-- The procedure ends when the host is connected.
--
-- Exceptions :
--
--    Cancel_Error  - Waiting canceled by a call to Cancel
--    Socket_Error  - Socket error
--    Status_Error  - Connection failure, e.g. attempts exhausted
--    Timeout_Error - Timeout expired
--
   procedure Connect
             (  Client         : in out LDAP_Client;
                Host           : String;
                Port           : Port_Type := 389;
                Max_Connect_No : Positive  := Positive'Last;
                Timeout        : Duration  := Duration'Last
             );
--
-- Exists -- Send search request
--
--    Client  - The LDAP client
--    Name    - To check
--    Timeout - The timeout
--
-- This function is a wrapper around Send_Search used to check if a name
-- exists.
--
-- Returns :
--
--    True of Name exists
--
   function Exists
            (  Client  : LDAP_Client;
               Name    : Distinguished_Name;
               Timeout : Duration
            )  return Boolean;
--
-- Receive_Add_Response -- Received add response callback
--
--    Client   - The LDAP client
--    ID       - The message ID
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- The default implementation does nothing.
--
   procedure Receive_Add_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             );
--
-- Receive_Bind_Response -- Received bind response callback
--
--    Client      - The LDAP client
--    ID          - The message ID
--    Result      - The result code
--    Matched     - The matched name
--    Message     - The diagnostic message
--    Referral    - The referral list
--    Credentials - The credentials
--
-- The default implementation does nothing.
--
   procedure Receive_Bind_Response
             (  Client      : in out LDAP_Client;
                ID          : Integer_32;
                Result      : Result_Code;
                Matched     : Distinguished_Name;
                Message     : String;
                Referral    : Values_List;
                Credentials : String
             );
--
-- Receive_Challenge -- Received bind challenge callback
--
--    Client      - The LDAP client
--    Mechanism   - The authentication mechanism
--    Matched     - The matched name
--    Message     - The diagnostic message
--    Credentials - The challenge
--
-- This procedure  is called  when  synchronous  bind request receives a
-- challenge  from  the server  when  SASL  authentication is used.  The
-- default implementation propagates Use_Error.
--
   function Receive_Challenge
            (  Client      : access LDAP_Client;
               Mechanism   : String;
               Matched     : Distinguished_Name;
               Message     : String;
               Credentials : String
            )  return String;
--
-- Receive_Compare_Response -- Received compare response callback
--
--    Client   - The LDAP client
--    ID       - The message ID
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- The default implementation does nothing.
--
   procedure Receive_Compare_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             );
--
-- Receive_Delete_Response -- Received delete response callback
--
--    Client   - The LDAP client
--    ID       - The message ID
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- The default implementation does nothing.
--
   procedure Receive_Delete_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             );
--
-- Receive_Extended_Response -- Received extended response callback
--
--    Client - The LDAP client
--    ID     - The message ID
--    Name   - The object identifier
--    Value  - The value
--
-- The default implementation does nothing.
--
   procedure Receive_Extended_Response
             (  Client : in out LDAP_Client;
                ID     : Integer_32;
                Name   : Object_Identifier;
                Value  : String
             );
--
-- Receive_Intermediate_Response -- Received intermediate response
--                                  callback
--    Client - The LDAP client
--    ID     - The message ID
--    Name   - The object identifier
--    Value  - The value
--
-- The default implementation does nothing.
--
   procedure Receive_Intermediate_Response
             (  Client : in out LDAP_Client;
                ID     : Integer_32;
                Name   : Object_Identifier;
                Value  : String
             );
--
-- Receive_Modify_Response -- Received modify response callback
--
--    Client   - The LDAP client
--    ID       - The message ID
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- The default implementation does nothing.
--
   procedure Receive_Modify_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             );
--
-- Receive_Modify_DN_Response -- Received modify DN response callback
--
--    Client   - The LDAP client
--    ID       - The message ID
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- The default implementation does nothing.
--
   procedure Receive_Modify_DN_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             );
--
-- Receive_Search_Done_Response -- Received search done response
--                                 callback
--    Client   - The LDAP client
--    ID       - The message ID
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- The default implementation does nothing.
--
   procedure Receive_Search_Done_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             );
--
-- Receive_Search_Entry_Response -- Received search entry response
--                                  callback
--    Client     - The LDAP client
--    ID         - The message ID
--    Name       - Found distinguished name
--    Attributes - Found attributes
--
-- The default implementation does nothing.
--
   procedure Receive_Search_Entry_Response
             (  Client     : in out LDAP_Client;
                ID         : Integer_32;
                Name       : Distinguished_Name;
                Attributes : Attributes_List
             );
--
-- Receive_Search_Reference_Response -- Received search reference
--                                      response callback
--    Client - The LDAP client
--    ID     - The message ID
--    URIs   - The references list
--
-- The default implementation does nothing.
--
   procedure Receive_Search_Reference_Response
             (  Client : in out LDAP_Client;
                ID     : Integer_32;
                URIs   : Values_List
             );
--
-- Send_Abandon -- Send add request
--
--    Client - The LDAP client
--    ID     - The message ID
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Abandon
             (  Client : in out LDAP_Client;
                ID     : Integer_32
             );
--
-- Send_Add -- Send add request
--
--    Client     - The LDAP client
--    Name       - The entry to add
--    Attributes - The list of attribute description with values list
--  [ Timeout ]  - The timeout, asynchronous if absent
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Add
             (  Client     : in out LDAP_Client;
                Name       : Distinguished_Name;
                Attributes : Attributes_List;
                Timeout    : Duration := Duration'First
             );
--
-- Send_Bind -- Send bind request (simple authentication)
--
--    Client    - The LDAP client
--    Name      - The user name
--    Password  - The password
--  [ Timeout ] - The timeout, asynchronous if absent
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Bind
             (  Client   : in out LDAP_Client;
                Name     : Distinguished_Name;
                Password : String;
                Timeout  : Duration := Duration'First
             );
--
-- Send_Bind -- Send bind request (SASL authentication)
--
--    Client      - The LDAP client
--    Name        - The name to bind
--    Mechanism   - The SASL mechanism to use
--    Credentials - The credentials if needed
--  [ Timeout ]   - The timeout, asynchronous if absent
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Bind
             (  Client      : in out LDAP_Client;
                Name        : Distinguished_Name;
                Mechanism   : String;
                Credentials : String   := "";
                Timeout     : Duration := Duration'First
             );
--
-- Send_Compare -- Send compare request
--
--    Client      - The LDAP client
--    Name        - The entry name
--    Description - The attribute description
--    Value       - The attribute value
--  [ Timeout ]   - The timeout, asynchronous if absent
--
-- Returns :
--
--    The result True/False
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Compare
             (  Client      : in out LDAP_Client;
                Name        : Distinguished_Name;
                Description : String;
                Value       : String
             );
   function Send_Compare
            (  Client      : LDAP_Client;
               Name        : Distinguished_Name;
               Description : String;
               Value       : String;
               Timeout     : Duration
            )  return Boolean;
--
-- Send_Delete -- Send delete request
--
--    Client    - The LDAP client
--    Name      - The entry name
--  [ Timeout ] - The timeout, asynchronous if absent
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Delete
             (  Client  : in out LDAP_Client;
                Name    : Distinguished_Name;
                Timeout : Duration := Duration'First
             );
--
-- Send_Extended -- Send extended request
--
--    Client    - The LDAP client
--    Name      - The name
--    Value     - The value
--  [ Timeout ] - The timeout, asynchronous if absent
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Extended
             (  Client  : in out LDAP_Client;
                Name    : Object_Identifier;
                Value   : String;
                Timeout : Duration := Duration'First
             );
--
-- Send_Modify -- Send modify request
--
--    Client    - The LDAP client
--    Name      - The object to modify
--    Update    - The updates list
--  [ Timeout ] - The timeout, asynchronous if absent
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Modify
             (  Client  : in out LDAP_Client;
                Name    : Distinguished_Name;
                Update  : Updates_List;
                Timeout : Duration := Duration'First
             );
--
-- Send_Modify_DN -- Send modify DN request
--
--    Client       - The LDAP client
--    Name         - The entry to modify
--    New_RDN      - The new relative distinguished name
--    Delete_RDN   - Delete any attribute values in the original
--    New_Superior - The new parent for the entry
--  [ Timeout ]    - The timeout, asynchronous if absent
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Modify_DN
             (  Client       : in out LDAP_Client;
                Name         : Distinguished_Name;
                New_RDN      : Distinguished_Name;
                Delete_RDN   : Boolean;
                New_Superior : Distinguished_Name;
                Timeout      : Duration := Duration'First
             );
   procedure Send_Modify_DN
             (  Client     : in out LDAP_Client;
                Name       : Distinguished_Name;
                New_RDN    : Distinguished_Name;
                Delete_RDN : Boolean;
                Timeout    : Duration := Duration'First
             );
--
-- Send_Search -- Send search request
--
--    Client        - The LDAP client
--    Name          - To start the search at
--    Filter        - The filter to set
--    Must_Exist    - Raise End_Error if the object does not exist
--    Scope         - Of the search relatively to Base_Object
--    Aliasing_Mode - The method to handle aliases
--    Size_Limit    - The maximum number of result entries
--    Time_Limit    - For processing the request
--    Types_Only    - Only include attribute descriptions
--  [ Timeout ]     - The timeout, asynchronous if absent
--
-- When the synchronous with timeout is used,  the procedure awaits  all
-- exchange  to complete until the server sends search result done.  The
-- callbacks
--
--    Receive_Search_Entry_Response and
--    Receive_Search_Reference_Response
--
-- are called to accumulate intermediate server responses.
--
-- Returns :
--
--    The accumulated search result
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    End_Error    - No object and Must_Exist is True
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Search
             (  Client        : in out LDAP_Client;
                Name          : Distinguished_Name;
                Filter        : Search_Filter;
                Must_Exist    : Boolean          := False;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False;
                Timeout       : Duration         := Duration'First
             );
   function Send_Search
            (  Client        : LDAP_Client;
               Name          : Distinguished_Name;
               Filter        : Search_Filter;
               Must_Exist    : Boolean          := False;
               Scope         : Scope_Type       := Whole_Subtree_Scope;
               Aliasing_Mode : Dereference_Type := Deref_Always;
               Size_Limit    : Integer_32       := 0;
               Time_Limit    : Duration         := 0.0;
               Types_Only    : Boolean          := False;
               Timeout       : Duration
            )  return Search_Result;
--
-- Send_Unbind -- Send unbind request
--
--    Client - The LDAP client
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Send_Unbind (Client : in out LDAP_Client);
--
-- Trace -- Tracing
--
--    Client  - The LDAP client
--    Message - To write into the trace
--
   procedure Trace
             (  Client  : in out LDAP_Client;
                Message : String
             );
--
-- Wait -- For connection object ready
--
--    Client    - The LDAP client
--    Connected - Require connection
--    Timeout   - For the operation
--
-- This procedure is called to wait for the session to become ready  for
-- another request.
--
-- Exceptions :
--
--    Cancel_Error  - Waiting canceled by a call to Cancel
--    Socket_Error  - Socket error
--    Status_Error  - Connection failure, e.g. attempts exhausted
--    Timeout_Error - Timeout expired
--
   procedure Wait
             (  Client    : in out LDAP_Client;
                Connected : Boolean;
                Timeout   : Duration := Duration'Last
             );

private
   type Distinguished_Name_Ptr is access Distinguished_Name;
   type Search_Entry (Count : Natural) is new Object.Entity with record
      Name       : Distinguished_Name_Ptr;
      Attributes : Attributes_List (Count);
   end record;
   type Search_Entry_Ptr is access Search_Entry'Class;
   procedure Finalize (Item : in out Search_Entry);
   package Search_Entry_Handles is
      new Object.Handle (Search_Entry, Search_Entry_Ptr);
   function Create
            (  Name       : Distinguished_Name;
               Attributes : Attributes_List
            )  return Search_Entry_Handles.Handle;
   type Search_Entry_Array is
      array (Positive range <>) of Search_Entry_Handles.Handle;
   type Search_Result
        (  Entries_Count    : Natural;
           References_Count : Natural
        )  is
   record
      Entries    : Search_Entry_Array (1..Entries_Count);
      References : Values_List (References_Count);
   end record;

   protected type Event_Type
                  (  Client : access LDAP_Client'Class
                  )  is
      entry Cancel;
      entry Do_Notification (ID : Integer_32; Choice : Positive);
      procedure Release;
      entry Released;
      entry Start_Notification (ID : Integer_32; Choice : Positive);
      entry Start_Wait         (ID : Integer_32);
      entry Stop_Notification  (ID : Integer_32; Choice : Positive);
      procedure Stop_Wait;
      entry Seize;
      entry Wait (Connected : in out Boolean);
      procedure Set;
   private
      entry Lounge;
      entry Wait_Lounge (Boolean) (ID : Integer_32);

      Ready : Boolean := False;
      Down  : Boolean := True;
      Owner : Task_ID := Null_Task_ID;
      Count : Natural := 0;

      Current_Lounge   : Boolean := False;
      Requests_Count   : Natural := 0;
      Current_ID       : Integer_32;
      Current_Response : Integer;
   end Event_Type;

   procedure Connected (Client : in out LDAP_Client);
   procedure End_Of_Query (Client : in out LDAP_Client);
   procedure Released (Client : in out LDAP_Client);

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Event_Type
             );
   for Event_Type'Write use Write;

   type LDAP_Client is new LDAP_Peer with record
      Event    : aliased Event_Type (LDAP_Client'Unchecked_Access);
      Sequence : Integer_32 := 1;
   end record;
   procedure Check_Result
             (  Client : in out LDAP_Client;
                Result : LDAP_Result'Class
             );
   procedure Process_Packet (Client : in out LDAP_Client);

   type Holder (Event : access Event_Type) is
      new Ada.Finalization.Limited_Controlled with null record;
   procedure Finalize  (Lock : in out Holder);
   procedure Initialize (Lock : in out Holder);

end GNAT.Sockets.Connection_State_Machine.LDAP.Client;
