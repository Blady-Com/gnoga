--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     LDAP.Server                                 Summer, 2019       --
--  Interface                                                         --
--                                Last revision :  18:41 01 Aug 2019  --
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

package GNAT.Sockets.Connection_State_Machine.LDAP.Server is
--
-- LDAP_Server -- LDAP server
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
   type LDAP_Server is new LDAP_Peer with private;
--
-- The server must  implement the callbacks to the client requests.  The
-- callback  parameters  are  decoded  when  they  are  straightforward.
-- Otherwise the request object is passed without decoding to reduce the
-- overhead.
--
-- Receive_Abandon_Request -- Received abandon request callback
--
--    Server - The LDAP server
--    ID     - The ID of the message to abandon
--
-- The default implementation does nothing.
--
   procedure Receive_Abandon_Request
             (  Server : in out LDAP_Server;
                ID     : Integer_32
             );
--
-- Receive_Add_Request -- Received add request callback
--
--    Server  - The LDAP server
--    Request - The add request
--
-- The default implementation responds with Unwilling_To_Perform_Code.
--
   procedure Receive_Add_Request
             (  Server  : in out LDAP_Server;
                Request : LDAP_Add_Request
             );
--
-- Receive_Bind_Request -- Received bind request callback
--
--    Server      - The LDAP server
--    Name        - The name to bind
--    Password    - The password
--    Credentials - The credentials
--
-- This  procedure  is  called  to perform  simple  authentication.  The
-- implementation responds with Auth_Method_Not_Supported_Code.
--
   procedure Receive_Bind_Request
             (  Server   : in out LDAP_Server;
                Name     : Distinguished_Name;
                Password : String
             );
--
-- Receive_Bind_Request -- Received bind request callback
--
--    Server      - The LDAP server
--    Name        - The name to bind to
--    Mechanism   - The authentication mechanism
--    Credentials - The credentials
--
-- This  procedure  is  called  to  perform  SASL  authentication .  The
-- implementation responds with Auth_Method_Not_Supported_Code.
--
   procedure Receive_Bind_Request
             (  Server      : in out LDAP_Server;
                Name        : Distinguished_Name;
                Mechanism   : String;
                Credentials : String
             );
--
-- Receive_Compare_Request -- Received compare request callback
--
--    Server  - The LDAP server
--    Request - The compare request
--
-- The default implementation responds with Unwilling_To_Perform_Code.
--
   procedure Receive_Compare_Request
             (  Server  : in out LDAP_Server;
                Request : LDAP_Compare_Request
             );
--
-- Receive_Delete_Request -- Received delete request callback
--
--    Server - The LDAP server
--    Name   - The name to delete
--
-- The default implementation responds with Unwilling_To_Perform_Code.
--
   procedure Receive_Delete_Request
             (  Server : in out LDAP_Server;
                Name   : Distinguished_Name
             );
--
-- Receive_Extended_Request -- Received extended request callback
--
--    Server - The LDAP server
--    Name   - The object identifier
--    Value  - The value
--
-- The default implementation responds with Unwilling_To_Perform_Code.
--
   procedure Receive_Extended_Request
             (  Server : in out LDAP_Server;
                Name   : Object_Identifier;
                Value  : String
             );
--
-- Receive_Modify_Request -- Received modify request callback
--
--    Server  - The LDAP server
--    Request - The modify request
--
-- The default implementation responds with Unwilling_To_Perform_Code.
--
   procedure Receive_Modify_Request
             (  Server  : in out LDAP_Server;
                Request : LDAP_Modify_Request
             );
--
-- Receive_Modify_DN_Request -- Received modify DN request callback
--
--    Server  - The LDAP server
--    Request - The modify DN request
--
-- The default implementation responds with Unwilling_To_Perform_Code.
--
   procedure Receive_Modify_DN_Request
             (  Server   : in out LDAP_Server;
                Request : LDAP_Modify_DN_Request
             );
--
-- Receive_Search_Request -- Received search request callback
--
--    Server  - The LDAP server
--    Request - The search request
--
-- The default implementation responds with Unwilling_To_Perform_Code.
--
   procedure Receive_Search_Request
             (  Server  : in out LDAP_Server;
                Request : LDAP_Search_Request
             );
--
-- Receive_Unbind_Request -- Received unbind request callback
--
--    Server  - The LDAP server
--
-- The default implementation does nothing.
--
   procedure Receive_Unbind_Request
             (  Server : in out LDAP_Server
             );
--
-- Reply_Add -- Reply to an add request
--
--    Server   - The LDAP server
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- Exceptions :
--
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Add
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             );
--
-- Reply_Bind -- Reply to a bind request
--
--    Server        - The LDAP server
--    Result        - The result code
--  [ Credentials ] - The SASL challenge
--    Matched       - The matched name
--    Message       - The diagnostic message
--    Referral      - The referral list
--
-- Exceptions :
--
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Bind
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             );
   procedure Reply_Bind
             (  Server      : in out LDAP_Server;
                Result      : Result_Code;
                Credentials : String;
                Matched     : Distinguished_Name := Null_Name;
                Message     : String             := "";
                Referral    : Values_List        := Empty
             );
--
-- Reply_Compare -- Reply to a compare request
--
--    Server   - The LDAP server
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Compare
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             );
--
-- Reply_Delete -- Reply to a delete request
--
--    Server   - The LDAP server
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Delete
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             );
--
-- Reply_Extended -- Reply using extended response
--
--    Server   - The LDAP server
--    Name     - The object identifier
--    Value    - The value
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Extended
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Name     : Object_Identifier;
                Value    : String;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             );
--
-- Reply_Intermediate -- Reply using intermediate response
--
--    Server - The LDAP server
--    Name   - The object identifier
--    Value  - The value
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Intermediate
             (  Server : in out LDAP_Server;
                Name   : Object_Identifier;
                Value  : String
             );
--
-- Reply_Modify -- Reply to a modify request
--
--    Server   - The LDAP server
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Modify
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             );
--
-- Reply_Modify_DN -- Reply to a modify DN request
--
--    Server   - The LDAP server
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Modify_DN
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             );
--
-- Reply_Search_Done -- Last reply to a search request
--
--    Server   - The LDAP server
--    Result   - The result code
--    Matched  - The matched name
--    Message  - The diagnostic message
--    Referral - The referral list
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Search_Done
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             );
--
-- Reply_Search_Entry -- Partial reply to a search request
--
--    Server     - The LDAP server
--    Name       - Found distinguished name
--    Attributes - Found attributes
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Search_Entry
             (  Server     : in out LDAP_Server;
                Name       : Distinguished_Name;
                Attributes : Attributes_List
             );
--
-- Reply_Search_Reference -- Partial reply to a search request
--
--    Server - The LDAP server
--    URIs   - The references list
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Status_Error - The connection is down
--    Data_Error   - Protocol errors
--    LDAP_Error   - Execution error
--
   procedure Reply_Search_Reference
             (  Server : in out LDAP_Server;
                URIs   : Values_List
             );
--
-- Trace -- Tracing
--
--    Server  - The LDAP server
--    Message - To write into the trace
--
   procedure Trace
             (  Server  : in out LDAP_Server;
                Message : String
             );
private
   type LDAP_Server is new LDAP_Peer with null record;
   procedure Process_Packet (Server : in out LDAP_Server);

end GNAT.Sockets.Connection_State_Machine.LDAP.Server;
