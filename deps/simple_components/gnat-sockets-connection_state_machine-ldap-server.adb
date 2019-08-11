--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     LDAP.Server                                 Summer, 2019       --
--  Implementation                                                    --
--                                Last revision :  13:37 03 Aug 2019  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body GNAT.Sockets.Connection_State_Machine.LDAP.Server is

   procedure Process_Packet (Server : in out LDAP_Server) is
   begin
      case Get_Selected (Server.Message.Protocol_Op) is
         when Abandon_Request_Choice =>
            Receive_Abandon_Request
            (  LDAP_Server'Class (Server),
               Server.Message.Protocol_Op.Abandon_Request.Value
            );
         when Add_Request_Choice =>
            Receive_Add_Request
            (  LDAP_Server'Class (Server),
               Server.Message.Protocol_Op.Add_Request
            );
         when Bind_Request_Choice =>
            declare
               This : LDAP_Bind_Request renames
                      Server.Message.Protocol_Op.Bind_Request;
            begin
               if Get_Selected (This.Authentication) = 1 then
                  Receive_Bind_Request
                  (  LDAP_Server'Class (Server),
                     Get_Name  (This.Name),
                     Get_Value (This.Authentication.Simple)
                  );
               else
                  Receive_Bind_Request
                  (  LDAP_Server'Class (Server),
                     Get_Name  (This.Name),
                     Get_Value (This.Authentication.SASL.Mechanism),
                     Get_Value (This.Authentication.SASL.Credentials)
                  );
               end if;
            end;
         when Compare_Request_Choice =>
            Receive_Compare_Request
            (  LDAP_Server'Class (Server),
               Server.Message.Protocol_Op.Compare_Request
            );
         when Delete_Request_Choice =>
            Receive_Delete_Request
            (  LDAP_Server'Class (Server),
               Get_Name (Server.Message.Protocol_Op.Delete_Request)
            );
         when Extended_Request_Choice =>
            declare
               This : LDAP_Extended_Request renames
                      Server.Message.Protocol_Op.Extended_Request;
            begin
               Receive_Extended_Request
               (  LDAP_Server'Class (Server),
                  Get (This.Name),
                  Get_Value (This.Value)
               );
            end;
         when Modify_Request_Choice =>
            Receive_Modify_Request
            (  LDAP_Server'Class (Server),
               Server.Message.Protocol_Op.Modify_Request
            );
         when Modify_DN_Request_Choice =>
            Receive_Modify_DN_Request
            (  LDAP_Server'Class (Server),
               Server.Message.Protocol_Op.Modify_DN_Request
            );
         when Search_Request_Choice =>
            Receive_Search_Request
            (  LDAP_Server'Class (Server),
               Server.Message.Protocol_Op.Search_Request
            );
         when Unbind_Request_Choice =>
            Receive_Unbind_Request (LDAP_Server'Class (Server));
         when others =>
            Trace
            (  LDAP_Server'Class (Server),
               (  "Unsolicited request "
               &  Image (Get_Selected (Server.Message.Protocol_Op))
            )  );
            return;
      end case;
   end Process_Packet;

   procedure Receive_Abandon_Request
             (  Server : in out LDAP_Server;
                ID     : Integer_32
             )  is
   begin
      null;
   end Receive_Abandon_Request;

   procedure Receive_Add_Request
             (  Server  : in out LDAP_Server;
                Request : LDAP_Add_Request
             )  is
   begin
      Reply_Add (Server, Unwilling_To_Perform_Code);
   end Receive_Add_Request;

   procedure Receive_Bind_Request
             (  Server   : in out LDAP_Server;
                Name     : Distinguished_Name;
                Password : String
             )  is
   begin
      Reply_Bind (Server, Auth_Method_Not_Supported_Code);
   end Receive_Bind_Request;

   procedure Receive_Bind_Request
             (  Server      : in out LDAP_Server;
                Name        : Distinguished_Name;
                Mechanism   : String;
                Credentials : String
             )  is
   begin
      Reply_Bind (Server, Auth_Method_Not_Supported_Code);
   end Receive_Bind_Request;

   procedure Receive_Compare_Request
             (  Server  : in out LDAP_Server;
                Request : LDAP_Compare_Request
             )  is
   begin
      Reply_Compare (Server, Unwilling_To_Perform_Code);
   end Receive_Compare_Request;

   procedure Receive_Delete_Request
             (  Server : in out LDAP_Server;
                Name   : Distinguished_Name
             )  is
   begin
      Reply_Delete (Server, Unwilling_To_Perform_Code);
   end Receive_Delete_Request;

   procedure Receive_Extended_Request
             (  Server : in out LDAP_Server;
                Name   : Object_Identifier;
                Value  : String
             )  is
   begin
      Reply_Extended
      (  Server,
         Unwilling_To_Perform_Code,
         (1..0 => 0),
         ""
      );
   end Receive_Extended_Request;

   procedure Receive_Modify_Request
             (  Server  : in out LDAP_Server;
                Request : LDAP_Modify_Request
             )  is
   begin
      Reply_Modify (Server, Unwilling_To_Perform_Code);
   end Receive_Modify_Request;

   procedure Receive_Modify_DN_Request
             (  Server   : in out LDAP_Server;
                Request : LDAP_Modify_DN_Request
             )  is
   begin
      Reply_Modify_DN (Server, Unwilling_To_Perform_Code);
   end Receive_Modify_DN_Request;

   procedure Receive_Search_Request
             (  Server  : in out LDAP_Server;
                Request : LDAP_Search_Request
             )  is
   begin
      Reply_Search_Done (Server, Unwilling_To_Perform_Code);
   end Receive_Search_Request;

   procedure Receive_Unbind_Request
             (  Server : in out LDAP_Server
             )  is
   begin
      null;
   end Receive_Unbind_Request;

   procedure Reply_Add
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             )  is
   begin
      Erase (Server.Buffer);
      Set_Add_Response
      (  Message     => Server.Output.Message,
         Code        => Result,
         Matched     => Matched,
         Diagnostics => Message,
         Referral    => Referral
      );
      Reply_Response (Server);
   end Reply_Add;

   procedure Reply_Bind
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             )  is
   begin
      Erase (Server.Buffer);
      Set_Bind_Response
      (  Message     => Server.Output.Message,
         Code        => Result,
         Matched     => Matched,
         Diagnostics => Message,
         Referral    => Referral
      );
      Reply_Response (Server);
   end Reply_Bind;

   procedure Reply_Bind
             (  Server      : in out LDAP_Server;
                Result      : Result_Code;
                Credentials : String;
                Matched     : Distinguished_Name := Null_Name;
                Message     : String             := "";
                Referral    : Values_List        := Empty
             )  is
   begin
      Erase (Server.Buffer);
      Set_Bind_Response
      (  Message     => Server.Output.Message,
         Code        => Result,
         Matched     => Matched,
         Diagnostics => Message,
         Referral    => Referral,
         Credentials => Credentials
      );
      Reply_Response (Server);
   end Reply_Bind;

   procedure Reply_Compare
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             )  is
   begin
      Erase (Server.Buffer);
      Set_Compare_Response
      (  Message     => Server.Output.Message,
         Code        => Result,
         Matched     => Matched,
         Diagnostics => Message,
         Referral    => Referral
      );
      Reply_Response (Server);
   end Reply_Compare;

   procedure Reply_Delete
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             )  is
   begin
      Erase (Server.Buffer);
      Set_Delete_Response
      (  Message     => Server.Output.Message,
         Code        => Result,
         Matched     => Matched,
         Diagnostics => Message,
         Referral    => Referral
      );
      Reply_Response (Server);
   end Reply_Delete;

   procedure Reply_Extended
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Name     : Object_Identifier;
                Value    : String;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             )  is
   begin
      Erase (Server.Buffer);
      Set_Extended_Response
      (  Message     => Server.Output.Message,
         Code        => Result,
         Matched     => Matched,
         Diagnostics => Message,
         Referral    => Referral
      );
      Reply_Response (Server);
   end Reply_Extended;

   procedure Reply_Intermediate
             (  Server : in out LDAP_Server;
                Name   : Object_Identifier;
                Value  : String
             )  is
   begin
      Erase (Server.Buffer);
      Set_Intermediate_Response
      (  Message => Server.Output.Message,
         Name    => Name,
         Value   => Value
      );
      Reply_Response (Server);
   end Reply_Intermediate;

   procedure Reply_Modify
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             )  is
   begin
      Erase (Server.Buffer);
      Set_Modify_Response
      (  Message     => Server.Output.Message,
         Code        => Result,
         Matched     => Matched,
         Diagnostics => Message,
         Referral    => Referral
      );
      Reply_Response (Server);
   end Reply_Modify;

   procedure Reply_Modify_DN
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             )  is
   begin
      Erase (Server.Buffer);
      Set_Modify_DN_Response
      (  Message     => Server.Output.Message,
         Code        => Result,
         Matched     => Matched,
         Diagnostics => Message,
         Referral    => Referral
      );
      Reply_Response (Server);
   end Reply_Modify_DN;

   procedure Reply_Search_Done
             (  Server   : in out LDAP_Server;
                Result   : Result_Code;
                Matched  : Distinguished_Name := Null_Name;
                Message  : String             := "";
                Referral : Values_List        := Empty
             )  is
   begin
      Erase (Server.Buffer);
      Set_Search_Result_Done
      (  Message     => Server.Output.Message,
         Code        => Result,
         Matched     => Matched,
         Diagnostics => Message,
         Referral    => Referral
      );
      Reply_Response (Server);
   end Reply_Search_Done;

   procedure Reply_Search_Entry
             (  Server     : in out LDAP_Server;
                Name       : Distinguished_Name;
                Attributes : Attributes_List
             )  is
   begin
      Erase (Server.Buffer);
      Set_Search_Result_Entry
      (  Message    => Server.Output.Message,
         Name       => Name,
         Attributes => Attributes
      );
      Reply_Response (Server);
   end Reply_Search_Entry;

   procedure Reply_Search_Reference
             (  Server : in out LDAP_Server;
                URIs   : Values_List
             )  is
   begin
      Erase (Server.Buffer);
      Set_Search_Result_Reference
      (  Message => Server.Output.Message,
         URIs    => URIs
      );
      Reply_Response (Server);
   end Reply_Search_Reference;

   procedure Trace
             (  Server  : in out LDAP_Server;
                Message : String
             )  is
   begin
      Trace
      (  Server.Listener.Factory.all,
         (  Get_Client_Name (Server.Listener.Factory.all, Server)
         &  ' '
         &  Message
      )  );
   end Trace;

end GNAT.Sockets.Connection_State_Machine.LDAP.Server;
