--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_HTTPS_OpenSSL_Client                   Luebeck            --
--  HTTPS OpenSSL client test                      Winter, 2019       --
--                                                                    --
--                                Last revision :  14:41 03 Apr 2020  --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Streams;                  use Ada.Streams;
--with GNAT.Exception_Traces;      use GNAT.Exception_Traces;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with GNAT.Sockets.Server.OpenSSL;  use GNAT.Sockets.Server.OpenSSL;
with OpenSSL;                      use OpenSSL;
with Parsers.JSON;                 use Parsers.JSON;
with Parsers.JSON.String_Source;   use Parsers.JSON.String_Source;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.Quoted;          use Strings_Edit.Quoted;
with Strings_Edit.Streams;         use Strings_Edit.Streams;
with Strings_Edit.Long_Floats;     use Strings_Edit.Long_Floats;
with Test_HTTP_Servers.OpenSSL;    use Test_HTTP_Servers.OpenSSL;

with GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled;
with GNAT.Sockets.Server.Pooled;
with Parsers.String_Source;
with Stack_Storage;

procedure Test_HTTPS_OpenSSL_JSON_Client is
   use GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled;

   Address : constant String := "poloniex.com";
   Path    : constant String := "public?command=returnTicker";
   Port    : constant := 443;

   procedure Dump (Prefix : String; Value : JSON_Value) is
   begin
      case Value.JSON_Type is
         when JSON_Boolean =>
            Put_Line (Prefix & Boolean'Image (Value.Condition));
         when JSON_Null =>
            Put_Line (Prefix & "null");
         when JSON_Number =>
            Put_Line (Prefix & Image (Value.Value));
         when JSON_String =>
            Put_Line (Prefix & Quote (Value.Text.all));
         when JSON_Array =>
            Put_Line (Prefix & "(");
            for Index in Value.Sequence'Range loop
               Dump (Prefix & "   ", Value.Sequence (Index));
            end loop;
            Put_Line (Prefix & ")");
         when JSON_Object =>
            Put_Line (Prefix & "{");
            for Index in Value.Map'Range loop
               Put_Line (Prefix & "   " & Value.Map (Index).Name.all & "=");
               Dump (Prefix & "      ", Value.Map (Index).Value);
            end loop;
            Put_Line (Prefix & "}");
      end case;
   end Dump;
begin
   declare
      Factory : aliased HTTPS_OpenSSL_Factory
                        (  Request_Length  => 200,
                           Input_Size      => 40,
                           Output_Size     => 1024,
                           Decoded_Size    => 40,
                           Max_Connections => 100
                        );
   begin
      Set_Default_Verify_Paths (Factory, Client_Context);
      declare
         Message   : aliased String_Stream (1024 * 100);
         Server    : aliased GNAT.Sockets.Server.
                             Connections_Server (Factory'Access, 0);
         Reference : GNAT.Sockets.Server.Handles.Handle;
      begin
         Put_Line ("HTTP client started");
         Set
         (  Reference,
            new HTTP_Session_Signaled
                (  Server'Unchecked_Access,
                   200,
                   512,
                   1024
         )      );
         declare
            Client : HTTP_Session_Signaled renames
                     HTTP_Session_Signaled (Ptr (Reference).all);
         begin
            Connect (Client, Address, Port);
            Get
            (  Client,
               "https://" & Address & "/" & Path,
               Message'Unchecked_Access
            );
            Wait (Client, False);
            Put_Line
            (  Image (Get_Response_Code (Client))
            &  " "
            &  Get_Response_Reason (Client)
            &  " Message >>>>>>>>>>>>>>>>>>>>"
            );
            declare
               Content : aliased String := Get (Message);
               Source  : aliased Parsers.String_Source.
                                 Source (Content'Access);
               Arena   : aliased Stack_Storage.Pool (1024, 10);
               Data    : constant JSON_Value :=
                                  Parse (Source'Access, Arena'Access);
            begin
               Dump ("", Data);
            end;
            Put_Line ("<<<<<<<<<<<<<<<<<<<< Message");
         end;
         Put_Line ("HTTP client stopping");
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_HTTPS_OpenSSL_JSON_Client;
