--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_HTTPS_OpenSSL_Client                   Luebeck            --
--  HTTPS OpenSSL client test                      Winter, 2019       --
--                                                                    --
--                                Last revision :  10:33 11 May 2019  --
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
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.Streams;         use Strings_Edit.Streams;
with Test_HTTP_Servers.OpenSSL;    use Test_HTTP_Servers.OpenSSL;

with GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled;
with GNAT.Sockets.Server.Pooled;

procedure Test_HTTPS_OpenSSL_Client is
   use GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled;

--    Address : constant String := "httpbin.org";
--    Path    : constant String := "get";
--    Port    : constant := 443;

   Address : constant String := "prod.idrix.eu";
   Path    : constant String := "secure";
   Port    : constant := 443;
begin
-- Trace_On (Every_Raise);
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
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Any,
         Sent     => GNAT.Sockets.Server.Trace_Any
      );
      Set_TLS_Tracing
      (  Factory => Factory,
         Session => True,
         Decoded => True
      );
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
            Put_Line (Get (Message));
            Put_Line ("<<<<<<<<<<<<<<<<<<<< Message");
         end;
         Put_Line ("HTTP client stopping");
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_HTTPS_OpenSSL_Client;
