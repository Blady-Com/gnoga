--                                                                    --
--  procedure Test_HTTP_Client      Copyright (c)  Dmitry A. Kazakov  --
--  HTTP client test                               Luebeck            --
--                                                 Spring, 2015       --
--                                                                    --
--                                Last revision :  22:35 24 May 2015  --
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
with Ada.Text_IO.Text_Streams;     use Ada.Text_IO.Text_Streams;
--  with GNAT.Exception_Traces;        use GNAT.Exception_Traces;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.Streams;         use Strings_Edit.Streams;

with GNAT.Sockets.Connection_State_Machine.HTTP_Server;

with GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled;
use  GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled;

procedure Test_HTTP_Client is
   Port : constant := 80;
   procedure Dump_Headers (Session : HTTP_Session_Signaled) is
      use GNAT.Sockets.Connection_State_Machine.HTTP_Client;
   begin
      Put_Line ("Headers:");
      for Header in Text_Header'Range loop
         if Get_Response_Header (Session, Header) /= "" then
            Put_Line
            (  "   "
            &  Image (Header)
            &  ": "
            &  Get_Response_Header (Session, Header)
            );
          end if;
      end loop;
   end Dump_Headers;
begin
-- Trace_On (Every_Raise);
   declare
      Factory   : aliased Connections_Factory;
      Server    : aliased Connections_Server (Factory'Access, 0);
      Reference : Handle;
      Message   : aliased String_Stream (1024 * 100);
   begin
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Decoded,
         Sent     => GNAT.Sockets.Server.Trace_Decoded
      );
      Set
      (  Reference,
         new HTTP_Session_Signaled
             (  Server'Unchecked_Access,
                80,
                80,
                80
      )      );
      declare
         Client : HTTP_Session_Signaled renames
                  HTTP_Session_Signaled (Ptr (Reference).all);
         use GNAT.Sockets.Connection_State_Machine.HTTP_Server;

         procedure Test_Delete is
         begin
            Shutdown (Client);
            Rewind (Message);
            while not Is_Down (Client) loop
               delay 0.1;
            end loop;
            Connect
            (  Server,
               Client'Unchecked_Access,
               "httpbin.org",
               Port
            );
            while not Is_Connected (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("HTTP client connected to httpbin.org");
            Delete
            (  Client,
               "http://httpbin.org/test",
               Message'Unchecked_Access
            );
            while Is_Active (Client) loop -- Busy waiting for a response
               delay 0.1;
            end loop;
            Dump_Headers (Client);
            Put_Line
            (  Image (Get_Response_Code (Client))
            &  " "
            &  Get_Response_Reason (Client)
            &  " Message >>>>>>>>>>>>>>>>>>>>"
            );
            Put_Line (Get (Message));
            Put_Line ("<<<<<<<<<<<<<<<<<<<< Message");
         end Test_Delete;

         procedure Test_Head is
         begin
            Shutdown (Client);
            Rewind (Message);
            while not Is_Down (Client) loop
               delay 0.1;
            end loop;
            Connect
            (  Server,
               Client'Unchecked_Access,
               "httpbin.org",
               Port
            );
            while not Is_Connected (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("HTTP client connected to httpbin.org");
            Head (Client, "http://httpbin.org/head");
            while Is_Active (Client) loop -- Busy waiting for a response
               delay 0.1;
            end loop;
            Dump_Headers (Client);
         end Test_Head;

         procedure Test_Get_1 is
         begin
            Shutdown (Client);
            Rewind (Message);
            while not Is_Down (Client) loop
               delay 0.1;
            end loop;
            Connect
            (  Server,
               Client'Unchecked_Access,
               "httpbin.org",
               Port
            );
            while not Is_Connected (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("HTTP client connected to httpbin.org");
            Set_Request_Header (Client, Accept_Header, "text/plain");
            Get
            (  Client,
               "http://httpbin.org/get",
               Message'Unchecked_Access
            );
            while Is_Active (Client) loop -- Busy waiting for a response
               delay 0.1;
            end loop;
            Dump_Headers (Client);
            Put_Line
            (  Image (Get_Response_Code (Client))
            &  " "
            &  Get_Response_Reason (Client)
            &  " Message >>>>>>>>>>>>>>>>>>>>"
            );
            Put_Line (Get (Message));
            Put_Line ("<<<<<<<<<<<<<<<<<<<< Message");
         end Test_Get_1;

         procedure Test_Get_2 is
         begin
            Shutdown (Client);
            Rewind (Message);
            while not Is_Down (Client) loop
               delay 0.1;
            end loop;
            Connect
            (  Server,
               Client'Unchecked_Access,
               "127.0.0.1",
               Port
            );
            while not Is_Connected (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("HTTP client connected to 127.0.0.1");
            Get
            (  Client,
               "http://127.0.0.1/",
               Message'Unchecked_Access
            );
            while Is_Active (Client) loop -- Busy waiting for a response
               delay 0.1;
            end loop;
            Dump_Headers (Client);
            Put_Line
            (  Image (Get_Response_Code (Client))
            &  " "
            &  Get_Response_Reason (Client)
            &  " Message >>>>>>>>>>>>>>>>>>>>"
            );
            Put_Line (Get (Message));
            Put_Line ("<<<<<<<<<<<<<<<<<<<< Message");
         end Test_Get_2;

         procedure Test_Options is
         begin
            Shutdown (Client);
            Rewind (Message);
            while not Is_Down (Client) loop
               delay 0.1;
            end loop;
            Connect
            (  Server,
               Client'Unchecked_Access,
               "httpbin.org",
               Port
            );
            while not Is_Connected (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("HTTP client connected to httpbin.org");
            Options
            (  Client,
               "http://httpbin.org",
               Message'Unchecked_Access
            );
            while Is_Active (Client) loop -- Busy waiting for a response
               delay 0.1;
            end loop;
            Dump_Headers (Client);
            Put_Line
            (  Image (Get_Response_Code (Client))
            &  " "
            &  Get_Response_Reason (Client)
            &  " Message >>>>>>>>>>>>>>>>>>>>"
            );
            Put_Line (Get (Message));
            Put_Line ("<<<<<<<<<<<<<<<<<<<< Message");
         end Test_Options;

         procedure Test_Post_1 is
            File : File_Type;
         begin
            Open (File, In_File, "test_http_client.adb");
            Shutdown (Client);
            Rewind (Message);
            Connect (Client, "httpbin.org", Port);
            Put_Line ("HTTP client connected to httpbin.org");
            Post
            (  Session => Client,
               Name    => "http://httpbin.org/post",
               Content => "a=1&b=2",
               Message => Message'Unchecked_Access
            );
            while Is_Active (Client) loop -- Busy waiting for a response
               delay 0.1;
            end loop;
            Dump_Headers (Client);
            Put_Line
            (  Image (Get_Response_Code (Client))
            &  " "
            &  Get_Response_Reason (Client)
            &  " Message >>>>>>>>>>>>>>>>>>>>"
            );
            Put_Line (Get (Message));
            Put_Line ("<<<<<<<<<<<<<<<<<<<< Message");
            Close (File);
         end Test_Post_1;

         procedure Test_Put_1 is
            File : File_Type;
         begin
            Open (File, In_File, "test_http_client.adb");
            Shutdown (Client);
            Rewind (Message);
            Connect (Client, "httpbin.org", Port);
            Put_Line ("HTTP client connected to httpbin.org");
            Put
            (  Session => Client,
               Name    => "http://httpbin.org/put",
               Content => "1234567890",
               Message => Message'Unchecked_Access
            );
            Wait (Client, False);
            Dump_Headers (Client);
            Put_Line
            (  Image (Get_Response_Code (Client))
            &  " "
            &  Get_Response_Reason (Client)
            &  " Message >>>>>>>>>>>>>>>>>>>>"
            );
            Put_Line (Get (Message));
            Put_Line ("<<<<<<<<<<<<<<<<<<<< Message");
            Close (File);
         end Test_Put_1;

         procedure Test_Put_2 is
            File : File_Type;
         begin
            Open (File, In_File, "test_http_client.adb");
            Shutdown (Client);
            Rewind (Message);
            while not Is_Down (Client) loop
               delay 0.1;
            end loop;
            Connect
            (  Server,
               Client'Unchecked_Access,
               "httpbin.org",
               Port
            );
            while not Is_Connected (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("HTTP client connected to httpbin.org");
            Put
            (  Session => Client,
               Name    => "http://httpbin.org/put",
               Content => Stream (File),
               Message => Message'Unchecked_Access
            );
            while Is_Active (Client) loop -- Busy waiting for a response
               delay 0.1;
            end loop;
            Dump_Headers (Client);
            Put_Line
            (  Image (Get_Response_Code (Client))
            &  " "
            &  Get_Response_Reason (Client)
            &  " Message >>>>>>>>>>>>>>>>>>>>"
            );
            Put_Line (Get (Message));
            Put_Line ("<<<<<<<<<<<<<<<<<<<< Message");
            Close (File);
         end Test_Put_2;
      begin
--           Set_Request_Header
--           (  Client,
--              User_Agent_Header,
--              (  "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:37.0) "
--              &  "Gecko/20100101 Firefox/37.0"
--           )  );
--           Set_Request_Header
--           (  Client,
--              Accept_Header,
--              "text/html"
--           );
--           Set_Request_Header
--           (  Client,
--              Accept_Language_Header,
--              "en-US,en;q=0.5"
--           );
--           Set_Request_Header
--           (  Client,
--              Accept_Encoding_Header,
--              "deflate"
--           );
         Put_Line ("HTTP client started");
         Receive_Message_Tracing (Client, True);
--       Test_Head;
--       Test_Options;
--       Test_Delete;
         Test_Get_1;
--       Test_Get_2;
--       Test_Post_1;
--       Test_Put_1;
--       Test_Put_2;
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_HTTP_Client;
