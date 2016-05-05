--                                                                    --
--  procedure Test_MQTT_Client      Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Spring, 2016       --
--                                                                    --
--                                Last revision :  20:01 04 Apr 2016  --
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
with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;
with GNAT.Sockets.MQTT.Streams;    use GNAT.Sockets.MQTT.Streams;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Test_MQTT_Clients;            use Test_MQTT_Clients;

with GNAT.Exception_Traces;

with Strings_Edit.Lexicographical_Order;
use  Strings_Edit.Lexicographical_Order;

procedure Test_MQTT_Client is
   Server_Address : constant String := "test.mosquitto.org";
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   declare
      Message : aliased MQTT_Message := Compose ("Test", "");
      Stream  : aliased MQTT_Stream (Message'Access);
   begin
      Erase (Stream);
      String'Output (Stream'Access, "Hello");
      String'Output (Stream'Access, " ");
      String'Output (Stream'Access, "World!");
      Rewind (Stream);
      if String'Input (Stream'Access) /= "Hello" then
         Raise_Exception
         (  Use_Error'Identity,
            "Error in readin 'Hello'"
         );
      end if;
      if String'Input (Stream'Access) /= " " then
         Raise_Exception
         (  Use_Error'Identity,
            "Error in readin ' '"
         );
      end if;
      if String'Input (Stream'Access) /= "World!" then
         Raise_Exception
         (  Use_Error'Identity,
            "Error in readin 'World!'"
         );
      end if;
   end;
   if not Match_Topic ("abc/def/gh", "+/+/+") then
      Raise_Exception
      (  Use_Error'Identity,
         "Error in abc/def/gh vs +/+/+"
      );
   end if;
   if not Match_Topic ("abc/c/def/s", "#") then
      Raise_Exception
      (  Use_Error'Identity,
         "Error in abc/c/def/s vs #"
      );
   end if;
   if not Match_Topic ("abc/c/def/s", "abc/c/#") then
      Raise_Exception
      (  Use_Error'Identity,
         "Error in abc/c/def/s vs abc/c/#"
      );
   end if;
   if not Match_Topic ("abc/c/def", "abc/+/def") then
      Raise_Exception
      (  Use_Error'Identity,
         "Error in abc/c/def vs abc/+/def"
      );
   end if;
   if Match_Topic ("abc/def", "abc/+/def") then
      Raise_Exception
      (  Use_Error'Identity,
         "Error in abc/def vs abc/+/def"
      );
   end if;
   if Check_Topic ("abc/def") then
      Raise_Exception (Use_Error'Identity, "Error in abc/def");
   end if;
   begin
      if Check_Topic ("/abc/def") then
         Raise_Exception (Use_Error'Identity, "Error in /abc/def");
      end if;
   exception
      when Constraint_Error =>
         null;
   end;
   begin
      if Check_Topic ("abc/def/") then
         Raise_Exception (Use_Error'Identity, "Error in abc/def/");
      end if;
   exception
      when Constraint_Error =>
         null;
   end;
   begin
      if not Check_Topic ("abc/+/def") then
         Raise_Exception (Use_Error'Identity, "Error in abc/+/def/");
      end if;
   exception
      when Constraint_Error =>
         null;
   end;
   if not Check_Topic ("abc/+/def") then
      Raise_Exception (Use_Error'Identity, "Error in abc/+/def");
   end if;
   declare
      Factory   : aliased Connections_Factory;
      Server    : aliased Connections_Server (Factory'Access, 0);
      Reference : Handle;
   begin
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Decoded,
         Sent     => GNAT.Sockets.Server.Trace_Decoded
      );
      Set
      (  Reference,
         new Test_Client
             (  Listener             => Server'Unchecked_Access,
                Input_Size           => 80,
                Output_Size          => 10, -- Deliberately small
                Max_Subscribe_Topics => 20
      )      );
      declare
         Client : Test_Client renames Test_Client (Ptr (Reference).all);

         procedure Test_1 is
         begin
            Set_Overlapped_Size (Client, 4); -- One response packet
            Connect
            (  Server,
               Client'Unchecked_Access,
               Server_Address,
               MQTT_Port
            );
            while not Is_Connected (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("MQTT client connected to " & Server_Address);
            Send_Connect (Client, "TestMQTTclient");
            delay 1.0;
            Send_Ping (Client);
            delay 1.0;
            Send_Subscribe
            (  Client,
               12,
               "$SYS/broker/uptime" / "$SYS/broker/load/#",
               (At_Least_Once, Exactly_Once)
            );
            delay 1.0;
            Send_Ping (Client);
            delay 5.0;
            Send_Unsubscribe (Client, 13, "$SYS/broker/uptime" / "??");
            delay 1.0;
            Send_Disconnect (Client);
            delay 1.0;
         end Test_1;

      begin
         Put_Line ("MQTT client test 1 started");
         Test_1;
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_MQTT_Client;
