--                                                                    --
--  procedure Test_MQTT_Serial      Copyright (c)  Dmitry A. Kazakov  --
--  Test server and client                         Luebeck            --
--                                                 Winter, 2018       --
--                                                                    --
--                                Last revision :  14:04 26 Dec 2018  --
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
--
--  This  is a test of  a MQTT server and client  communicating  over  a
--  serial  connection.   It  illustrates  use  of  blocking  connection
--  servers.
--
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;
with GNAT.Sockets.MQTT;             use GNAT.Sockets.MQTT;
with GNAT.Sockets.MQTT.Server;      use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;           use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Blocking;  use GNAT.Sockets.Server.Blocking;
with GNAT.Sockets.Server.Handles;   use GNAT.Sockets.Server.Handles;
with GNAT.Serial_Communications;    use GNAT.Serial_Communications;
with Test_MQTT_Clients;             use Test_MQTT_Clients;
with Test_MQTT_Serials;             use Test_MQTT_Serials;
with Test_MQTT_Servers;             use Test_MQTT_Servers;

-- with GNAT.Exception_Traces;

procedure Test_MQTT_Serial is
   Port_1 : constant String := "COM2"; -- Change as appropriate to the
   Port_2 : constant String := "COM3"; -- actual COM devices

   Server_Port : aliased Serial_Port;
   Client_Port : aliased Serial_Port;
   Factory     : aliased Test_Factory;
   Server      : aliased MQTT_Serial_Server
                         (  Server_Port'Unchecked_Access,
                            Factory'Unchecked_Access,
                            Server_Port'Unchecked_Access,
                            Server_Port'Unchecked_Access,
                            2048
                         );
   Client      : aliased MQTT_Serial_Server
                         (  Client_Port'Unchecked_Access,
                            Factory'Unchecked_Access,
                            Client_Port'Unchecked_Access,
                            Client_Port'Unchecked_Access,
                            2048
                         );
begin
-- GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   Trace_On (Factory, Trace_Any, Trace_Any);
   Put_Line ("Opening port " & Port_1);
   Open (Server_Port, Port_Name (Port_1));
   Set (Server_Port, Block => False, Timeout => 1.0);
   Put_Line ("Opening port " & Port_2);
   Open (Client_Port, Port_Name (Port_2));
   Set (Client_Port, Block => False, Timeout => 1.0);
   Put_Line ("Connecting the server");
   Connect
   (  Server,
      new Test_Server
          (  Server               => Factory.Server'Unchecked_Access,
             Listener             => Server'Unchecked_Access,
             Input_Size           => 100,
             Output_Size          => 100,
             Max_Subscribe_Topics => 100
   )      );
   Set_Name (Test_Server'Class (Ptr (Get_Pier (Server)).all), Port_1);
   Put_Line ("Connecting the client");
   Connect
   (  Client,
      new Test_Client
          (  Listener             => Client'Unchecked_Access,
             Input_Size           => 100,
             Output_Size          => 100,
             Max_Subscribe_Topics => 100
   )      );
   Set_Name (Test_Client'Class (Ptr (Get_Pier (Client)).all), Port_2);
   delay 0.1;
   declare
      This   : constant Handle := Get_Pier (Client);
      Master : Test_Client'Class renames
               Test_Client'Class (Ptr (This).all);
   begin
      Put_Line ("Connect");
      Send_Connect
      (  Pier   => Master,
         Client => "test"
      );
      Put_Line ("Wait for connection...");
      Wait_For_Event (Pier => Master);

      Put_Line ("Subscribing to ""chat""");
      Reset_Event (Pier => Master);
      Send_Subscribe
      (  Pier   => Master,
         Packet => 10,
         Topic  => "chat",
         QoS    => At_Most_Once
      );
      Put_Line ("Wait for subscribtion response...");
      Wait_For_Event (Pier => Master);

      Put_Line ("Publishing chat=Hello!");
      Reset_Event (Pier => Master);
      Publish
      (  Server  => Factory.Server,
         Topic   => "chat",
         Message => "Hello!",
         Policy  => Updated
      );
      Put_Line ("Wait for publisher...");
      Wait_For_Event (Pier => Master);

      Put_Line ("Publishing chat=Hello again");
      Reset_Event (Pier => Master);
      Publish
      (  Server  => Factory.Server,
         Topic   => "chat",
         Message => "Hello again",
         Policy  => Updated
      );
      Put_Line ("Wait for publisher...");
      Wait_For_Event (Pier => Master);

      Put_Line ("Publishing chat=Going to sleep...");
      Reset_Event (Pier => Master);
      Publish
      (  Server  => Factory.Server,
         Topic   => "chat",
         Message => "Going to sleep...",
         Policy  => Updated
      );
      Put_Line ("Wait for publisher...");
      Wait_For_Event (Pier => Master);

      Put_Line ("Publishing chat=Bye!");
      Reset_Event (Pier => Master);
      Publish
      (  Server  => Factory.Server,
         Topic   => "chat",
         Message => "Bye!",
         Policy  => Updated
      );
      Put_Line ("Wait for publisher...");
      Wait_For_Event (Pier => Master);

      Send_Unsubscribe
      (  Pier   => Master,
         Packet => 11,
         Topics => +"chat"
      );
      Send_Disconnect (Pier => Master);
      delay 2.0;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_MQTT_Serial;
