--                                                                    --
--  procedure Test_MQTT_WebServer   Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Spring, 2017       --
--                                                                    --
--                                Last revision :  21:11 16 Apr 2017  --
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
with GNAT.Sockets.MQTT.Server;     use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Test_HTTP_MQTT_Servers;       use Test_HTTP_MQTT_Servers;
with Test_MQTT_Servers;            use Test_MQTT_Servers;

with GNAT.Exception_Traces;

procedure Test_MQTT_WebServer is
   Try_Count : constant := 30;
   Port      : constant := 8080;
begin
-- GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   declare
      MQTT_Factory : aliased Test_Factory;
      HTTP_Factory : aliased Test_HTTP_Factory
                             (  Request_Length  => 200,
                                Input_Size      => 1024,
                                Output_Size     => 1024,
                                Max_Connections => 100,
                                Factory         => MQTT_Factory'Access
                             );
--    Server  : GNAT.Sockets.Server.Pooled.Pooled_Server (Factory'Access, Port, Tasks);
      HTTP_Server : Test_HTTP_Server (HTTP_Factory'Access, Port);
   begin
      Set_Tracing_Flags (MQTT_Factory.Server, Trace_All);
      Trace_On
      (  Factory  => MQTT_Factory,
         Received => GNAT.Sockets.Server.Trace_Any,
         Sent     => GNAT.Sockets.Server.Trace_Any
      );
      Trace_On
      (  Factory  => HTTP_Factory,
         Received => GNAT.Sockets.Server.Trace_Any,
         Sent     => GNAT.Sockets.Server.Trace_Any
      );
      Put_Line ("HTTP server started");
      for Index in 1..Try_Count loop
         delay 1.0;
         Put_Line
         (  "   Publish "
         &  Image (Index)
         &  '/'
         &  Image (Try_Count)
         );
         Publish (MQTT_Factory.Server, "test/count", Image (Index));
      end loop;
      Put_Line ("HTTP server stopping");
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_MQTT_WebServer;
