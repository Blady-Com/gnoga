--                                                                    --
--  procedure Test_MQTT_Server      Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Spring, 2016       --
--                                                                    --
--                                Last revision :  15:46 28 May 2016  --
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
with GNAT.Sockets.MQTT.Server;     use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Test_MQTT_Servers;            use Test_MQTT_Servers;

with GNAT.Exception_Traces;

procedure Test_MQTT_Server is
   Try_Count : constant := 100_000;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   declare
      Factory : aliased Test_Factory;
      Server  : Connections_Server (Factory'Access, MQTT_Port);
   begin
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Decoded,
         Sent     => GNAT.Sockets.Server.Trace_Decoded
      );
      Set_Tracing_Flags (Factory.Server, Trace_All);
      for Index in 1..Try_Count loop
         delay 1.0;
         Publish (Factory.Server, "/test/count", Image (Index));
      end loop;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_MQTT_Server;
