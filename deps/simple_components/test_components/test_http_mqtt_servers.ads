--                                                                    --
--  package Test_HTTP_MQTT_Servers  Copyright (c)  Dmitry A. Kazakov  --
--  Test server                                    Luebeck            --
--  Interface                                      Spring, 2017       --
--                                                                    --
--                                Last revision :  18:49 10 Apr 2017  --
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
with Ada.Streams;                  use Ada.Streams;
with Ada.Streams.Stream_IO;        use Ada.Streams.Stream_IO;
with GNAT.Directory_Operations;    use GNAT.Directory_Operations;
with GNAT.Sockets;                 use GNAT.Sockets;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Synchronization.Events;       use Synchronization.Events;

with GNAT.Sockets.Connection_State_Machine.HTTP_Server.WebSocket_Server;

with Test_MQTT_Servers;

package Test_HTTP_MQTT_Servers is
   use GNAT.Sockets.Connection_State_Machine.HTTP_Server;
   use WebSocket_Server;
--
-- Test_HTTP_Factory -- A factory of HTTP connection objects
--
   type Test_HTTP_Factory
        (  Request_Length  : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length;
           Max_Connections : Positive;
           Factory         : access Test_MQTT_Servers.Test_Factory
        )  is new Connections_Factory with null record;
--
-- Create -- A HTTP connection object
--
   function Create
            (  Factory  : access Test_HTTP_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
--
-- Test_HTTP_Server -- A HTTP server listening HTTP connections
--
   type Test_HTTP_Server is new Connections_Server with null record;
--
-- Test_HTTP_Client -- HTTP  connection   object   that   accepts   MQTT
--                     connections over WebSockets
--
   type Test_HTTP_Client is new HTTP_WebSocket_Client with null record;
   function Get_Protocols (Client : Test_HTTP_Client) return String;

end Test_HTTP_MQTT_Servers;
