--                                                                    --
--  package Test_MQTT_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2016       --
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

with Ada.Streams;               use Ada.Streams;
with GNAT.Sockets;              use GNAT.Sockets;
with GNAT.Sockets.MQTT.Server;  use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;       use GNAT.Sockets.Server;

package Test_MQTT_Servers is

   type Test_Factory is new Connections_Factory with record
      Server : aliased MQTT_Server;
   end record;
   function Create
            (  Factory  : access Test_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
   function Get_Client_Name
            (  Factory : Test_Factory;
               Client  : Connection'Class
            )  return String;
   function Get_IO_Timeout (Factory : Test_Factory)
      return Duration;

   type Test_Server is new MQTT_Connection with private;
   procedure Finalize (Client : in out Test_Server);
   function Get_Name (Client : Test_Server) return String;
   procedure Set_Name (Client : in out Test_Server; Name : String);

private
   type String_Ptr is access String;
   type Test_Server is new MQTT_Connection with record
      Name : String_Ptr;
   end record;

end Test_MQTT_Servers;
