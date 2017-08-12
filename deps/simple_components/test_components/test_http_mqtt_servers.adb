--                                                                    --
--  package Test_HTTP_MQTT_Servers  Copyright (c)  Dmitry A. Kazakov  --
--  Test server                                    Luebeck            --
--  Implementation                                 Spring, 2017       --
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

with Ada.Streams.Stream_IO;  use Ada.Streams.Stream_IO;
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with GNAT.Sockets.Connection_State_Machine.HTTP_Server;
use  GNAT.Sockets.Connection_State_Machine.HTTP_Server;

package body Test_HTTP_MQTT_Servers is

   function Create
            (  Factory  : access Test_HTTP_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
      Result : Connection_Ptr;
   begin
      Trace (Factory.all, "Accepting HTTP connection");
      if Get_Clients_Count (Listener.all) < Factory.Max_Connections then
         Result :=
            new Test_HTTP_Client
                (  Listener => Listener.all'Unchecked_Access,
                   Factory  => Factory.Factory.all'Unchecked_Access,
                   Buffer_Size    => 800,
                   Request_Length => Factory.Request_Length,
                   Input_Size     => Factory.Input_Size,
                   Output_Size    => Factory.Output_Size
                );
         Receive_Body_Tracing   (Test_HTTP_Client (Result.all), True);
         Receive_Header_Tracing (Test_HTTP_Client (Result.all), True);
         return Result;
      else
         return null;
      end if;
   end Create;

   function Get_Protocols (Client : Test_HTTP_Client) return String is
   begin
      return "mqtt";
   end Get_Protocols;

end Test_HTTP_MQTT_Servers;
