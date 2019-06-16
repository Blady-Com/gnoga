--                                                                    --
--  package Test_MQTT_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Ada.Unchecked_Deallocation;
with Test_MQTT_Clients;

package body Test_MQTT_Servers is

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   function Create
            (  Factory  : access Test_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
   begin
      return
         new Test_Server
             (  Server               => Factory.Server'Unchecked_Access,
                Listener             => Listener,
                Input_Size           => 80,
                Output_Size          => 80,
                Max_Subscribe_Topics => 20
             );
   end Create;

   procedure Finalize (Client : in out Test_Server) is
   begin
      Finalize (MQTT_Connection (Client));
      Free (Client.Name);
   end Finalize;

   function Get_Client_Name
            (  Factory : Test_Factory;
               Client  : Connection'Class
            )  return String is
      use Test_MQTT_Clients;
   begin
      if Client in Test_Client'Class then
         return Get_Name (Test_Client'Class (Client));
      elsif Client in Test_Server'Class then
         return Get_Name (Test_Server'Class (Client));
      else
         return GNAT.Sockets.Image (Get_Client_Address (Client));
      end if;
   end Get_Client_Name;

   function Get_IO_Timeout (Factory : Test_Factory)
      return Duration is
   begin
      return 2.0;
   end Get_IO_Timeout;

   function Get_Name (Client : Test_Server) return String is
   begin
      if Client.Name = null then
         return GNAT.Sockets.Image (Get_Client_Address (Client));
      else
         return Client.Name.all;
      end if;
   end Get_Name;

   procedure Set_Name (Client : in out Test_Server; Name : String) is
   begin
      Free (Client.Name);
      Client.Name := new String'(Name);
   end Set_Name;

end Test_MQTT_Servers;
