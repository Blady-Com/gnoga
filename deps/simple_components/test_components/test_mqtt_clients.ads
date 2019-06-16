--                                                                    --
--  package Test_MQTT_Clients       Copyright (c)  Dmitry A. Kazakov  --
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

with Ada.Streams;        use Ada.Streams;
with GNAT.Sockets.MQTT;  use GNAT.Sockets.MQTT;

with Synchronization.Events;

package Test_MQTT_Clients is

   type Test_Client is new MQTT_Pier with private;
   procedure Finalize (Client : in out Test_Client);
   function Get_Name (Client : Test_Client) return String;
   procedure On_Connect_Accepted
             (  Pier            : in out Test_Client;
                Session_Present : Boolean
             );
   procedure On_Connect_Rejected
             (  Pier     : in out Test_Client;
                Response : Connect_Response
             );
   procedure On_Ping_Response (Pier : in out Test_Client);
   procedure On_Publish
             (  Pier      : in out Test_Client;
                Topic     : String;
                Message   : Stream_Element_Array;
                Packet    : Packet_Identification;
                Duplicate : Boolean;
                Retain    : Boolean
             );
   procedure On_Subscribe_Acknowledgement
             (  Pier   : in out Test_Client;
                Packet : Packet_Identifier;
                Codes  : Return_Code_List
             );
   procedure Set_Name (Pier : in out Test_Client; Name : String);
   procedure Reset_Event (Pier : in out Test_Client);
   procedure Wait_For_Event (Pier : in out Test_Client);
private
   use Synchronization.Events;

   type String_Ptr is access String;
   type Test_Client is new MQTT_Pier with record
      Name   : String_Ptr;
      Action : Event; -- Connected/subscribed
   end record;

end Test_MQTT_Clients;
