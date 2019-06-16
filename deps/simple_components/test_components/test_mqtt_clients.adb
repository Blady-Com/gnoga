--                                                                    --
--  package Test_MQTT_Clients       Copyright (c)  Dmitry A. Kazakov  --
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

with Ada.Text_IO;            use Ada.Text_IO;
with GNAT.Sockets.Server;    use GNAT.Sockets.Server;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Ada.Unchecked_Deallocation;

package body Test_MQTT_Clients is

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   procedure Finalize (Client : in out Test_Client) is
   begin
      Finalize (MQTT_Pier (Client));
      Free (Client.Name);
   end Finalize;

   function Get_Name (Client : Test_Client) return String is
   begin
      if Client.Name = null then
         return GNAT.Sockets.Image (Get_Client_Address (Client));
      else
         return Client.Name.all;
      end if;
   end Get_Name;

   procedure On_Connect_Accepted
             (  Pier            : in out Test_Client;
                Session_Present : Boolean
             )  is
   begin
      Put_Line ("Connect accepted");
      Pier.Action.Signal;
   end On_Connect_Accepted;

   procedure On_Connect_Rejected
             (  Pier     : in out Test_Client;
                Response : Connect_Response
             )  is
   begin
      Put_Line ("Connect rejected " & Image (Response));
      Pier.Action.Signal;
   end On_Connect_Rejected;

   procedure On_Ping_Response (Pier : in out Test_Client) is
   begin
      Put_Line ("Ping response");
   end On_Ping_Response;

   procedure On_Publish
             (  Pier      : in out Test_Client;
                Topic     : String;
                Message   : Stream_Element_Array;
                Packet    : Packet_Identification;
                Duplicate : Boolean;
                Retain    : Boolean
             )  is
   begin
      Put_Line ("Message " & Topic & "=" & Image (Message));
      On_Publish
      (  MQTT_Pier (Pier),
         Topic,
         Message,
         Packet,
         Duplicate,
         Retain
      );
      Pier.Action.Signal;
   end On_Publish;

   procedure On_Subscribe_Acknowledgement
             (  Pier   : in out Test_Client;
                Packet : Packet_Identifier;
                Codes  : Return_Code_List
             )  is
   begin
      Put ("Subscribed " & Image (Integer (Packet)) & ":");
      for Index in Codes'Range loop
         if Index /= Codes'First then
            Put (", ");
         end if;
         if Codes (Index).Success then
            Put (QoS_Level'Image (Codes (Index).QoS));
         else
            Put ("Failed");
         end if;
      end loop;
      New_Line;
      Pier.Action.Signal;
   end On_Subscribe_Acknowledgement;

   procedure Reset_Event (Pier : in out Test_Client) is
   begin
      Pier.Action.Reset;
   end Reset_Event;

   procedure Wait_For_Event (Pier : in out Test_Client) is
   begin
       select
          Pier.Action.Wait;
       or delay 5.0;
          raise Data_Error;
       end select;
   end Wait_For_Event;

   procedure Set_Name (Pier : in out Test_Client; Name : String) is
   begin
      Free (Pier.Name);
      Pier.Name := new String'(Name);
   end Set_Name;

end Test_MQTT_Clients;
