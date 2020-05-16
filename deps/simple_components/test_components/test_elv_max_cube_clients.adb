--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_ELV_MAX_Cube_Clients                   Luebeck            --
--  ELV MAX! Cube test client                      Summer, 2015       --
--                                                                    --
--                                Last revision :  13:13 14 Sep 2019  --
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

package body Test_ELV_MAX_Cube_Clients is

   procedure Configuration_Updated
             (  Client : in out Test_Client;
                Update : Update_Data
             )  is
      Dum_ID : Room_ID := No_Room;
   begin
      case Update.Kind_Of is
         when Cube_Update =>
            null;
         when Detached_Device_Update =>
            null;
         when Topology_Update =>
            null;
         when Device_Parameters_Update =>
            null;
         when Device_Discovery_Update =>
            Trace
            (  Client,
               (  Image (Update.Device)
               &  " "
               &  Image (Update.Address)
               &  " "
               &  Update.Serial_No
            )  );
            if Client.Test_Mode = Device_Pair_And_Add then
               Add_New_Device
               (  Client      => Client,
                  Room_Name   => "New room",
                  Kind_Of     => Update.Device,
                  Device_Name => "Thermostat",
                  Serial_No   => Update.Serial_No,
                  Address     => Update.Address,
                  ID          => Dum_ID
               );
            end if;
         when End_Discovery_Update =>
            null;
         when NTP_Servers_List_Update =>
            null;
      end case;
   end Configuration_Updated;

end Test_ELV_MAX_Cube_Clients;
