--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ELV_MAX_Cube_Client.Topology                Spring, 2019       --
--  Interface                                                         --
--                                Last revision :  10:32 11 May 2019  --
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

package GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
        Topology is

   type Device_Topology_Data (Length : Natural) is record
      Kind_Of   : Device_Type;
      Address   : RF_Address := 0;
      Room      : Room_ID;
      Serial_No : String (1..10);
      Name      : String (1..Length);
   end record;
   type Device_Topology_Data_Ptr is access Device_Topology_Data;
   type Device_Topology_Data_Ptr_Array is
      array (Positive range <>) of Device_Topology_Data_Ptr;

   type Room_Topology_Data
        (  Length : Natural;
           Count  : Natural
        )  is
   record
      ID      : Room_ID;
      Master  : RF_Address;
      Name    : String (1..Length);
      Devices : Device_Topology_Data_Ptr_Array (1..Count);
   end record;
   type Room_Topology_Data_Ptr is access Room_Topology_Data;
   type Room_Topology_Data_Ptr_Array is
      array (Positive range <>) of Room_Topology_Data_Ptr;

   type Topology_Data
        (  Rooms_Count   : Natural;
           Devices_Count : Natural
        )  is new Ada.Finalization.Controlled with
   record
      Rooms   : Room_Topology_Data_Ptr_Array   (1..Rooms_Count);
      Devices : Device_Topology_Data_Ptr_Array (1..Devices_Count);
   end record;
   procedure Adjust   (Topology : in out Topology_Data);
   procedure Finalize (Topology : in out Topology_Data);
--
-- Get_Topology -- Get topology from metadata
--
--    Data - The metadata
--
-- Returns :
--
--    The topology
--
-- Exceptions :
--
--    Data_Error - Invalid metadata
--
   function Get_Topology (Data : String) return Topology_Data;

private
   type Room_Data is record
      Count : Natural := 0;
      Room  : Room_Topology_Data_Ptr;
   end record;
   package Room_Maps is
      new Generic_Map (Room_ID, Room_Data);
   package Device_Maps is
      new Generic_Map (Device_Key, Device_Topology_Data_Ptr);

end GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
        Topology;
