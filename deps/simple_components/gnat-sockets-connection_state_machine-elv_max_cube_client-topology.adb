--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ELV_MAX_Cube_Client.Topology                Spring, 2019       --
--  Implementation                                                    --
--                                Last revision :  14:07 11 Nov 2019  --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
             Topology is
   use Device_Maps;
   use Room_Maps;

   procedure Clone (Devices : in out Device_Topology_Data_Ptr_Array) is
   begin
      for Index in Devices'Range loop
         Devices (Index) :=
            new Device_Topology_Data'(Devices (Index).all);
      end loop;
   end Clone;

   procedure Clone (Rooms : in out Room_Topology_Data_Ptr_Array) is
   begin
      for Index in Rooms'Range loop
         Rooms (Index) :=
            new Room_Topology_Data'(Rooms (Index).all);
         Clone (Rooms (Index).Devices);
      end loop;
   end Clone;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Device_Topology_Data,
             Device_Topology_Data_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Room_Topology_Data,
             Room_Topology_Data_Ptr
          );
   procedure Free (Devices : in out Device_Topology_Data_Ptr_Array) is
   begin
      for Index in Devices'Range loop
         Free (Devices (Index));
      end loop;
   end Free;

   procedure Free (Rooms : in out Room_Topology_Data_Ptr_Array) is
   begin
      for Index in Rooms'Range loop
         if Rooms (Index) /= null then
            Free (Rooms (Index).Devices);
            Free (Rooms (Index));
         end if;
      end loop;
   end Free;

   procedure Adjust (Topology : in out Topology_Data) is
   begin
      Clone (Topology.Rooms);
      Clone (Topology.Devices);
   end Adjust;

   procedure Finalize (Topology : in out Topology_Data) is
   begin
      Free (Topology.Rooms);
      Free (Topology.Devices);
   end Finalize;

   function Get_Topology (Data : String) return Topology_Data is

      function Get_Device
               (  Data    : String;
                  Pointer : access Integer;
                  No      : Positive
               )  return Device_Topology_Data is
         Index   : Integer := Pointer.all;
         Kind_Of : Device_Type;
         Name    : Integer;
         Serial  : Integer;
         Length  : Natural;
         Address : RF_Address;
         ID      : Room_ID;
      begin
         Get_Type (Data, Index, Kind_Of);
         Get_Address (Data, Index, Address);
         if Index + 9 > Data'Last then
            raise Data_Error with
                  "missing device serial number (device " &
                  Image (No) &
                  ')';
         end if;
         Serial := Index;
         Index  := Index + 10;
         Length := Character'Pos (Data (Index));
         Index  := Index + 1;
         Name   := Index;
         Index  := Index + Length;
         if Index - 1 > Data'Last then
            raise Data_Error with
                  "wrong name length (device " & Image (No) & ')';
         end if;
         if Index > Data'Last then
            raise Data_Error with
                  "missing room ID (device " & Image (No) & ')';
         end if;
         ID := Character'Pos (Data (Index));
         Pointer.all := Index + 1;
         return
         (  Length    => Length,
            Kind_Of   => Kind_Of,
            Address   => Address,
            Room      => ID,
            Serial_No => Data (Serial..Serial + 9),
            Name      => Data (Name..Name + Length - 1)
         );
      end Get_Device;

      function Get_Room
               (  Data    : String;
                  Pointer : access Integer;
                  No      : Positive
               )  return Room_Topology_Data is
         Index   : Integer := Pointer.all;
         ID      : Room_ID;
         Name    : Integer;
         Length  : Natural;
         Address : RF_Address;
      begin
         if Index > Data'Last then
            raise Data_Error with
                  "missing room ID (room " & Image (No) & ')';
         end if;
         ID    := Character'Pos (Data (Index));
         Index := Index + 1;
         if Index > Data'Last then
            raise Data_Error with
                  "missing room name length (room " & Image (No) & ')';
         end if;
         Length := Character'Pos (Data (Index));
         Index  := Index + 1;
         Name   := Index;
         Index  := Index + Length;
         if Index - 1 > Data'Last then
            raise Data_Error with
                  "wrong name length (room " & Image (No) & ')';
         end if;
         Get_Address (Data, Index, Address);
         Pointer.all := Index;
         return
         (  Length  => Length,
            Count   => 0,
            ID      => ID,
            Master  => Address,
            Name    => Data (Name..Name + Length - 1),
            Devices => (1..0 => null)
         );
      end Get_Room;

      Input : aliased Integer := Data'First;
      Count : Integer;
   begin
      if (  Data (Input) /= 'V'
         or else
            Data (Input + 1) /= Character'Val (2)
         )  then
         raise Data_Error with "malformed data header";
      end if;
      Input := Input + 2;
      if Input > Data'Last then
         return
         (  Ada.Finalization.Controlled
         with
            0, 0, (others => null), (others => null)
         );
      end if;
      Count := Character'Pos (Data (Input));
      Input := Input + 1;
      declare
         Rooms    : Room_Maps.Map;
         Roomless : Natural := 0;
         Devices  : Device_Maps.Map;
      begin
         Add (Rooms, No_Room, (0, null));
         for Index in 1..Count loop
            declare
               Room : Room_Topology_Data_Ptr :=
                  new Room_Topology_Data'
                      (  Get_Room (Data, Input'Access, Index)
                      );
            begin
               if Is_In (Rooms, Room.ID) then
                  Free (Room);
               else
                  Add (Rooms, Room.ID, (0, Room));
                  Room := null;
               end if;
            exception
               when others =>
                  Free (Room);
                  raise;
            end;
         end loop;
         if Input <= Data'Last then
            Count := Character'Pos (Data (Input));
            Input := Input + 1;
            for Index in 1..Count loop
               declare
                  Location : Integer;
                  Device   : Device_Topology_Data_Ptr :=
                         new Device_Topology_Data'
                             (  Get_Device (Data, Input'Access, Index)
                             );
               begin
                  Location := Find (Rooms, Device.Room);
                  if Location > 0 then
                     declare
                        This : Room_Data := Get (Rooms, Location);
                     begin
                        This.Count := This.Count + 1;
                        Add
                        (  Devices,
                           (Device.Room, This.Count),
                           Device
                        );
                        Device := null;
                        Replace (Rooms, Location, This);
                     end;
                  else
                     Free (Device);
                  end if;
               exception
                  when others =>
                     Free (Device);
                     raise;
               end;
            end loop;
         end if;
         for Index in reverse 2..Get_Size (Rooms) loop -- Empty rooms
            declare
               Data : Room_Data := Get (Rooms, Index);
            begin
               if Data.Count = 0 then
                  Free (Data.Room);
                  Remove (Rooms, Index);
               end if;
            end;
         end loop;
         declare
            Data : Room_Data := Get (Rooms, Positive'(1));
         begin
            Roomless := Data.Count;
            Free (Data.Room);
            Replace (Rooms, Positive'(1), Data);
         end;
         declare
            Result : Topology_Data (Get_Size (Rooms) - 1, Roomless);
         begin
            for Index in 2..Get_Size (Rooms) loop
               declare
                  Data : Room_Data := Get (Rooms, Index);
               begin
                  Result.Rooms (Index - 1) :=
                     new Room_Topology_Data'
                         (  Length  => Data.Room.Length,
                            Count   => Data.Count,
                            ID      => Data.Room.ID,
                            Master  => Data.Room.Master,
                            Name    => Data.Room.Name,
                            Devices => (others => null)
                         );
                  Free (Data.Room);
                  Replace (Rooms, Index, Data);
               end;
            end loop;
            for Index in 1..Get_Size (Devices) loop
               declare
                  Key : constant Device_Key := Get_Key (Devices, Index);
               begin
                  if Key.Room = No_Room then
                     Result.Devices (Key.Index) := Get (Devices, Index);
                  else
                     Result.Rooms
                     (  Find (Rooms, Key.Room) - 1
                     ) .Devices (Key.Index) := Get (Devices, Index);
                  end if;
                  Replace (Devices, Index, null);
               end;
            end loop;
            return Result;
         end;
      exception
         when others =>
            for Index in 1..Get_Size (Rooms) loop
               declare
                  Room : Room_Topology_Data_Ptr :=
                         Get (Rooms, Index).Room;
               begin
                  Free (Room);
               end;
            end loop;
            for Index in 1..Get_Size (Devices) loop
               declare
                  Device : Device_Topology_Data_Ptr :=
                           Get (Devices, Index);
               begin
                  Free (Device);
               end;
            end loop;
            raise;
      end;
   end Get_Topology;

end GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
        Topology;
