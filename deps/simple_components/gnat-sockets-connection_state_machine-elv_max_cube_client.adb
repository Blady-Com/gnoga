--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ELV_MAX_Cube_Client                         Summer, 2015       --
--  Implementation                                                    --
--                                Last revision :  13:32 28 Jan 2022  --
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
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Base64;    use Strings_Edit.Base64;
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Characters.Handling;

package body GNAT.Sockets.Connection_State_Machine.
             ELV_MAX_Cube_Client is
   use Stream_Element_Offset_Edit;
   use Device_Handles;
   use Room_Handles;

   Max_M_Blocks : constant Integer := 255;
   Busy_Error   : constant String :=
                           "Cube is busy (cannot send data right now)";
   Epoch        : constant Time :=
                     Ada.Calendar.Formatting.Time_Of (2000, 01, 01);

--     procedure Dump (Topology : Devices_Configuration; Prefix : String) is
--        use Ada.Text_IO;
--
--        function Image (Room : Room_ID) return String is
--        begin
--           return "#" & Image (Integer (Room));
--        end Image;
--
--        function Image (Key : Device_Key) return String is
--        begin
--           return Image (Key.Room) & "." & Image (Key.Index);
--        end Image;
--
--        function Image (Room : Room_Handles.Handle) return String is
--        begin
--           if Is_Valid (Room) then
--              return Image (Ptr (Room).ID);
--           else
--              return "null";
--           end if;
--        end Image;
--      begin
--        Put_Line (Prefix & "Rooms " & Image (Get_Size (Topology.Rooms)));
--        for Index in 1..Get_Size (Topology.Rooms) loop
--           declare
--              This : Room_Descriptor'Class renames
--                     Ptr (Get (Topology.Rooms, Index)).all;
--           begin
--              Put_Line
--              (  Prefix
--              &  "  "
--              &  Image (Get_Key (Topology.Rooms, Index))
--              &  "->"
--              &  Image (This.ID)
--              &  " "
--              &  Image (This.Master)
--              &  " "
--              &  Quote (This.Name)
--              &  " count "
--              &  Image (This.Count)
--              );
--           end;
--        end loop;
--        Put_Line
--        (  Prefix
--        &  "Devices "
--        &  Image (Get_Size (Topology.Devices))
--        );
--        for Index in 1..Get_Size (Topology.Devices) loop
--           declare
--              This : Device_Descriptor'Class renames
--                     Ptr (Get (Topology.Devices, Index)).all;
--           begin
--              Put_Line
--              (  Prefix
--              &  "  "
--              &  Image (Get_Key (Topology.Devices, Index))
--              &  "->"
--              &  Image (This.Data.Address)
--              &  " "
--              &  Image (This.Kind_Of)
--              &  " "
--              &  Quote (This.Data.Name)
--              &  " in "
--              &  Image (This.Data.Room)
--              &  "->"
--              &  Image (This.Room)
--              );
--           end;
--        end loop;
--        Put_Line (Prefix & "RF " & Image (Get_Size (Topology.RF)));
--        for Index in 1..Get_Size (Topology.RF) loop
--           Put_Line
--           (  Prefix
--           &  "  "
--           &  Image (Get_Key (Topology.RF, Index))
--           &  "->"
--           &  Image (Get (Topology.RF, Index))
--           );
--        end loop;
--     end Dump;

   CRLF : constant String := Character'Val (13) & Character'Val (10);
   Five_Minutes : constant := 60 * 5;
   Chunk_Size   : constant := 1_900 * 3 / 4; -- Metadata block size
--
-- 16#XX# Room modes
--    00 All rooms
--    04 Specific room
--
-- 16#XX# Command modes
--    10 Thermostat Week Program
--    11 Configure  a thermostat  (eco  temperature,   maximum  setpoint
--       temperature, minimum  setpoint temperature, temperature offset,
--       window open temperature, window open duration)
--    12 Configure a valve  (Boost, Decalcification,  Max Valve Setting,
--       Valve Offset)
--    40 Set Temperature (and Mode)
--    43 Set To Comfort Temperature
--    44 Set To Eco Temperature
--    82 Set Display Actual Temperature (wall thermostat only)

   subtype Command_Head is String (1..6);
   S_Head : constant Command_Head :=
                     (  Character'Val (16#00#)
                     &  Character'Val (16#04#) -- Room mode
                     &  Character'Val (16#40#) -- Command mode
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     );
   S_Group : constant Command_Head :=
                     (  Character'Val (16#00#)
                     &  Character'Val (16#00#) -- Room mode
                     &  Character'Val (16#22#) -- Command mode
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     );
   S_Ungroup : constant Command_Head :=
                     (  Character'Val (16#00#)
                     &  Character'Val (16#04#) -- Room mode
                     &  Character'Val (16#23#) -- Command mode
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     );
   S_Mode : constant Command_Head :=
                     (  Character'Val (16#00#)
                     &  Character'Val (16#04#) -- Room mode
                     &  Character'Val (16#82#) -- Command mode
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     );
   type Command_Array is array (Boolean) of Command_Head;
   S_Link : constant Command_Array :=
                  (  (  Character'Val (16#00#)
                     &  Character'Val (16#00#) -- Room mode
                     &  Character'Val (16#20#) -- Command mode
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     ),
                     (  Character'Val (16#00#)
                     &  Character'Val (16#04#) -- Room mode
                     &  Character'Val (16#20#) -- Command mode
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                  )  );
   S_Unlink : constant Command_Array :=
                  (  (  Character'Val (16#00#)
                     &  Character'Val (16#00#) -- Room mode
                     &  Character'Val (16#21#) -- Command mode
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     ),
                     (  Character'Val (16#00#)
                     &  Character'Val (16#04#) -- Room mode
                     &  Character'Val (16#21#) -- Command mode
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                     &  Character'Val (16#00#)
                  )  );

   function Encode (Temperature : Centigrade) return Unsigned_8;
   function Encode (Day : Week_Day) return Integer;
   function Encode (Address : RF_Address) return String;
   function Encode (Date : Time) return String;
   function Encode (Mode : Operating_Mode) return Character;
   function Encode (Kind_Of : Partner_Device_Type) return Character;
   function Encode (Room : Room_ID) return Character;

   type Disposition is
        (  Creating_Socket,
           Setting_Reuse,
           Setting_Broadcast,
           Setting_Timeout,
           Getting_Host_By_Name,
           Getting_Address_From_Name,
           Binding_Socket,
           Sending_Socket,
           Receiving_From_Socket,
           Closing_Socket
        );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  String_Buffer,
             String_Buffer_Ptr
          );
   generic
      with procedure Flush_Buffer (Buffer : String);
   procedure Get_Metadata_Unchecked
             (  Client  : ELV_MAX_Cube_Client'Class;
                Data    : in out String;
                Exclude : RF_Address := 0
             );

   function "&" (Left, Right : Device_Type) return Boolean is
   begin
      case Left is
         when Cube | Unknown =>
            return False;
         when Radiator_Thermostat | Radiator_Thermostat_Plus =>
            return Right in Wall_Thermostat..Shutter_Contact;
         when Wall_Thermostat =>
            case Right is
               when Radiator_Thermostat      |
                    Radiator_Thermostat_Plus |
                    Shutter_Contact          =>
                  return True;
               when others =>
                  return False;
            end case;
         when Shutter_Contact =>
            return Right in Radiator_Thermostat..Wall_Thermostat;
         when Eco_Button =>
            return False;
      end case;
   end "&";

   function Image2 (Value : Integer) return String is
      Result  : String (1..2);
      Pointer : Integer := 1;
   begin
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Value,
         Field       => 2,
         Justify     => Right,
         Fill        => '0'
      );
      return Result;
   end Image2;

   procedure Get_Metadata_Unchecked
             (  Client  : ELV_MAX_Cube_Client'Class;
                Data    : in out String;
                Exclude : RF_Address := 0
             )  is
      Topology : Devices_Configuration renames Client.Topology;
      Pointer  : Integer := Data'First;
      Count    : Natural := 0;
      Location : Room_ID := No_Room; -- Excluded device's room

      procedure Flush is
      begin
         if Pointer > Data'First then
            Flush_Buffer (Data (Data'First..Pointer - 1));
            Count   := Count + 1;
            Pointer := Data'First;
         end if;
      end Flush;

      procedure Put (Value : Character) is
      begin
         if Pointer >= Data'Last then
            Flush;
         end if;
         Data (Pointer) := Value;
         Pointer := Pointer + 1;
      end Put;

      procedure Put (Value : RF_Address) is
      begin
         if Pointer + 2 >= Data'Last then
            Flush;
         end if;
         Data (Pointer    ) := Character'Val (Value / 2**16);
         Data (Pointer + 1) := Character'Val ((Value / 2**8) mod 256);
         Data (Pointer + 2) := Character'Val (Value mod 256);
         Pointer := Pointer + 3;
      end Put;

      procedure Put (Value : String) is
      begin
         if Pointer + Value'Length - 1 >= Data'Last then
            Flush;
         end if;
         Data (Pointer..Pointer + Value'Length - 1) := Value;
         Pointer := Pointer + Value'Length;
      end Put;
   begin
      if Exclude /= 0 then
         declare
            Index : Integer := Find (Topology.RF, Exclude);
         begin
            if Index > 0 then
               Location :=
                  Ptr
                  (  Get
                     (  Topology.Devices,
                        Get (Topology.RF, Index)
                  )  ) .Data.Room;
               Index := Find (Topology.Rooms, Location);
               if (  Index = 0
                 or else
                     Ptr (Get (Topology.Rooms, Index)).Count > 1
                  )  then
                  Location := No_Room; -- Keep the room
               end if;
            end if;
         end;
      end if;

      Put ('V');
      Put (Character'Val (2));
      Put (Character'Val (Get_Size (Topology.Rooms)));
      for Index in 1..Get_Size (Topology.Rooms) loop
         declare
            Room : Room_Descriptor'Class renames
                   Ptr (Get (Topology.Rooms, Index)).all;
         begin
            if Room.Count > 1 or else Room.ID /= Location then
               Put (Character'Val (Room.ID));
               Put (Character'Val (Room.Name_Length));
               Put (Room.Name);
               Put (Room.Master);
            end if;
         end;
      end loop;
      Put (Character'Val (Get_Size (Topology.Devices)));
      for Index in 1..Get_Size (Topology.Devices) loop
         declare
            Device : Device_Descriptor'Class renames
                     Ptr (Get (Topology.Devices, Index)).all;
            Parameters : Device_Parameters renames Device.Data;
         begin
            if Device.Data.Address /= Exclude then
               Put (Character'Val (Device_Type'Pos (Device.Kind_Of)));
               Put (Parameters.Address);
               Put (Parameters.Serial_No);
               Put (Character'Val (Parameters.Name_Length));
               Put (Parameters.Name);
               Put (Character'Val (Parameters.Room));
            end if;
         end;
      end loop;
      Put (Character'Val (1));
      Flush;
   end Get_Metadata_Unchecked;

   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Room_Name   : String;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                ID          : in out Room_ID;
                S_Commands  : out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      S_Commands := 0;
      Add_New_Device_Unchecked
      (  Client      => Client,
         Room_Name   => Room_Name,
         Kind_Of     => Kind_Of,
         Device_Name => Device_Name,
         Serial_No   => Serial_No,
         Address     => Address,
         ID          => ID,
         S_Commands  => S_Commands,
         Mode        => Mode
      );
   end Add_New_Device;

   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Room_Name   : String;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                ID          : in out Room_ID;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      S_Commands : Natural := 0;
      Lock : Topology_Holder (Client'Access);
   begin
      Add_New_Device_Unchecked
      (  Client      => Client,
         Room_Name   => Room_Name,
         Kind_Of     => Kind_Of,
         Device_Name => Device_Name,
         Serial_No   => Serial_No,
         Address     => Address,
         ID          => ID,
         S_Commands  => S_Commands,
         Mode        => Mode
      );
   end Add_New_Device;

   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                S_Commands  : out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      S_Commands := 0;
      Add_New_Device_Unchecked
      (  Client      => Client,
         Device_Name => Device_Name,
         Serial_No   => Serial_No,
         Address     => Address,
         S_Commands  => S_Commands,
         Mode        => Mode
      );
   end Add_New_Device;

   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      S_Commands : Natural := 0;
      Lock       : Topology_Holder (Client'Access);
   begin
      Add_New_Device_Unchecked
      (  Client      => Client,
         Device_Name => Device_Name,
         Serial_No   => Serial_No,
         Address     => Address,
         S_Commands  => S_Commands,
         Mode        => Mode
      );
   end Add_New_Device;

   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                S_Commands  : out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
      Room : Room_Descriptor'Class renames
             Ptr (Get (Client.Topology.Rooms, Index)).all;
   begin
      S_Commands := 0;
      Add_New_Device_Unchecked
      (  Client      => Client,
         Room        => Room.ID,
         Master      => Room.Master,
         Kind_Of     => Kind_Of,
         Device_Name => Device_Name,
         Serial_No   => Serial_No,
         Address     => Address,
         S_Commands  => S_Commands,
         Mode        => Mode
      );
   end Add_New_Device;

   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      S_Commands : Natural := 0;
      Lock       : Topology_Holder (Client'Access);
      Room       : Room_Descriptor'Class renames
                   Ptr (Get (Client.Topology.Rooms, Index)).all;
   begin
      Add_New_Device_Unchecked
      (  Client      => Client,
         Room        => Room.ID,
         Master      => Room.Master,
         Kind_Of     => Kind_Of,
         Device_Name => Device_Name,
         Serial_No   => Serial_No,
         Address     => Address,
         S_Commands  => S_Commands,
         Mode        => Mode
      );
   end Add_New_Device;

   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                ID          : Room_ID;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                S_Commands  : out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
      Room : Room_Descriptor'Class renames
             Ptr
             (  Get
                (  Client.Topology.Rooms,
                   Find_Room_Unchecked (Client, ID)
             )  ) .all;
   begin
      S_Commands := 0;
      Add_New_Device_Unchecked
      (  Client      => Client,
         Room        => Room.ID,
         Master      => Room.Master,
         Kind_Of     => Kind_Of,
         Device_Name => Device_Name,
         Serial_No   => Serial_No,
         Address     => Address,
         S_Commands  => S_Commands,
         Mode        => Mode
      );
   end Add_New_Device;

   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                ID          : Room_ID;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      S_Commands : Natural := 0;
      Lock       : Topology_Holder (Client'Access);
      Room       : Room_Descriptor'Class renames
                   Ptr
                   (  Get
                      (  Client.Topology.Rooms,
                         Find_Room_Unchecked (Client, ID)
                   )  ) .all;
   begin
      Add_New_Device_Unchecked
      (  Client      => Client,
         Room        => Room.ID,
         Master      => Room.Master,
         Kind_Of     => Kind_Of,
         Device_Name => Device_Name,
         Serial_No   => Serial_No,
         Address     => Address,
         S_Commands  => S_Commands,
         Mode        => Mode
      );
   end Add_New_Device;

   procedure Add_New_Device_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Room        : Room_ID;
                Master      : RF_Address;
                Kind_Of     : Partner_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                S_Commands  : in out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Devices : Device_Maps.Map  renames Client.Topology.Devices;
      Rooms   : Rooms_Maps.Map   renames Client.Topology.Rooms;
      Index   : constant Integer := Find (Rooms, Room);
      Device  : Device_Handles.Handle;
   begin
      if Is_In (Client.Topology.RF, Address) then
         Raise_Exception
         (  Name_Error'Identity,
            "A device " & Image (Address) & " already exists"
         );
      elsif Device_Name'Length > 255 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The device name is longer than 255 characters"
         );
      end if;
      if 0 /= (Mode and S_Command) then
         if Index > 0 then
            declare
               Location : Room_Descriptor'Class renames
                          Get (Rooms, Index).Ptr.all;
            begin
               if Location.Count >= 255 then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "The number of devices in a room cannot exceed 255"
                  );
               end if;
               Link_Unchecked -- The room already exists
               (  Client,
                  Kind_Of,
                  Address,
                  Location,
                  Is_New_Master (Client, Kind_Of, Location),
                  S_Commands
               );
            end;
         end if;
         Send
         (  Client,  -- Set group address for the new device
            (  "s:"
            &  To_Base64
               (  S_Group
               &  Encode (Address)
               &  Character'Val (0)
               &  Encode (Room)
               )
            &  CRLF
         )  );
         S_Commands := S_Commands + 1;
      end if;
      if 0 /= (Mode and S_Response) then
         if Index <= 0 then
            Raise_Exception
            (  Constraint_Error'Identity,
               "The room " & Image (Integer (Room)) & " does not exist"
            );
         end if;
         Set
         (  Device,
            new Device_Descriptor (Kind_Of, Device_Name'Length)
         );
         declare
            Location   : Room_Descriptor'Class renames
                         Get (Rooms, Index).Ptr.all;
            Descriptor : Device_Descriptor'Class renames
                         Ptr (Device).all;
         begin
            Descriptor.Room           := Get (Rooms, Index);
            Descriptor.Data.Room      := Room;
            Descriptor.Data.Address   := Address;
            Descriptor.Data.Serial_No := Serial_No;
            Descriptor.Data.Name      := Device_Name;
            Location.Count := Location.Count + 1;
            Add (Devices, (Location.ID, Location.Count), Device);
            if Is_New_Master (Client, Kind_Of, Location) then
               Location.Master := Address;
            end if;
         end;
         Build_Reference (Client.Topology);
         Send
         (  Client,  -- Request the device configuration
            "c:" & Image (Address) & CRLF
         );
         Set_Metadata_Unchecked (Client);
      end if;
   end Add_New_Device_Unchecked;

   procedure Add_New_Device_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Room_Name   : String;
                Kind_Of     : Partner_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                ID          : in out Room_ID;
                S_Commands  : in out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Room : Room_Handles.Handle;
   begin
      if Is_In (Client.Topology.RF, Address) then
         Raise_Exception
         (  Name_Error'Identity,
            "A device " & Image (Address) & " already exists"
         );
      elsif Device_Name'Length > 255 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The device name is longer than 255 characters"
         );
      elsif Room_Name'Length > 255 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The room name is longer than 255 characters"
         );
      end if;
      if 0 /= (Mode and S_Command) then -- Create room
         declare
            Topology : Devices_Configuration renames Client.Topology;
            Used_IDs : array (1..Room_ID'Last) of Boolean :=
                          (others => False);
         begin
            for Index in 1..Get_Size (Topology.Rooms) loop
               declare
                  This : Room_Descriptor'Class renames
                         Ptr (Get (Topology.Rooms, Index)).all;
               begin
                  if This.Name = Room_Name then -- Here is a room with
                     ID := This.ID;             -- this name
                     Add_New_Device_Unchecked
                     (  Client      => Client,
                        Room        => This.ID,
                        Master      => This.Master,
                        Kind_Of     => Kind_Of,
                        Serial_No   => Serial_No,
                        Device_Name => Device_Name,
                        Address     => Address,
                        S_Commands  => S_Commands,
                        Mode        => Mode
                     );
                     return;
                  end if;
                  Used_IDs (This.ID) := True;
               end;
            end loop;
            ID := 0;
            for Index in Used_IDs'Range loop
               if not Used_IDs (Index) then
                  ID := Index;
                  exit;
               end if;
            end loop;
            if ID = 0 then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "The cube cannot have more than "
                  &  Image (Integer (Room_ID'Last))
                  &  " rooms"
               )  );
            end if;
         end;
      end if;
      if 0 /= (Mode and S_Response) then
         declare
            Index : constant Integer :=
                             Find (Client.Topology.Rooms, ID);
         begin
            if Index <= 0 then -- A room created with the ID
               Set (Room, new Room_Descriptor (Room_Name'Length));
               declare
                  Descriptor : Room_Descriptor'Class renames
                               Ptr (Room).all;
               begin
                  Descriptor.ID    := ID;
                  Descriptor.Count := 0;
                  Descriptor.Name  := Room_Name;
                  if Kind_Of in Radiator_Thermostat
                             .. Wall_Thermostat then
                     Descriptor.Master := Address;
                  else
                     Descriptor.Master := 0;
                  end if;
               end;
               Add (Client.Topology.Rooms, ID, Room);
            end if;
         end;
      end if;
      Add_New_Device_Unchecked
      (  Client      => Client,
         Room        => ID,
         Master      => Address,
         Kind_Of     => Kind_Of,
         Serial_No   => Serial_No,
         Device_Name => Device_Name,
         Address     => Address,
         Mode        => Mode,
         S_Commands  => S_Commands
      );
   end Add_New_Device_Unchecked;

   procedure Add_New_Device_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                S_Commands  : in out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Device : Device_Handles.Handle;
   begin
      if Is_In (Client.Topology.RF, Address) then
         Raise_Exception
         (  Name_Error'Identity,
            "A device " & Image (Address) & " already exists"
         );
      elsif Device_Name'Length > 255 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The device name is longer than 255 characters"
         );
      elsif Client.Roomless >= 255 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The number of roomless devices cannot exceed 255"
         );
      end if;
      if 0 /= (Mode and S_Response) then
         Set
         (  Device,
            new Device_Descriptor (Eco_Button, Device_Name'Length)
         );
         declare
            Descriptor : Device_Descriptor'Class renames
                         Ptr (Device).all;
         begin
            Descriptor.Data.Room      := No_Room;
            Descriptor.Data.Address   := Address;
            Descriptor.Data.Serial_No := Serial_No;
            Descriptor.Data.Name      := Device_Name;
         end;
         Client.Roomless := Client.Roomless + 1;
         Add
         (  Client.Topology.Devices,
            (No_Room, Client.Roomless),
            Device
         );
         Build_Reference (Client.Topology);
         Send  -- Request the device configuration
         (  Client,
            "c:" & Image (Address) & CRLF
         );
         Set_Metadata_Unchecked (Client);
      end if;
   end Add_New_Device_Unchecked;

   procedure Attach_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Device     : RF_Address;
                Index      : Positive;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      S_Commands := 0;
      Attach_Device_Unchecked
      (  Client,
         Device,
         Get (Client.Topology.Rooms, Index),
         S_Commands,
         Mode
      );
   end Attach_Device;

   procedure Attach_Device
             (  Client : in out ELV_MAX_Cube_Client;
                Device : RF_Address;
                Index  : Positive;
                Mode   : Setting_Mode := S_Command or S_Response
             )  is
      S_Commands : Natural := 0;
      Lock       : Topology_Holder (Client'Access);
   begin
      Attach_Device_Unchecked
      (  Client,
         Device,
         Get (Client.Topology.Rooms, Index),
         S_Commands,
         Mode
      );
   end Attach_Device;

   procedure Attach_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Device     : RF_Address;
                ID         : Room_ID;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      S_Commands := 0;
      Attach_Device_Unchecked
      (  Client,
         Device,
         Get (Client.Topology.Rooms, Find_Room_Unchecked (Client, ID)),
         S_Commands,
         Mode
      );
   end Attach_Device;

   procedure Attach_Device
             (  Client : in out ELV_MAX_Cube_Client;
                Device : RF_Address;
                ID     : Room_ID;
                Mode   : Setting_Mode := S_Command or S_Response
             )  is
      S_Commands : Natural := 0;
      Lock       : Topology_Holder (Client'Access);
   begin
      Attach_Device_Unchecked
      (  Client,
         Device,
         Get (Client.Topology.Rooms, Find_Room_Unchecked (Client, ID)),
         S_Commands,
         Mode
      );
   end Attach_Device;

   procedure Attach_Device
             (  Client : in out ELV_MAX_Cube_Client;
                Device : RF_Address;
                Name   : String;
                Mode   : Setting_Mode := S_Command or S_Response
             )  is
      S_Commands : Natural := 0;
   begin
      Attach_Device (Client, Device, Name, S_Commands, Mode);
   end Attach_Device;

   procedure Attach_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Device     : RF_Address;
                Name       : String;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             )  is
      Lock  : Topology_Holder (Client'Access);
      Rooms : Rooms_Maps.Map renames Client.Topology.Rooms;
   begin
      S_Commands := 0;
      for Index in 1..Get_Size (Rooms) loop
         if Ptr (Get (Rooms, Index)).Name = Name then -- The room exists
            Attach_Device_Unchecked
            (  Client     => Client,
               Address    => Device,
               To         => Get (Rooms, Index),
               S_Commands => S_Commands,
               Mode       => Mode
            );
            return;
         end if;
      end loop;
      declare
         Index : constant Integer :=
                          Find (Client.Topology.Detached, Device);
      begin
         if Index <= 0 then
            Raise_Exception
            (  End_Error'Identity,
               "There is no detached device " & Image (Device)
            );
         end if;
         declare
            Item : constant Device_Handles.Handle :=
                            Get (Client.Topology.Detached, Index);
            This : Device_Descriptor'Class renames Ptr (Item).all;
         begin
            Add_New_Device_Unchecked
            (  Client      => Client,
               Room_Name   => Name,
               Kind_Of     => This.Kind_Of,
               Serial_No   => This.Data.Serial_No,
               Device_Name => This.Data.Name,
               Address     => Device,
               ID          => This.Data.Room,
               S_Commands  => S_Commands,
               Mode        => Mode
            );
         end;
         if 0 /= (S_Response and Mode) then
            Remove (Client.Topology.Detached, Index);
         end if;
      end;
   end Attach_Device;

   procedure Attach_Device_Unchecked
             (  Client     : in out ELV_MAX_Cube_Client;
                Address    : RF_Address;
                To         : Room_Handles.Handle;
                S_Commands : in out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             )  is
      Devices : Device_Maps.Map renames Client.Topology.Devices;
      Index   : constant Integer :=
                         Find (Client.Topology.Detached, Address);
      Room    : Room_Descriptor'Class renames Ptr (To).all;
   begin
      if Index <= 0 then
         Raise_Exception
         (  End_Error'Identity,
            "There is no detached device " & Image (Address)
         );
      elsif Room.Count >= 255 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The number of devices in a room cannot exceed 255"
         );
      end if;
      declare
         Item   : constant Device_Handles.Handle :=
                           Get (Client.Topology.Detached, Index);
         Device : Device_Descriptor'Class renames Ptr (Item).all;
      begin
         if 0 /= (S_Command and Mode) then
            Link_Unchecked
            (  Client,
               Ptr (Get (Client.Topology.Detached, Index)).Kind_Of,
               Address,
               Room,
               Is_New_Master (Client, Device.Kind_Of, Room),
               S_Commands
            );
            Send
            (  Client,  -- Change group address for the moved device
               (  "s:"
               &  To_Base64
                  (  S_Group
                  &  Encode (Address)
                  &  Character'Val (0)
                  &  Encode (Room.ID)
                  )
               &  CRLF
            )  );
            S_Commands := S_Commands + 1;
         end if;
         if 0 /= (S_Response and Mode) then
            Room.Count := Room.Count + 1;
            Add (Devices, (Room.ID, Room.Count), Item);
            if Is_New_Master (Client, Device.Kind_Of, Room) then
               Room.Master := Address;
            end if;
            Device.Room := To;
            Device.Data.Room := Room.ID;
            Remove (Client.Topology.Detached, Index);
            Build_Reference (Client.Topology);
            Set_Metadata_Unchecked (Client);
         end if;
      end;
   end Attach_Device_Unchecked;

   procedure Build_Reference
             (  Topology : in out Devices_Configuration
             )  is
      RF     : Address_Maps.Map renames Topology.RF;
      Devices : Device_Maps.Map renames Topology.Devices;
   begin
      Erase (RF);
      for Index in 1..Get_Size (Devices) loop
         Add
         (  RF,
            Ptr (Get (Devices, Index)).Data.Address,
            Index
         );
      end loop;
   end Build_Reference;

   procedure Cancel_Pairing
             (  Client  : in out ELV_MAX_Cube_Client
             )  is
   begin
      Send (Client, "x:" & CRLF);
   end Cancel_Pairing;

   procedure Get_Duration
             (  Line    : String;
                Pointer : in out Integer;
                Value   : out Duration
             );
   procedure Get_Temperature
             (  Line    : String;
                Pointer : in out Integer;
                Value   : out Centigrade
             );

   procedure Clean_Up
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Full   : Boolean := True
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      if Full then
         Client.Ready := 0;
      end if;
      Client.Roomless := 0;
      Erase (Client.Topology.RF);
      Erase (Client.Topology.Devices);
      Erase (Client.Topology.Rooms);
   end Clean_Up;

   procedure Configuration_Updated
             (  Client : in out ELV_MAX_Cube_Client;
                Update : Update_Data
             )  is
     use Servers_List;
   begin
      case Update.Kind_Of is
         when Cube_Update              |
              Topology_Update          |
              Detached_Device_Update   |
              Device_Parameters_Update =>
            null;
         when NTP_Servers_List_Update =>
            for Index in 1..Get_Size (Update.NTP_Servers_List) loop
               Trace
               (  ELV_MAX_Cube_Client'Class (Client),
                  (  "NTP server "
                  &  Image (Index)
                  &  "/"
                  &  Image (Get_Size (Update.NTP_Servers_List))
                  &  ": "
                  &  Get (Update.NTP_Servers_List, Index)
               )  );
            end loop;
         when Device_Discovery_Update =>
            Trace
            (  ELV_MAX_Cube_Client'Class (Client),
               (  "Device found: "
               &  Image (Update.Device)
               &  " "
               &  Image (Update.Address)
               &  " S/N "
               &  Update.Serial_No
            )  );
         when End_Discovery_Update =>
            Trace
            (  ELV_MAX_Cube_Client'Class (Client),
               "Device discovery finished"
            );
      end case;
   end Configuration_Updated;

   function Create_List (Text : String) return Servers_List.Set is
      use Servers_List;
      Start : Integer := Text'First;
      List  : Servers_List.Set;
      procedure Add (Pointer : Integer) is
         Name : constant String := Trim (Text (Start..Pointer - 1));
      begin
         if Name'Length > 0 then
            Add (List, Name);
         end if;
      end Add;
   begin
      for Pointer in Text'Range loop
         if Text (Pointer) = ',' then
            Add (Pointer);
            Start := Pointer + 1;
         end if;
      end loop;
      Add (Text'Last + 1);
      return List;
   end Create_List;

   procedure Data_Received
             (  Client : in out ELV_MAX_Cube_Client;
                Data   : Device_Data
             )  is
   begin
      Trace (ELV_MAX_Cube_Client'Class (Client), Image (Data));
   end Data_Received;

   function Decode (Value : Natural) return Week_Day is
   begin
      case Value is
         when 0      => return Sa;
         when 1      => return Su;
         when 2      => return Mo;
         when 3      => return Tu;
         when 4      => return We;
         when 5      => return Th;
         when others => return Fr;
      end case;
   end Decode;

   procedure Delete
             (  Client     : in out ELV_MAX_Cube_Client;
                List       : RF_Address_Array;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             )  is
      Lock      : Topology_Holder (Client'Access);
      Device    : Device_Handles.Handle;
      Addresses : constant RF_Address_Array :=
                           Sort_Unchecked (Client, List);
      Chain     : String (1..Addresses'Length * 3);
      RF        : Address_Maps.Map renames Client.Topology.RF;
   begin
      S_Commands := 0;
      if Addresses'Length = 0  then
         return;
      end if;
      declare
         Pointer : Integer := 1;
      begin
         for Index in Addresses'Range loop
            Chain (Pointer..Pointer + 2) := Encode (Addresses (Index));
            Pointer := Pointer + 3;
         end loop;
      end;
      --
      -- Deleting devices
      --
      for Index in Addresses'Range loop
         Detach_Device_Unchecked
         (  Client,
            Addresses (Index),
            False,
            Device,
            S_Commands,
            Mode
         );
      end loop;
      if 0 /= (Mode and S_Command) then
         Send
         (  Client,
            (  "t:"
            &  Image2 (Addresses'Length)
            &  ",1,"
            &  To_Base64 (Chain)
            &  CRLF
         )  );
      end if;
      if 0 /= (Mode and S_Response) then
         Build_Reference (Client.Topology);
         Set_Metadata (Client);
      end if;
   end Delete;

   procedure Delete
             (  Client : in out ELV_MAX_Cube_Client;
                List   : RF_Address_Array;
                Mode   : Setting_Mode := S_Command or S_Response
             )  is
      S_Command : Natural;
   begin
      Delete (Client, List, S_Command, Mode);
   end Delete;

   procedure Delete_Device
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address;
                Mode    : Setting_Mode := S_Command or S_Response
             )  is
      S_Commands : Natural;
      Lock       : Topology_Holder (Client'Access);
      Index      : constant Positive :=
                           Get_Device_Unchecked (Client, Address);
   begin
      Delete (Client, (1 => Address), S_Commands, Mode);
   end Delete_Device;

   procedure Delete_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Address    : RF_Address;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             )  is
      Lock  : Topology_Holder (Client'Access);
      Index : constant Positive :=
                       Get_Device_Unchecked (Client, Address);
   begin
      Delete (Client, (1 => Address), S_Commands, Mode);
   end Delete_Device;

   procedure Delete_Room
             (  Client     : in out ELV_MAX_Cube_Client;
                Index      : Positive;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             )  is
      Lock       : Topology_Holder (Client'Access);
      Room       : constant Room_Handles.Handle :=
                            Get (Client.Topology.Rooms, Index);
      Descriptor : Room_Descriptor'Class renames Ptr (Room).all;
      Devices    : Device_Maps.Map renames Client.Topology.Devices;
      List       : RF_Address_Array (1..Descriptor.Count);
   begin
      for Index in List'Range loop
         List (Index) :=
            Ptr (Get (Devices, (Descriptor.ID, Index))).Data.Address;
      end loop;
      Delete (Client, List, S_Commands, Mode);
   end Delete_Room;

   procedure Delete_Room
             (  Client : in out ELV_MAX_Cube_Client;
                Index  : Positive;
                Mode   : Setting_Mode := S_Command or S_Response
             )  is
      S_Commands : Natural;
   begin
      Delete_Room (Client, Index, S_Commands, Mode);
   end Delete_Room;

   procedure Delete_Room
             (  Client     : in out ELV_MAX_Cube_Client;
                ID         : Room_ID;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Delete_Room
      (  Client     => Client,
         Index      => Find_Room_Unchecked (Client, ID),
         S_Commands => S_Commands,
         Mode       => Mode
      );
   end Delete_Room;

   procedure Delete_Room
             (  Client : in out ELV_MAX_Cube_Client;
                ID     : Room_ID;
                Mode   : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Delete_Room (Client, Find_Room_Unchecked (Client, ID), Mode);
   end Delete_Room;

   procedure Detach_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Device     : RF_Address;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             )  is
      Lock     : Topology_Holder (Client'Access);
      Index    : constant Integer := Find (Client.Topology.RF, Device);
      Detached : Device_Handles.Handle;
   begin
      if Index <= 0 then
         Raise_Exception
         (  End_Error'Identity,
            "There is no device " & Image (Device)
         );
      elsif (  Ptr (Get (Client.Topology.Devices, Index)).Data.Room
            =  No_Room
            )  then
         Raise_Exception
         (  Status_Error'Identity,
            "The " & Image (Device) & " is not located in any room"
         );
      end if;
      S_Commands := 0;
      Detach_Device_Unchecked
      (  Client,
         Device,
         True,
         Detached,
         S_Commands,
         Mode
      );
      if Is_Valid (Detached) then
         Replace (Client.Topology.Detached, Device, Detached);
      end if;
   end Detach_Device;

   procedure Detach_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Device     : RF_Address;
                Mode       : Setting_Mode := S_Command or S_Response
             )  is
      S_Commands : Natural := 0;
   begin
      Detach_Device (Client, Device, S_Commands, Mode);
   end Detach_Device;

   procedure Detach_Device_Unchecked
             (  Client       : in out ELV_MAX_Cube_Client;
                Address      : RF_Address;
                Set_Metadata : Boolean;
                Device       : out Device_Handles.Handle;
                S_Commands   : in out Natural;
                Mode         : Setting_Mode := S_Command or S_Response
             )  is
      Devices      : Device_Maps.Map  renames Client.Topology.Devices;
      Rooms        : Rooms_Maps.Map   renames Client.Topology.Rooms;
      RF           : Address_Maps.Map renames Client.Topology.RF;
      Kind_Of      : Device_Type;
      RF_Index     : constant Integer := Find (RF, Address);
      Device_Index : Integer;
      Room         : Room_Handles.Handle;
   --
   -- Delete_From_Devices -- Delete the device from the devices maps
   --
   --    Room - The room ID (No_Room if outside)
   --
      procedure Delete_From_Devices (Room : Room_ID) is
         This : Device_Handles.Handle;
         Key  : Device_Key;
      begin
            -- Delete from devices
         Remove (Devices, Device_Index);
            -- Fixing the keys with the higher positions
         for Next in Device_Index..Get_Size (Devices) loop
            Key := Get_Key (Devices, Next);
            exit when Key.Room /= Room;
            Key.Index := Key.Index - 1;
            This := Get (Devices, Next);
            Remove (Devices, Next);
            Replace (Devices, Key, This);
         end loop;
            -- Fixing the address map
         for Index in reverse 1..Get_Size (RF) loop
            declare
               Position : constant Integer := Get (RF, Index);
            begin
               if Position = Device_Index then -- Removed device
                  Remove (RF, Index);
               elsif Position > Device_Index then -- Moved device
                  Replace (RF, Index, Position - 1);
               end if;
            end;
         end loop;
      end Delete_From_Devices;
   begin
      if RF_Index <= 0 then
          return;
      end if;
      Device_Index := Get (RF, RF_Index);
      Device       := Get (Devices, Device_Index);
      declare
         This : Device_Descriptor'Class renames Ptr (Device).all;
      begin
         Kind_Of := This.Kind_Of;
         Room    := This.Room;
      end;
      if 0 /= (Mode and S_Command) then
         if Is_Valid (Room) then
            declare
               Location : Room_Descriptor'Class renames Ptr (Room).all;
            begin
               Wake_Up_Unchecked (Client, Location.ID);
               Send
               (  Client,  -- Remove the group address for the device
                  (  "s:"
                  &  To_Base64
                     (  S_Ungroup
                     &  Encode (Address)
                     &  Character'Val (0)
                     &  Encode (Location.ID)
                     )
                  &  CRLF
               )  );
               S_Commands := S_Commands + 1;
               Unlink_Unchecked
               (  Client,
                  Kind_Of,
                  Address,
                  Location,
                  S_Commands
               );
            end;
         end if;
      end if;
      if 0 /= (Mode and S_Response) then
         if Is_Valid (Room) then
            declare
               Location : Room_Descriptor'Class renames Ptr (Room).all;
            begin
               Delete_From_Devices (Location.ID);
               if Location.Count > 1 then
                  Location.Count := Location.Count - 1;
                  if Location.Master = Address then
                     --
                     -- It was the master device
                     --
                     Location.Master := 0;
                  end if;
               else -- Delete the room
                  Remove (Rooms, Location.ID);
               end if;
            end;
         else
            Delete_From_Devices (No_Room);
            Client.Roomless := Client.Roomless - 1;
         end if;
         if Set_Metadata then
            Build_Reference (Client.Topology);
            Set_Metadata_Unchecked (Client, Address);
         end if;
      end if;
   end Detach_Device_Unchecked;

   procedure Disconnected (Client : in out ELV_MAX_Cube_Client) is
   begin
      Clean_Up (Client);
   end Disconnected;

   function Discover
            (  Timeout  : Timeval_Duration := 2.0;
               Attempts : Positive  := 2;
               Host     : String    := "";
               Port     : Port_Type := 23272
            )  return Cube_Descriptor_Array is
      Announce : constant Stream_Element_Array (1..19) :=
                  (  16#65#, 16#51#, 16#33#, 16#4D#, 16#61#, 16#78#,
                     16#2A#, 16#00#, 16#2A#, 16#2A#, 16#2A#, 16#2A#,
                     16#2A#, 16#2A#, 16#2A#, 16#2A#, 16#2A#, 16#2A#,
                     16#49#
                  );
      Action  : Disposition := Creating_Socket;
      Address : aliased Sock_Addr_Type;

      function Image return String is
      begin
         case Action is
            when Creating_Socket =>
               return "creating socket";
            when Setting_Reuse =>
               return "setting socket reuse flag";
            when Setting_Broadcast =>
               return "setting socket broadcast flag";
            when Setting_Timeout =>
               return "setting socket timeout";
            when Getting_Host_By_Name =>
               return "getting the host name from " & Host_Name;
            when Getting_Address_From_Name =>
               return "getting address from host name " & Host;
            when Binding_Socket =>
               return "binding the socket to " & Image (Address);
            when Sending_Socket =>
               return "sending data into the socket";
            when Receiving_From_Socket =>
               return "receiving from the socket";
            when Closing_Socket =>
               return "closing the socket";
         end case;
      end Image;

      Data      : Stream_Element_Array  (1..100);
      Result    : Cube_Descriptor_Array (1..255);
      Count     : Natural           := 0;
      Step      : constant Duration := Timeout / Attempts;
      Start     : constant Time     := Clock;
      Last      : Stream_Element_Offset;
      Cube      : Sock_Addr_Type;
      Socket    : Socket_Type := No_Socket;
   begin
      Create_Socket (Socket, Family_Inet, Socket_Datagram);
      Action := Setting_Reuse;
      Set_Socket_Option
      (  Socket,
         Socket_Level,
         (Reuse_Address, True)
      );
      Action := Setting_Broadcast;
      Set_Socket_Option
      (  Socket,
         Socket_Level,
         (Broadcast, True)
      );
      Action := Setting_Timeout;
      Set_Socket_Option
      (  Socket,
         Socket_Level,
         (Receive_Timeout, Step)
      );
      Set_Socket_Option
      (  Socket,
         Socket_Level,
         (Send_Timeout, Step)
      );
      if Host = "" then
         Action := Getting_Host_By_Name;
         Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
      else
         Action := Getting_Address_From_Name;
         Address.Addr := To_Addr (Host);
      end if;
      Address.Port := Port;
      Action := Binding_Socket;
      Bind_Socket (Socket, Address);
      Address.Addr := Broadcast_Inet_Addr;
      Address.Port := Port;
      for Try in 1..Attempts loop
         exit when Count = Result'Last;
         Action := Sending_Socket;
         begin
            Send_Socket (Socket, Announce, Last, Address'Access);
         exception
            when Socket_Error =>
               goto Receive_End;
         end;
         while (Clock - Start) < Try * Step loop
            begin
               Action := Receiving_From_Socket;
               Receive_Socket (Socket, Data, Last, Cube);
               if Last = 26 then
                  declare
                     New_Cube : Boolean := True;
                  begin
                     for Index in 1..Count loop
                        if Result (Index).Address = Cube.Addr then
                           New_Cube := False;
                        end if;
                     end loop;
                     if New_Cube then
                        Count := Count + 1;
                        declare
                           This : Cube_Descriptor renames
                                  Result (Count);
                        begin
                           This.Address   := Cube.Addr;
                           This.Name      := To_String (Data (1..8));
                           This.Serial_No := To_String (Data (9..18));
                        end;
                        exit when Count = Result'Last;
                     end if;
                  end;
               end if;
            exception
               when Socket_Error =>
                  null;
            end;
         end loop;
         <<Receive_End>> null;
      end loop;
      Action := Closing_Socket;
      Close_Socket (Socket);
      return Result (1..Count);
   exception
      when Error : others =>
         if Socket /= No_Socket then
            begin
               Close_Socket (Socket);
            exception
               when others =>
                  null;
            end;
         end if;
         Raise_Exception
         (  Exception_Identity (Error),
            Exception_Message (Error) & ", when " & Image
         );
   end Discover;

   function Dump (Value : String) return String is
      Result  : String (1..Value'Length * 3 - 1);
      Pointer : Integer := 1;
   begin
      for Index in Value'Range loop
         if Index > Value'First then
            Result (Pointer) := ' ';
            Pointer := Pointer + 1;
         end if;
         Put
         (  Destination => Result,
            Pointer     => Pointer,
            Value       => Integer (Character'Pos (Value (Index))),
            Base        => 16,
            Fill        => '0',
            Justify     => Right,
            Field       => 2
         );
      end loop;
      return Result (1..Pointer - 1);
   end Dump;

   function Encode (Temperature : Centigrade) return Unsigned_8 is
      Value : constant Integer := Integer (Float (Temperature) * 2.0);
   begin
      if Value <= 0 then
         return 0;
      elsif Value >= 2#0011_1111# then
         return 2#0011_1111#;
      else
         return Unsigned_8 (Value);
      end if;
   end Encode;

   function Encode (Day : Week_Day) return Integer is
   begin
      case Day is
         when Mo => return 2;
         when Tu => return 3;
         when We => return 4;
         when Th => return 5;
         when Fr => return 6;
         when Sa => return 0;
         when Su => return 1;
      end case;
   end Encode;

   function Encode (Address : RF_Address) return String is
   begin
      return
      (  Character'Val ( Address / 2**16         )
      &  Character'Val ((Address / 2**8) mod 2**8)
      &  Character'Val ( Address         mod 2**8)
      );
   end Encode;

   function Encode (Date : Time) return String is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
      Result  : String (1..3);
   begin
      Split (Date, Year, Month, Day, Seconds);
      Result (1) := Character'Val ((Month / 2) * 2**5 + Day);
      Result (2) := Character'Val ((Month mod 2) * 2**7 + Year - 2000);
      Result (3) := Character'Val (Integer (Seconds) / (30 * 60));
      return Result;
   end Encode;

   function Encode (Mode : Operating_Mode) return Character is
   begin
      case Mode is
         when Automatic => return Character'Val (0);
         when Manual    => return Character'Val (1);
         when Vacation  => return Character'Val (2);
         when Boost     => return Character'Val (3);
      end case;
   end Encode;

   function Encode (Time : Duration; Valve : Ratio) return Character is
      Value : Integer;
   begin
      if Time >= 3600.0 then
         Value := 7; -- Special value 1 hour
      else
         Value := Integer (Time / 300.0);
         if Value = 7 then
            Value := 6;
         end if;
      end if;
      Value := Value * 32 + Integer (Float (Valve) * 20.0);
      return Character'Val (Value);
   end Encode;

   function Encode (Kind_Of : Partner_Device_Type) return Character is
   begin
      case Kind_Of is
         when Radiator_Thermostat =>
            return Character'Val (1);
         when Radiator_Thermostat_Plus =>
            return Character'Val (2);
         when Wall_Thermostat =>
            return Character'Val (3);
         when Shutter_Contact =>
            return Character'Val (4);
         when Eco_Button =>
            return Character'Val (5);
      end case;
   end Encode;

   function Encode (Room : Room_ID) return Character is
   begin
      return Character'Val (Room);
   end Encode;

   procedure Faulty_Device_Received
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Length      : Natural;
                Error       : Boolean;
                Initialized : Boolean;
                Orphaned    : Boolean
             )  is
      function Is_Initialized return String is
      begin
         if not Initialized then
            return ", uninitialized";
         else
            return "";
         end if;
      end Is_Initialized;

      function Is_Error return String is
      begin
         if Error then
            return ", error";
         else
            return "";
         end if;
      end Is_Error;

      function Is_Orphaned return String is
      begin
         if Orphaned then
            return ", orphaned";
         else
            return "";
         end if;
      end Is_Orphaned;

   begin
      Trace
      (  ELV_MAX_Cube_Client'Class (Client),
         (  "Faulty device "
         &  Image (Address)
         &  ", data length "
         &  Image (Length)
         &  Is_Error
         &  Is_Initialized
         &  Is_Orphaned
      )  );
   end Faulty_Device_Received;

   procedure Finalize (Lock : in out Topology_Holder) is
   begin
      if not Lock.Released then
         Lock.Released := True;
         Lock.Client.Topology.Self.Lock.Release;
      end if;
   end Finalize;

   procedure Finalize (Client : in out ELV_MAX_Cube_Client) is
   begin
      Free (Client.Metadata);
      Finalize (State_Machine (Client));
   end Finalize;

   function Find_Room
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return Positive is
      Lock : Topology_Holder (Client'Access);
   begin
      return Find_Room_Unchecked (Client, ID);
   end Find_Room;

   function Find_Room_Unchecked
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return Positive is
      Result : constant Integer := Find (Client.Topology.Rooms, ID);
   begin
      if Result <= 0 then
         Raise_Exception
         (  End_Error'Identity,
            "No such room"
         );
      else
         return Result;
      end if;
   end Find_Room_Unchecked;

   procedure Get_A
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Line   : String
             )  is
   begin
      begin
         Configuration_Updated (Client, (Kind_Of => Topology_Update));
      exception
         when others =>
            null;
      end;
   end Get_A;

   procedure Get_Address
             (  Line    : String;
                Pointer : in out Integer;
                Address : out RF_Address
             )  is
   begin
      if Pointer + 2 > Line'Last then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid response, missing device RF address"
         );
      end if;
      Address := Character'Pos (Line (Pointer    )) * 2**16
               + Character'Pos (Line (Pointer + 1)) * 2**8
               + Character'Pos (Line (Pointer + 2));
      Pointer := Pointer + 3;
   end Get_Address;

   procedure Get_C
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Line   : String
             )  is
      procedure Get_Day
                (  Device  : in out Device_Descriptor'Class;
                   Data    : String;
                   Pointer : in out Integer;
                   List    : in out Points_List;
                   Day     : Week_Day
                )  is
      begin
         for Point in List'Range loop
            declare
               This  : Set_Point renames List (Point);
               Value : Duration;
               Point : constant Integer :=
                       (  Character'Pos (Data (Pointer    )) * 256
                       +  Character'Pos (Data (Pointer + 1))
                       );
            begin
               This.Point := Centigrade (Float (Point / 2**9) / 2.0);
               Value      := Duration (Point mod 2**9) * 300.0;
               if Value > Day_Duration'Last then
                  This.Last := Day_Duration'Last;
               else
                  This.Last := Value;
               end if;
               Pointer := Pointer + 2;
            end;
         end loop;
         declare -- Truncate list
            Last  : constant Day_Duration := List (List'Last).Last;
            Count : Point_Count  := List'Last;
         begin
            while Count > 1 and then List (Count - 1).Last = Last loop
               Count := Count - 1;
            end loop;
            Device.Data.Schedule (Day) := (Count, List (1..Count));
         end;
      end Get_Day;

      procedure Get_Thermostat_Data
                (  Data    : String;
                   Pointer : in out Integer;
                   Device  : in out Device_Descriptor'Class
                )  is
         List : Points_List (1..Point_Count'Last);
      begin
         Get_Temperature (Data, Pointer, Device.Data.Comfort);
         Get_Temperature (Data, Pointer, Device.Data.Eco);
         Get_Temperature (Data, Pointer, Device.Data.Max);
         Get_Temperature (Data, Pointer, Device.Data.Min);
         Get_Temperature (Data, Pointer, Device.Data.Offset);
         Device.Data.Offset := Device.Data.Offset - 3.5;
         Get_Temperature (Data, Pointer, Device.Data.Window_Open);
         Get_Duration    (Data, Pointer, Device.Data.Window_Time);
         if Pointer + 4 + 13 * 2 * 7 - 1 > Line'Last then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid C-response, schedule"
            );
         end if;
         declare
            Value : constant Integer :=
                             Character'Pos (Data (Pointer)) / 2**5;
         begin
            if Value = 7 then
               Device.Data.Boost_Time := 3600.0;
            else
               Device.Data.Boost_Time := Duration (Value) * 300.0;
            end if;
         end;
         Device.Data.Boost_Valve :=
            Ratio
            (  Float (Character'Pos (Data (Pointer)) mod 2**5) * 0.05
            );
         Pointer := Pointer + 1;
         Device.Data.Decalcification.Day :=
            Decode (Character'Pos (Data (Pointer)) / 2**5);
         Device.Data.Decalcification.Time :=
            Duration (Character'Pos (Data (Pointer)) mod 2**5) * 3600.0;
         Pointer := Pointer + 1;
         Device.Data.Max_Valve :=
            Ratio
            (  Float (Character'Pos (Data (Pointer))) / 255.0
            );
         Pointer := Pointer + 1;
         Device.Data.Valve_Offset :=
            Ratio
            (  Float (Character'Pos (Data (Pointer))) / 255.0
            );
         Pointer := Pointer + 1;
         for Index in 0..6 loop
            Get_Day (Device, Data, Pointer, List, Decode (Index));
         end loop;
      end Get_Thermostat_Data;

      procedure Get_Wall_Thermostat_Data
                (  Data    : String;
                   Pointer : in out Integer;
                   Device  : in out Device_Descriptor'Class
                )  is
         List : Points_List (1..Point_Count'Last);
      begin
         Get_Temperature (Data, Pointer, Device.Data.Comfort);
         Get_Temperature (Data, Pointer, Device.Data.Eco);
         Get_Temperature (Data, Pointer, Device.Data.Max);
         Get_Temperature (Data, Pointer, Device.Data.Min);
         for Index in 0..6 loop
            Get_Day (Device, Data, Pointer, List, Decode (Index));
         end loop;
         Get_Temperature (Data, Pointer, Device.Data.Offset);
         Device.Data.Offset := Device.Data.Offset - 3.5;
         Get_Temperature (Data, Pointer, Device.Data.Window_Open);
      end Get_Wall_Thermostat_Data;

      Update  : Update_Type;
      Pointer : Integer := Line'First;
      Address : RF_Address;
   begin
      if Pointer > Line'Last then -- Empty C-response
         return;
      end if;
      begin
         Get
         (  Source  => Line,
            Pointer => Pointer,
            Value   => Integer (Address),
            Base    => 16
         );
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid C-response, missing radio frequency address"
            );
      end;
      Get_Comma (Line, Pointer);
      declare
         Data    : constant String :=
                   From_Base64 (Line (Pointer..Line'Last));
         Pointer : Integer := Data'First;
         Kind_Of : Device_Type;
         Address : RF_Address;
         ID      : Room_ID;
         Length  : Integer;
      begin
         if Pointer > Data'Length then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid C-response, missing length field"
            );
         end if;
         Length  := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         if Length /= Data'Length - 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid C-response, wrong length "
               &  Image (Length)
               &  " received "
               &  Image (Integer (Data'Length))
            )  );
         elsif Length < 17 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid C-response, wrong length "
               &  Image (Length)
               &  " < 17"
            )  );
         end if;
         Get_Address (Data, Pointer, Address);
         Get_Type (Data, Pointer, Kind_Of);
         ID := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1 + 1 + 1 + 10;
         declare
            Lock  : Topology_Holder (Client'Access);
            Index : constant Integer :=
                             Find (Client.Topology.RF, Address);
         begin
            if Index <= 0 then
               Update := Detached_Device_Update;
            else
               Update := Device_Parameters_Update;
               declare
                  Device : Device_Descriptor'Class renames
                           Ptr
                           (  Get
                              (  Client.Topology.Devices,
                                 Get (Client.Topology.RF, Index)
                              )
                           ) .all;
               begin
                  if Kind_Of /= Device.Kind_Of then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Invalid C-response, wrong device type "
                        &  Device_Type'Image (Kind_Of)
                        &  ", expected "
                        &  Device_Type'Image (Device.Kind_Of)
                     )  );
                  end if;
                  case Kind_Of is
                     when Cube            |
                          Shutter_Contact |
                          Eco_Button      |
                          Unknown         =>
                        null;
                     when Wall_Thermostat =>
                        Get_Wall_Thermostat_Data
                        (  Data,
                           Pointer,
                           Device
                        );
                     when Radiator_Thermostat |
                          Radiator_Thermostat_Plus =>
                        Get_Thermostat_Data (Data, Pointer, Device);
                  end case;
                  Device.Init := Device.Init or Have_Settings;
               end;
            end if;
         end;
         begin
            if Update = Detached_Device_Update then
               if Kind_Of in Radiator_Thermostat..Eco_Button then
                  Configuration_Updated
                  (  Client,
                     (  Kind_Of   => Detached_Device_Update,
                        Device    => Kind_Of,
                        Address   => Address,
                        Serial_No => Data (Pointer - 10..Pointer - 1)
                  )  );
               end if;
            else
               Client.Ready := Client.Ready or Got_C;
               Configuration_Updated
               (  Client,
                  (  Kind_Of => Device_Parameters_Update,
                     Device  => Kind_Of,
                     Address => Address
               )  );
            end if;
         exception
            when others =>
               null;
         end;
      end;
   end Get_C;

   function Get_Clock_Difference (Client : ELV_MAX_Cube_Client)
      return Duration is
   begin
      if 0 /= (Client.Ready and Got_H) then
         return Client.Clock_Diff;
      else
         Raise_Exception
         (  Status_Error'Identity,
            "No clock difference known"
         );
      end if;
   end Get_Clock_Difference;

   procedure Get_Comma
             (  Line    : String;
                Pointer : in out Integer
             )  is
   begin
      if Pointer <= Line'Last and then Line (Pointer) = ',' then
         Pointer := Pointer + 1;
      else
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid response, missing comma (,)"
         );
      end if;
   end Get_Comma;

   function Get_Device
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Positive is
      Lock : Topology_Holder (Client'Access);
   begin
      return Get_Device_Unchecked (Client, Address);
   end Get_Device;

   function Get_Device
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive;
               Device : Positive
            )  return Positive is
      Lock : Topology_Holder (Client'Access);
   begin
      return Find
             (  Client.Topology.Devices,
                (Ptr (Get (Client.Topology.Rooms, Index)).ID, Device)
             );
   end Get_Device;

   function Get_Device
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID;
               Device : Positive
            )  return Positive is
      Lock : Topology_Holder (Client'Access);
   begin
      return Find (Client.Topology.Devices, (ID, Device));
   end Get_Device;

   function Get_Device_Data
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Device_Data is
      Lock   : Topology_Holder (Client'Access);
      Device : Device_Descriptor'Class renames
               Ptr (Get (Client.Topology.Devices, Index)).all;
   begin
      if 0 /= (Device.Init and Have_Data) then
         return Device.Last;
      else
         Raise_Exception
         (  Status_Error'Identity,
            (  "No data received from the device "
            &  Image (Device.Data.Address)
            &  " yet"
         )  );
      end if;
   end Get_Device_Data;

   function Get_Device_Data
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Device_Data is
      Lock   : Topology_Holder (Client'Access);
      Device : Device_Descriptor'Class renames
               Ptr
               (  Get
                  (  Client.Topology.Devices,
                     Get_Device_Unchecked (Client, Address)
               )  ) .all;
   begin
      if 0 /= (Device.Init and Have_Data) then
         return Device.Last;
      else
         Raise_Exception
         (  Status_Error'Identity,
            (  "No data received from the device "
            &  Image (Address)
            &  " yet"
         )  );
      end if;
   end Get_Device_Data;

   function Get_Device_Name
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return String is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Data.Name;
   end Get_Device_Name;

   function Get_Device_Name
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return String is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr
             (  Get
                (  Client.Topology.Devices,
                   Get_Device_Unchecked (Client, Address)
             )  ) .Data.Name;
   end Get_Device_Name;

   function Get_Device_Parameters
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Device_Parameters is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Data;
   end Get_Device_Parameters;

   function Get_Device_Parameters
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Device_Parameters is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr
             (  Get
                (  Client.Topology.Devices,
                   Get_Device_Unchecked (Client, Address)
             )  ) .Data;
   end Get_Device_Parameters;

   function Get_Device_Room
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Room_ID is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Data.Room;
   end Get_Device_Room;

   function Get_Device_Room
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Room_ID is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr
             (  Get
                (  Client.Topology.Devices,
                   Get_Device_Unchecked (Client, Address)
             )  ) .Data.Room;
   end Get_Device_Room;

   function Get_Device_RF_Address
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return RF_Address is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Data.Address;
   end Get_Device_RF_Address;

   function Get_Device_Serial_No
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return String is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Data.Serial_No;
   end Get_Device_Serial_No;

   function Get_Device_Serial_No
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return String is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr
             (  Get (Client.Topology.Devices,
                Get_Device_Unchecked (Client, Address))
             ) .Data.Serial_No;
   end Get_Device_Serial_No;

   function Get_Device_Type
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Device_Type is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Kind_Of;
   end Get_Device_Type;

   function Get_Device_Type
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Device_Type is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr
             (  Get
                (  Client.Topology.Devices,
                   Get_Device_Unchecked (Client, Address)
             )  ) .Kind_Of;
   end Get_Device_Type;

   function Get_Device_Unchecked
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Positive is
   begin
      return Get (Client.Topology.RF, Address);
   exception
      when Constraint_Error =>
         Raise_Exception
         (  End_Error'Identity,
            "No device " & Image (Address) & " known"
         );
   end Get_Device_Unchecked;

   function Get_Device_Unchecked_Unsafe
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Natural is
      Index : constant Integer := Find (Client.Topology.RF, Address);
   begin
      if Index > 0 then
         return Get (Client.Topology.RF, Index);
      else
         return 0;
      end if;
   end Get_Device_Unchecked_Unsafe;

   procedure Get_Duration
             (  Line    : String;
                Pointer : in out Integer;
                Value   : out Duration
             )  is
   begin
      if Pointer > Line'Last then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid response, missing duration"
         );
      end if;
      Value := Duration (Character'Pos (Line (Pointer)) * Five_Minutes);
      if Value > Day_Duration'Last then
         Value := Day_Duration'Last;
      end if;
      Pointer := Pointer + 1;
   end Get_Duration;

   function Get_Duty (Client : ELV_MAX_Cube_Client) return Ratio is
   begin
      return Client.Duty;
   end Get_Duty;

   function Get_Error (Client : ELV_MAX_Cube_Client) return Boolean is
   begin
      return Client.Error;
   end Get_Error;

   procedure Get_F
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Line   : String
             )  is
   begin
      Configuration_Updated
      (  Client,
         (  Kind_Of          => NTP_Servers_List_Update,
            NTP_Servers_List => Create_List (Line)
      )  );
   exception
      when others =>
         null;
   end Get_F;

   procedure Get_Field
             (  Line    : String;
                Pointer : in out Integer;
                From    : out Integer;
                To      : out Integer
             )  is
   begin
      for Index in Pointer..Line'Last loop
         if Line (Index) = ',' then
            From    := Pointer;
            To      := Index - 1;
            Pointer := Index + 1;
            return;
         end if;
      end loop;
      From    := Pointer;
      To      := Line'Last;
      Pointer := Line'Last + 1;
   end Get_Field;

   procedure Get_H
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Line   : String
             )  is
      Pointer : Integer := Line'First;
      From    : Integer;
      To      : Integer;
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Get_Field (Line, Pointer, From, To);
      if To - From + 1 >= Client.Serial_No'Length then
         Client.Serial_No :=
            Line (From..From + Client.Serial_No'Length - 1);
      else
         Client.Serial_No := (others => ' ');
         Client.Serial_No (1..To - From + 1) := Line (From..To);
      end if;
      begin
         Get
         (  Source  => Line,
            Pointer => Pointer,
            Value   => Integer (Client.Address),
            Base    => 16
         );
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid H-response, missing radio frequency address"
            );
      end;
      Get_Comma (Line, Pointer);
      Get_Field (Line, Pointer, From, To);
      if To - From /= 3 then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid H-response, wrong version field"
         );
      else
         if Line (From) = '0' then
            Client.Version (1) := ' ';
         else
            Client.Version (1) := Line (From);
         end if;
         Client.Version (2) := Line (From + 1);
         Client.Version (4) := Line (From + 2);
         Client.Version (6) := Line (From + 3);
      end if;
      Get_Field (Line, Pointer, From, To); -- Unknown field
      Get_Field (Line, Pointer, From, To); -- HTTP Connection ID
      Get_Field (Line, Pointer, From, To); -- Duty Cycle
      if To - From /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid H-response, wrong duty cycle"
         );
      else
         declare
            Last  : constant := 100;
            Index : Integer  := From;
            Value : Integer;
         begin
            Get (Line, Index, Value, Base => 16);
            if Value <= 0 then
               Client.Duty := Ratio'First;
            elsif Value >= Last then
               Client.Duty := Ratio'Last;
            else
               Client.Duty := Ratio (Float (Value) / Float (Last));
            end if;
         exception
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid H-response, wrong duty cycle"
              );
         end;
      end if;
      Get_Field (Line, Pointer, From, To); -- Free Memory slots
      declare
         Date : Integer;
      begin
         Get
         (  Source  => Line,
            Pointer => Pointer,
            Value   => Date,
            Base    => 16
         );
         Year  := Date / 2**16 + 2000;
         Month := (Date / 2**8) mod 2**8;
         Day   := Date mod 2**8;
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid H-response, missing or illegal date"
            );
      end;
      Get_Comma (Line, Pointer);
      declare
         Time : Integer;
      begin
         Get
         (  Source  => Line,
            Pointer => Pointer,
            Value   => Time,
            Base    => 16
         );
         Seconds := (  Duration ((Time / 2**8) mod 2**8) * 3600.0
                    +  Duration (Time mod 2**8) * 60.0
                    );
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid H-response, missing or illegal time"
            );
      end;
      begin
         Client.Clock_Diff :=
            Time_Of (Year, Month, Day, Seconds) - Clock;
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid H-response, missing or wrong date or time"
            );
      end;
      Get_Field (Line, Pointer, From, To); -- Unknown field
      Client.Ready := Client.Ready or Got_H;
      begin
         Configuration_Updated (Client, (Kind_Of => Cube_Update));
      exception
         when others =>
            null;
      end;
   end Get_H;

   procedure Get_L
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Line   : String
             )  is
      Data        : constant String := From_Base64 (Line);
      Pointer     : Integer := Data'First;
      Length      : Integer := 0;
      RF_Index    : Integer;
      Next        : Integer := Data'First;
      Address     : RF_Address;
      Answer      : Boolean;
      Error       : Boolean;
      Uptodate    : Boolean;
      Initialized : Boolean;

      procedure Get_Eco_Button_Status (Last : in out Device_Data) is
         Byte : constant Unsigned_8 := Character'Pos (Data (Pointer));
      begin
         Pointer := Pointer + 1;
         Last.Battery_Low   := 0 /= (Byte and 2**7);
         Last.Link_Error    := 0 /= (Byte and 2**6);
         Last.Panel_Locked  := 0 /= (Byte and 2**5);
         Last.Gateway_Known := 0 /= (Byte and 2**4);
         Last.DST           := 0 /= (Byte and 2**3);
         case Byte and 2#11# is
            when      0 => Last.Mode := Automatic;
            when      1 => Last.Mode := Manual;
            when      2 => Last.Mode := Vacation;
            when others => Last.Mode := Boost;
         end case;
      end Get_Eco_Button_Status;

      procedure Get_Shutter_Contact_Status
                (  Last : in out Device_Data
                )  is
         Byte : constant Unsigned_8 := Character'Pos (Data (Pointer));
      begin
         Pointer := Pointer + 1;
         Last.Battery_Low   := 0 /= (Byte and 2**7);
         Last.Link_Error    := 0 /= (Byte and 2**6);
         Last.Panel_Locked  := 0 /= (Byte and 2**5);
         Last.Gateway_Known := 0 /= (Byte and 2**4);
         Last.DST           := 0 /= (Byte and 2**3);
         case Byte and 2#11# is
            when      0 => Last.Mode := Automatic;
            when      1 => Last.Mode := Manual;
            when      2 => Last.Mode := Vacation;
            when others => Last.Mode := Boost;
         end case;
         Last.Open := 0 /= (Byte and 2**1);
      end Get_Shutter_Contact_Status;

      procedure Get_Thermostat_Status (Last : in out Device_Data) is
         Byte : Unsigned_8;
      begin
         Byte   := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Last.Battery_Low   := 0 /= (Byte and 2**7);
         Last.Link_Error    := 0 /= (Byte and 2**6);
         Last.Panel_Locked  := 0 /= (Byte and 2**5);
         Last.Gateway_Known := 0 /= (Byte and 2**4);
         Last.DST           := 0 /= (Byte and 2**3);
         case Byte and 2#11# is
            when      0 => Last.Mode := Automatic;
            when      1 => Last.Mode := Manual;
            when      2 => Last.Mode := Vacation;
            when others => Last.Mode := Boost;
         end case;
         Last.Valve_Position :=
            Ratio
            (  Float (Character'Pos (Data (Pointer))) / 100.0
            );
         Pointer := Pointer + 1;
         Last.Set_Temperature :=
            Centigrade (Float (Character'Pos (Data (Pointer))) / 2.0);
         Last.New_Temperature := Last.Set_Temperature;
         Pointer := Pointer + 1;
         Last.Temperature := Centigrade'First;
         if (  Length >= 11
            and then
               Last.Mode in Automatic..Manual
            and then
               Character'Pos (Data (Pointer + 1)) /= 0
            )
         then
            Last.Temperature :=
               Centigrade
               (  Float
                  (  (Character'Pos (Data (Pointer)) mod 2) * 256
                  +  Character'Pos (Data (Pointer + 1))
                  )
               /  10.0
               );
            Last.Latest_Temperature := Last.Temperature;
            Last.Received_At        := Clock;
            Pointer := Pointer + 2;
         end if;
--           declare
--              Month     : Month_Number;
--              Day       : Day_Number;
--              Year      : Year_Number;
--              Seconds   : Duration;
--              Next_Time : Time;
--              Date : Unsigned_16 :=
--                     (  Character'Pos (Data (Pointer    )) * 256
--                     +  Character'Pos (Data (Pointer + 1))
--                     );
--           begin
--              if Date /= 0 and then Mode = Automatic then
--                 Month :=
--                    Month_Number
--                    (  (Shift_Right (Date, 7    ) and 2#0001#)
--                    +  (Shift_Right (Date, 4 + 8) and 2#1110#)
--                    );
--                 Day  := Day_Number (Shift_Right (Date, 8) and 2#1_1111#);
--                 Year := Year_Number ((Date and 2#11_1111#) + 2000);
--                 Seconds :=
--                    Duration (Character'Pos (Data (Pointer))) * 1800.0;
--                 Next_Time := Time_Of (Year, Month, Day, Seconds);
--              else
--                 Next_Time := Clock;
--              end if;
--           exception
--              when Constraint_Error =>
--                 Raise_Exception
--                 (  Data_Error'Identity,
--                    "Invalid L-response, wrong thermostat date"
--                 );
--              when Time_Error =>
--                 Raise_Exception
--                 (  Data_Error'Identity,
--                    "Invalid L-response, invalid thermostat date"
--                 );
--           end;
         Pointer := Pointer + 2;
         Pointer := Pointer + 1;
      end Get_Thermostat_Status;

      procedure Get_Wall_Thermostat_Status
                (  Last : in out Device_Data
                )  is
         Byte  : Unsigned_8;
         Eight : Integer;
      begin
         Byte    := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Last.Battery_Low   := 0 /= (Byte and 2**7);
         Last.Link_Error    := 0 /= (Byte and 2**6);
         Last.Panel_Locked  := 0 /= (Byte and 2**5);
         Last.Gateway_Known := 0 /= (Byte and 2**4);
         Last.DST           := 0 /= (Byte and 2**3);
         case Byte and 2#11# is
            when      0 => Last.Mode := Automatic;
            when      1 => Last.Mode := Manual;
            when      2 => Last.Mode := Vacation;
            when others => Last.Mode := Boost;
         end case;
         Pointer := Pointer + 1;
         Eight   := Character'Pos (Data (Pointer));
         Last.Set_Temperature :=
            Centigrade (Float (Eight mod 2**6) / 2.0);
         Last.New_Temperature := Last.Set_Temperature;
         Pointer := Pointer + 4;
         Last.Temperature :=
            Centigrade
            (  Float
               (  (Eight / 2**7) * 256
               +  Character'Pos (Data (Pointer))
               )
            /  10.0
            );
         Pointer := Pointer + 1;
      end Get_Wall_Thermostat_Status;

      function Get_Data return Device_Data is
         Index  : constant Integer :=
                           Get (Client.Topology.RF, RF_Index);
         Device : Device_Descriptor'Class renames
                  Ptr (Get (Client.Topology.Devices, Index)).all;
      begin
         case Device.Kind_Of is
            when Cube | Unknown =>
               null;
            when Wall_Thermostat =>
               if Length < 12 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Invalid L-response, wrong wall thermostat "
                     &  Image (Address)
                     &  " data length "
                     &  Image (Length)
                     &  ", expected at least 12"
                  )  );
               end if;
               Get_Wall_Thermostat_Status (Device.Last);
            when Shutter_Contact =>
               if Length < 6 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Invalid L-response, wrong shutter contact "
                     &  Image (Address)
                     &  " data length "
                     &  Image (Length)
                     &  ", expected at least 6"
                  )  );
               end if;
               Get_Shutter_Contact_Status (Device.Last);
            when Eco_Button =>
               if Length < 6 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Invalid L-response, wrong eco button "
                     &  Image (Address)
                     &  " data length "
                     &  Image (Length)
                     &  ", expected at least 6"
                  )  );
               end if;
               Get_Eco_Button_Status (Device.Last);
            when Radiator_Thermostat | Radiator_Thermostat_Plus =>
               if Length < 11 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Invalid L-response, wrong radiator thermostat "
                     &  Image (Address)
                     &  " data length "
                     &  Image (Length)
                     &  ", expected at least 11"
                  )  );
               end if;
               Get_Thermostat_Status (Device.Last);
         end case;
         Device.Last.Address     := Address;
         Device.Last.Error       := Error;
         Device.Last.Initialized := Initialized;
         Device.Init             := Device.Init or Have_Data;
         return Device.Last;
      end Get_Data;

   begin
      while Pointer <= Data'Last loop
         Length  := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         if Pointer + Length - 1 > Data'Last then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid L-response, wrong data length"
            );
         end if;
         Next := Pointer + Length;
         Get_Address (Data, Pointer, Address);
         Pointer := Pointer + 1;
         declare
            Byte : constant Unsigned_8 :=
                            Character'Pos (Data (Pointer));
         begin
            Uptodate    := 0 /= (Byte and 2**4);
            Error       := 0 /= (Byte and 2**3);
            Answer      := 0 /= (Byte and 2**2);
            Initialized := 0 /= (Byte and 2**1);
         end;
         declare
            Lock : Topology_Holder (Client'Access);
         begin
            RF_Index := Find (Client.Topology.RF, Address);
            Pointer  := Pointer + 1;
            if RF_Index <= 0 then
               Release (Lock);
               Faulty_Device_Received
               (  Client,
                  Address,
                  Length,
                  Error,
                  Initialized,
                  True
               );
            else
               declare
                  Data : constant Device_Data := Get_Data;
               begin
                  Release (Lock);
                  if Data.Kind_Of in Radiator_Thermostat..Eco_Button
                  then
                     Data_Received (Client, Data);
                  end if;
               end;
            end if;
         end;
         Pointer := Next;
      end loop;
      Client.Ready := Client.Ready or Got_L;
   exception
      when Error : others =>
         declare
            This : constant Integer := Next - Length - 1;
         begin
            if (  This in Data'First..Data'Last + 1
               and then
                  Pointer in Data'First..Data'Last + 1
               and then
                  Next in Data'First..Data'Last + 1
               )  then
               Trace
               (  Client,
                  (  "Error parsing L-response "
                  &  Image (From_String (Data (Data'First..This - 1)))
                  &  " [ "
                  &  Image (From_String (Data (This..Pointer - 1)))
                  &  " | "
                  &  Image (From_String (Data (Pointer..Next - 1)))
                  &  " ] "
                  &  Image (From_String (Data (Next..Data'Last)))
                  &  " ("
                  &  Exception_Message (Error)
                  &  ")"
               )  );
            else
               Trace
               (  Client,
                  (  "Error parsing L-response "
                  &  Image (From_String (Data))
                  &  " Pointer=" & Image (Pointer)
                  &  " Length="  & Image (Length)
                  &  " Next="    & Image (Next)
                  &  " ("
                  &  Exception_Message (Error)
                  &  ")"
               )  );
            end if;
         end;
         if Exception_Identity (Error) /= End_Error'Identity then
            raise; -- Not an unknown device
         end if;
   end Get_L;

   procedure Get_M
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Line   : String
             )  is
      procedure Append (Data : String) is
         Unused : constant Natural :=
                  Client.Metadata.Size - Client.Metadata.Length;
      begin
         if Unused < Data'Length then
            declare
               Ptr : constant String_Buffer_Ptr :=
                     new String_Buffer
                         (  Client.Metadata.Size + Data'Length - Unused
                         );
            begin
               Ptr.Length := Client.Metadata.Length;
               Ptr.Count  := Client.Metadata.Count;
               Ptr.Data (1..Ptr.Length) :=
                  Client.Metadata.Data (1..Ptr.Length);
               Free (Client.Metadata);
               Client.Metadata := Ptr;
            end;
         end if;
         Client.Metadata.Data
         (  Client.Metadata.Length + 1
         .. Client.Metadata.Length + Data'Length
         )  := Data;
         Client.Metadata.Length := Client.Metadata.Length + Data'Length;
         Client.Metadata.Count  := Client.Metadata.Count + 1;
      end Append;

      Pointer : Integer := Line'First;
      I, J    : Integer;
      Lock    : Topology_Holder (Client'Access);
   begin
      if Pointer > Line'Last then -- Unconfigured cude
         Trace
         (  Client,
            (  "Cube "
            &  Image (Client.Address)
            &  " "
            &  Client.Serial_No
            &  " is not configured"
         )  );
         Client.Ready := Client.Ready or Got_M;
         return;
      end if;
      begin
         Get (Line, Pointer, I, First => 0);
      exception
         when End_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid M-response, missing first field"
            );
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid M-response, wrong first field"
            );
      end;
      if I = 0 then
         if Client.Metadata /= null then
            Client.Metadata.Length := 0;
            Client.Metadata.Count  := 0;
         end if;
      else
         if Client.Metadata = null then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid M-response, wrong first field "
               &  Image (I)
               &  ", expected 0"
            )  );
         elsif Client.Metadata.Count /= I then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid M-response, wrong first field "
               &  Image (I)
               &  ", expected "
               &  Image (Client.Metadata.Count)
            )  );
         end if;
      end if;
      Get_Comma (Line, Pointer);
      begin
         Get (Line, Pointer, J, First => 0, Last => Max_M_Blocks);
      exception
         when End_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid M-response, missing second field"
            );
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid M-response, wrong second field, expected 0.."
               &  Image (Max_M_Blocks)
            )  );
      end;
      Get_Comma (Line, Pointer);
      declare
         Data : constant String :=
                         From_Base64 (Line (Pointer..Line'Last));
      begin
         if I + 1 < J then -- Accumulating responses
            if I = 0 then  -- The first in the sequence
               if Client.Metadata = null then
                  Client.Metadata :=
                     new String_Buffer (1_900 * Natural'Min (J, 4));
               end if;
            end if;
            Append (Data);
         else -- Last response
            if I = 0 then -- Single response
               Set_From_Metadata_Unchecked
               (  Client,
                  Lock,  -- Release the lock upon notification
                  Data,
                  True
               );
            else
               Append (Data);
               Set_From_Metadata_Unchecked
               (  Client,
                  Lock,  -- Release the lock upon notification
                  Client.Metadata.Data
                  (  1
                  .. Client.Metadata.Length
                  ),
                  True
               );
            end if;
         end if;
      end;
   exception
      when Error : Data_Error =>
         Clean_Up (Client, False);
         Client.Ready := Client.Ready or Got_M;
         Trace
         (  Client,
            "Malconfigured cube: " & Exception_Message (Error)
         );
   end Get_M;

   function Get_Metadata
            (  Client : ELV_MAX_Cube_Client
            )  return String is
      Length : Natural := 0;
      Buffer : String (1..1024 * 2);
      Lock   : Topology_Holder (Client'Access);
   begin
      declare
         procedure Count (Chunk : String) is
         begin
            Length := Length + Chunk'Length;
         end Count;
         procedure Count_Length is new Get_Metadata_Unchecked (Count);
      begin
         Count_Length (Client, Buffer);
      end;
      declare
         Result  : String (1..Length);
         Pointer : Integer := 1;
         procedure Store_Chunk (Chunk : String) is
         begin
            Result (Pointer..Pointer + Chunk'Length - 1) := Chunk;
            Pointer := Pointer + Chunk'Length;
         end Store_Chunk;
         procedure Store is new Get_Metadata_Unchecked (Store_Chunk);
      begin
         Store (Client, Buffer);
         return Result (1..Pointer - 1);
      end;
   end Get_Metadata;

   procedure Get_N
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Line   : String
             )  is
      Data    : constant String := From_Base64 (Line);
      Pointer : Integer := Data'First;
      Kind_Of : Device_Type;
      Address : RF_Address;
   begin
      if Data'Length = 0 then
         begin
            Configuration_Updated
            (  Client,
               (Kind_Of => End_Discovery_Update)
            );
         exception
            when others =>
               null;
         end;
      elsif Data'Length < 14 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Invalid N-response length "
            &  Image (Integer (Data'Length))
            &  ", expected at least 14"
         )  );
      else
         Get_Type    (Data, Pointer, Kind_Of);
         Get_Address (Data, Pointer, Address);
         begin
            Configuration_Updated
            (  Client,
               (  Kind_Of   => Device_Discovery_Update,
                  Device    => Kind_Of,
                  Address   => Address,
                  Serial_No => Data (Pointer..Pointer + 9)
            )  );
         exception
            when others =>
               null;
         end;
      end if;
   end Get_N;

   function Get_Number_Of_Devices
            (  Client : ELV_MAX_Cube_Client
            )  return Natural is
      Lock : Topology_Holder (Client'Access);
   begin
      return Get_Size (Client.Topology.Devices);
   end Get_Number_Of_Devices;

   function Get_Number_Of_Devices
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Natural is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr (Get (Client.Topology.Rooms, Index)).Count;
   end Get_Number_Of_Devices;

   function Get_Number_Of_Devices
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return Natural is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr
             (  Get
                (  Client.Topology.Rooms,
                   Find_Room_Unchecked (Client, ID)
             )  ).Count;
   end Get_Number_Of_Devices;

   function Get_Number_Of_Rooms
            (  Client : ELV_MAX_Cube_Client
            )  return Natural is
      Lock : Topology_Holder (Client'Access);
   begin
      return Get_Size (Client.Topology.Rooms);
   end Get_Number_Of_Rooms;

   function Get_RF_Address
            (  Client    : ELV_MAX_Cube_Client;
               Unchecked : Boolean := False
            )  return RF_Address is
   begin
      if Unchecked or else 0 /= (Client.Ready and Got_H) then
         return Client.Address;
      else
         Raise_Exception
         (  Status_Error'Identity,
            "No radio frequency address known"
         );
      end if;
   end Get_RF_Address;

   function Get_Room_ID
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Room_ID is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr (Get (Client.Topology.Rooms, Index)).ID;
   end Get_Room_ID;

   function Get_Room_Name
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return String is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr (Get (Client.Topology.Rooms, Index)).Name;
   end Get_Room_Name;

   function Get_Room_Name
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return String is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr
             (  Get
                (  Client.Topology.Rooms,
                   Find_Room_Unchecked (Client, ID)
             )  ) .Name;
   end Get_Room_Name;

   function Get_Room_RF_Address
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return RF_Address is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr (Get (Client.Topology.Rooms, Index)).Master;
   end Get_Room_RF_Address;

   function Get_Room_RF_Address
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return RF_Address is
      Lock : Topology_Holder (Client'Access);
   begin
      return Ptr
             (  Get
                (  Client.Topology.Rooms,
                   Find_Room_Unchecked (Client, ID)
             )  ) .Master;
   end Get_Room_RF_Address;

   function Get_Serial_No (Client : ELV_MAX_Cube_Client)
      return String is
   begin
      if 0 /= (Client.Ready and Got_H) then
         return Trim (Client.Serial_No);
      else
         Raise_Exception
         (  Status_Error'Identity,
            "No serial number known"
         );
      end if;
   end Get_Serial_No;

   procedure Get_S
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Line   : String
             )  is
      Last    : constant := 100;
      Duty    : Ratio;
      Error   : Integer;
      Slots   : Integer;
      Pointer : Integer := Line'First;
   begin
      declare
         Value : Integer;
      begin
         Get (Line, Pointer, Value, Base => 16);
         if Value <= 0 then
            Duty := Ratio'First;
         elsif Value >= Last then
            Duty := Ratio'Last;
         else
            Duty := Ratio (Float (Value) / Float (Last));
         end if;
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid S-response, missing first field"
            );
      end;
      Get_Comma (Line, Pointer);
      begin
         Get (Line, Pointer, Error);
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid S-response, missing error field"
            );
      end;
      Get_Comma (Line, Pointer);
      begin
         Get (Line, Pointer, Slots, Base => 16);
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid S-response, missing third field"
            );
      end;
      Client.Error := Error /= 0;
      Client.Duty  := Duty;
      Client.Slots := Slots;
      Status_Received (Client, Error /= 0, Duty, Slots);
   end Get_S;

   procedure Get_Temperature
             (  Line    : String;
                Pointer : in out Integer;
                Value   : out Centigrade
             )  is
   begin
      if Pointer > Line'Last then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid response, missing temperature"
         );
      end if;
      Value := Centigrade
               (  Float (Character'Pos (Line (Pointer)))
               /  2.0
               );
      Pointer := Pointer + 1;
   end Get_Temperature;

   procedure Get_Type
             (  Line    : String;
                Pointer : in out Integer;
                Kind_Of : out Device_Type
             )  is
   begin
      if Pointer > Line'Last then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid response, missing device type"
         );
      end if;
      case Character'Pos (Line (Pointer)) is
         when 0      => Kind_Of := Cube;
         when 1      => Kind_Of := Radiator_Thermostat;
         when 2      => Kind_Of := Radiator_Thermostat_Plus;
         when 3      => Kind_Of := Wall_Thermostat;
         when 4      => Kind_Of := Shutter_Contact;
         when 5      => Kind_Of := Eco_Button;
         when others => Kind_Of := Unknown;
      end case;
      Pointer := Pointer + 1;
   end Get_Type;

   function Get_Version (Client : ELV_MAX_Cube_Client) return String is
   begin
      if 0 /= (Client.Ready and Got_H) then
         return Trim (Client.Version);
      else
         Raise_Exception
         (  Status_Error'Identity,
            "No version known"
         );
      end if;
   end Get_Version;

   procedure Handshake_Received (Client : in out ELV_MAX_Cube_Client) is
   begin
      null;
   end Handshake_Received;

   function Has_Device_Configuration
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Boolean is
      Lock   : Topology_Holder (Client'Access);
      Device : Device_Descriptor'Class renames
               Ptr (Get (Client.Topology.Devices, Index)).all;
   begin
      return 0 /= (Device.Init and Have_Settings);
   end Has_Device_Configuration;

   function Has_Device_Configuration
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Boolean is
      Lock  : Topology_Holder (Client'Access);
      Index : constant Natural :=
                       Get_Device_Unchecked_Unsafe (Client, Address);
   begin
      if Index > 0 then
         declare
            Device : Device_Descriptor'Class renames
                     Ptr (Get (Client.Topology.Devices, Index)).all;
         begin
            return 0 /= (Device.Init and Have_Settings);
         end;
      else
         return False;
      end if;
   end Has_Device_Configuration;

   function Has_Device_Data
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Boolean is
      Lock   : Topology_Holder (Client'Access);
      Device : Device_Descriptor'Class renames
               Ptr (Get (Client.Topology.Devices, Index)).all;
   begin
      return 0 /= (Device.Init and Have_Data);
   end Has_Device_Data;

   function Has_Device_Data
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Boolean is
      Lock   : Topology_Holder (Client'Access);
      Device : Device_Descriptor'Class renames
               Ptr
               (  Get
                  (  Client.Topology.Devices,
                     Get_Device_Unchecked (Client, Address)
               )  ) .all;
   begin
      return 0 /= (Device.Init and Have_Data);
   end Has_Device_Data;

   function Image (Data : Device_Data) return String is
      Result  : String (1..200);
      Pointer : Integer := 1;
   begin
      Put (Result, Pointer, Image (Data.Kind_Of));
      Put (Result, Pointer, " ");
      Put (Result, Pointer, Image (Data.Address));
      if Data.Kind_Of in Radiator_Thermostat..Wall_Thermostat then
         case Data.Mode is
            when Automatic =>
               Put (Result, Pointer, ", automatic");
            when Manual =>
               Put (Result, Pointer, ", manual");
            when Vacation =>
               Put (Result, Pointer, ", vacation");
            when Boost =>
               Put (Result, Pointer, ", boost");
         end case;
      end if;
      if Data.Error then
         Put (Result, Pointer, ", error");
      end if;
      if Data.Initialized then
         Put (Result, Pointer, ", initialized");
      end if;
      case Data.Kind_Of is
         when Cube =>
            null;
         when Eco_Button =>
            null;
         when Shutter_Contact =>
            if Data.Open then
               Put (Result, Pointer, ", open");
            else
               Put (Result, Pointer, ", closed");
            end if;
         when Radiator_Thermostat | Radiator_Thermostat_Plus =>
            Put (Result, Pointer, ", valve ");
            Put
            (  Result,
               Pointer,
               Integer (Float (Data.Valve_Position) * 100.0)
            );
            Put (Result, Pointer, "%, ");
            if Data.Temperature /= Centigrade'First then
               Put
               (  Destination => Result,
                  Pointer     => Pointer,
                  Value       => Float (Data.Temperature),
                  AbsSmall    => -1
               );
               Put (Result, Pointer, ", ");
            else
               Put (Result, Pointer, "no temperature, ");
            end if;
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => Float (Data.Set_Temperature),
               AbsSmall    => -1
            );
            Put (Result, Pointer, " -> ");
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => Float (Data.New_Temperature),
               AbsSmall    => -1
            );
         when Wall_Thermostat =>
            Put (Result, Pointer, ", ");
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => Float (Data.Temperature),
               AbsSmall    => -1
            );
            Put (Result, Pointer, ", ");
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => Float (Data.Set_Temperature),
               AbsSmall    => -1
            );
            Put (Result, Pointer, " -> ");
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => Float (Data.New_Temperature),
               AbsSmall    => -1
            );
         when Unknown =>
            null;
      end case;
      if Data.Kind_Of in Radiator_Thermostat..Eco_Button then
         if Data.Panel_Locked then
            Put (Result, Pointer, ", panel locked");
         else
            Put (Result, Pointer, ", panel unlocked");
         end if;
         if Data.Battery_Low then
            Put (Result, Pointer, ", low battery");
         end if;
         if Data.Link_Error then
            Put (Result, Pointer, ", link error");
         end if;
         if Data.DST then
            Put (Result, Pointer, ", summer time");
         else
            Put (Result, Pointer, ", winter time");
         end if;
      end if;
      return Result (1..Pointer - 1);
   end Image;

   function Image (Value : RF_Address) return String is
      Result  : String (1..6);
      Pointer : Integer := 1;
   begin
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Integer (Value),
         Fill        => '0',
         Base        => 16,
         Field       => 6,
         Justify     => Right
      );
      return Result;
   end Image;

   function Image (Value : Centigrade) return String is
      Result  : String (1..10);
      Pointer : Integer := 1;
   begin
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Float (Value),
         AbsSmall    => -1
      );
      return Result (1..Pointer - 1);
   end Image;

   function Image (Kind_Of : Device_Type) return String is
   begin
      case Kind_Of is
         when Cube =>
            return "cube";
         when Radiator_Thermostat =>
            return "radiator thermostat";
         when Radiator_Thermostat_Plus =>
            return "radiator thermostat plus";
         when Wall_Thermostat =>
            return "wall thermostat";
         when Shutter_Contact =>
            return "shutter contact";
         when Eco_Button =>
            return "eco button";
         when Unknown =>
            return "unknown device";
      end case;
   end Image;

   function Image (Day : Week_Day; Short : Boolean := True)
      return String is
   begin
      if Short then
         case Day is
            when Mo => return "Mo";
            when Tu => return "Tu";
            when We => return "We";
            when Th => return "Th";
            when Fr => return "Fr";
            when Sa => return "Sa";
            when Su => return "Su";
         end case;
      else
         case Day is
            when Mo => return "Monday";
            when Tu => return "Tuesday";
            when We => return "Wednesday";
            when Th => return "Thursday";
            when Fr => return "Friday";
            when Sa => return "Saturday";
            when Su => return "Sunday";
         end case;
      end if;
   end Image;

   function Image (Time : Week_Time; Short : Boolean := True)
      return String is
   begin
      return Image (Time.Day, Short) & ' ' & Minutes (Time.Time);
   end Image;

   function Image_Metadata (Data : String) return String is
      Size : Natural := 1024;
   begin
      loop
         declare
            Result  : String (1..Size);
            Pointer : Integer := 1;
         begin
            Put_Metadata (Result, Pointer, Data);
            return Result (1..Pointer - 1);
         exception
            when Layout_Error =>
               Size := Size * 2;
         end;
      end loop;
   end Image_Metadata;

   procedure Initialize (Lock : in out Topology_Holder) is
   begin
      Lock.Client.Topology.Self.Lock.Seize;
   end Initialize;

   procedure Initialize (Client : in out ELV_MAX_Cube_Client) is
   begin
      Client.LF.Value (1) := Stream_Element'Val (10);
      Initialize (State_Machine (Client));
   end Initialize;

   function Is_Configured
            (  Client : ELV_MAX_Cube_Client
            )  return Boolean is
      Lock : Topology_Holder (Client'Access);
   begin
      if 0 = (Client.Ready and Got_H) then
         return False;
      end if;
      for Index in 1..Get_Size (Client.Topology.Devices) loop
         declare
            Device : Device_Descriptor'Class renames
                     Ptr (Get (Client.Topology.Devices, Index)).all;
         begin
            case Device.Kind_Of is
               when Cube | Eco_Button | Unknown =>
                  null;
               when Radiator_Thermostat..Shutter_Contact =>
                  if 0 = (Device.Init and Have_Settings) then
                     return False;
                  end if;
            end case;
         end;
      end loop;
      return True;
   end Is_Configured;

   function Is_Blocking (Data : String) return Boolean is
   begin
      case Data (Data'First) is
         when 'd' | 'e' | 'f' | 's' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Blocking;

   function Is_In
            (  Client : ELV_MAX_Cube_Client;
               Device : RF_Address
            )  return Boolean is
      Lock : Topology_Holder (Client'Access);
   begin
      return Find (Client.Topology.RF, Device) > 0;
   end Is_In;

   function Is_In
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive;
               Device : Positive
            )  return Boolean is
      Lock : Topology_Holder (Client'Access);
   begin
      if Index > Get_Size (Client.Topology.Rooms) then
         return False;
      elsif Device > Get_Size (Client.Topology.Devices) then
         return False;
      else
         return
         (  Get (Client.Topology.Rooms, Index)
         =  Ptr (Get (Client.Topology.Devices, Device)).Room
         );
      end if;
   end Is_In;

   function Is_In
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID;
               Device : Positive
            )  return Boolean is
      Lock  : Topology_Holder (Client'Access);
      Index : constant Integer := Find (Client.Topology.Rooms, ID);
   begin
      if Index <= 0 then
         return False;
      elsif Device > Get_Size (Client.Topology.Devices) then
         return False;
      else
         return
         (  Get (Client.Topology.Rooms, Index)
         =  Ptr (Get (Client.Topology.Devices, Device)).Room
         );
      end if;
   end Is_In;

   function Is_New_Master
            (  Client  : ELV_MAX_Cube_Client;
               Kind_Of : Device_Type;
               Room    : Room_Descriptor'Class
            )  return Boolean is
   begin
      case Kind_Of is
         when Radiator_Thermostat | Radiator_Thermostat_Plus =>
            return Room.Master = 0;
         when Wall_Thermostat =>
            if Room.Master = 0 then
               return True;
            else
               declare
                  Index : constant Integer :=
                          Find (Client.Topology.RF, Room.Master);
               begin
                  return
                  (  Index <= 0
                  or else
                     (  Ptr
                        (  Get
                           (  Client.Topology.Devices,
                              Index
                        )  ) .Kind_Of
                     /= Wall_Thermostat
                  )  );
               end;
            end if;
         when others =>
            return False;
      end case;
   end Is_New_Master;

   procedure Link_Unchecked
             (  Client     : in out ELV_MAX_Cube_Client;
                Kind_Of    : Partner_Device_Type;
                Address    : RF_Address;
                To         : Room_Descriptor'Class;
                New_Master : Boolean;
                S_Commands : in out Natural
             )  is
      procedure Link (Data : Device_Parameters) is
      begin
         Send -- Link backward
         (  Client,
            (  "s:"
            &  To_Base64
               (  S_Link (Data.Address = To.Master)
               &  Encode (Data.Address)
               &  Encode (Data.Room)
               &  Encode (Address)
               &  Encode (Kind_Of)
               )
            &  CRLF
         )  );
         Send -- Link forward
         (  Client,
            (  "s:"
            &  To_Base64
               (  S_Link (False) -- New_Master)
               &  Encode (Address)
               &  Encode (Data.Room)
               &  Encode (Data.Address)
               &  Encode (Kind_Of)
               )
            &  CRLF
         )  );
         S_Commands := S_Commands + 2;
      end Link;
   begin
      for Index in 1..To.Count loop -- Devices in the room
         declare
            Device : Device_Descriptor'Class renames
                     Ptr
                     (  Get (Client.Topology.Devices, (To.ID, Index))
                     ) .all;
         begin
            if Device.Data.Address /= Address then
               case Device.Kind_Of is
                  when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                     case Kind_Of is -- Anything but radiator thermostat
                        when Wall_Thermostat |
                             Shutter_Contact |
                             Eco_Button      =>
                           Link (Device.Data);
                        when others =>
                           null;
                     end case;
                  when Wall_Thermostat =>
                     case Kind_Of is -- Anything but wall thermostat
                        when Radiator_Thermostat      |
                             Radiator_Thermostat_Plus |
                             Shutter_Contact          |
                             Eco_Button               =>
                           Link (Device.Data);
                        when others =>
                           null;
                     end case;
                  when Shutter_Contact | Eco_Button =>
                     case Kind_Of is -- Any thermostat
                        when Radiator_Thermostat..Wall_Thermostat =>
                           Link (Device.Data);
                        when others =>
                           null;
                     end case;
                  when others =>
                     null;
               end case;
            end if;
         end;
      end loop;
   end Link_Unchecked;

   function Minutes (Time : Day_Duration) return String is
      Minutes : constant Integer := Integer (Time) / 60;
      Result  : String (1..5);
      Pointer : Integer := 1;
   begin
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Minutes / 60,
         Fill        => '0',
         Field       => 2,
         Justify     => Right
      );
      Result (Pointer) := ':';
      Pointer := Pointer + 1;
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Minutes mod 60,
         Fill        => '0',
         Field       => 2,
         Justify     => Right
      );
      return Result;
   end Minutes;

   procedure Pair
             (  Client  : in out ELV_MAX_Cube_Client;
                Timeout : Duration := 60.0
             )  is
      Command : String  := "n:0000" & CRLF;
      Pointer : Integer := Command'First + 2;
   begin
      begin
         Put
         (  Destination => Command,
            Pointer     => Pointer,
            Value       => Positive (Timeout),
            Base        => 16,
            Field       => 4,
            Fill        => '0',
            Justify     => Right
         );
      exception
         when others =>
            Raise_Exception
            (  Constraint_Error'Identity,
               "Invalid timeout value"
            );
      end;
      Send (Client, Command);
   end Pair;

   procedure Process_Packet (Client : in out ELV_MAX_Cube_Client) is
      Line : String  renames Client.Line.Value;
      Last : Natural renames Client.Line.Last;
   begin
      if Last < Line'First + 1 then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid response, shorter that 2 characters"
         );
      elsif Line (Line'First + 1) /= ':' then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid response, missing colon (:)"
         );
      end if;
      case Line (Line'First) is
         when 'A' =>
            Get_A (Client, Line (Line'First + 2..Last));
         when 'C' =>
            Get_C (Client, Line (Line'First + 2..Last));
         when 'F' =>
            Get_F (Client, Line (Line'First + 2..Last));
         when 'H' =>
            Get_H (Client, Line (Line'First + 2..Last));
            Handshake_Received (ELV_MAX_Cube_Client'Class (Client));
         when 'L' =>
            Get_L (Client, Line (Line'First + 2..Last));
         when 'M' =>
            Get_M (Client, Line (Line'First + 2..Last));
         when 'N' =>
            Get_N (Client, Line (Line'First + 2..Last));
         when 'S' =>
            Get_S (Client, Line (Line'First + 2..Last));
         when others =>
            Trace
            (  ELV_MAX_Cube_Client'Class (Client),
               "Unsupported message '" & Line (Line'First) & '''
            );
      end case;
      if Is_Empty (Client.Secondary) then
         Client.Blocked := False;
      else
         declare
            Data    : String  := Get (Client.Secondary);
            Pointer : Integer := Data'First;
         begin
            Send (Client, Data, Pointer);
            Client.Blocked := Is_Blocking (Data);
         end;
      end if;
   end Process_Packet;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Client      : ELV_MAX_Cube_Client
             )  is
      Index : Integer := Pointer;
      Lock  : Topology_Holder (Client'Access);

      procedure Store_Chunk (Chunk : String) is
      begin
         if Destination'Last - Index + 1 < Chunk'Length  then
            raise Layout_Error;
         end if;
         Index := Index + Chunk'Length;
      end Store_Chunk;
      procedure Store is new Get_Metadata_Unchecked (Store_Chunk);
   begin
      if (  Pointer < Destination'First
         or else
            (  Pointer > Destination'Last
            and then
               Pointer /= Destination'Last + 1
         )  )  then
         raise Layout_Error;
      end if;
      Store (Client, Destination (Index..Destination'Last));
      Pointer := Index;
   end Put;

   procedure Put_Metadata
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : String
             )  is
      Output : Integer := Pointer;
      First  : Boolean := True;

      procedure Put (Text : String) is
      begin
         Put (Destination, Output, Text);
      end Put;

      procedure Get_Device
                (  Data    : String;
                   Pointer : in out Integer;
                   No      : Positive
                )  is
         Kind_Of : Device_Type;
         Name    : Integer;
         Serial   : Integer;
         Length  : Natural;
         Address : RF_Address;
         ID      : Room_ID;
      begin
         Get_Type (Data, Pointer, Kind_Of);
         Get_Address (Data, Pointer, Address);
         if Pointer + 9 > Data'Last then
            raise Data_Error with
                  "missing device serial number (device " &
                  Image (No) &
                  ')';
         end if;
         Serial  := Pointer;
         Pointer := Pointer + 10;
         Length  := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Name    := Pointer;
         Pointer := Pointer + Length;
         if Pointer - Data'Last > 1 then
            raise Data_Error with
                  "wrong name length (device " & Image (No) & ')';
         end if;
         if Pointer > Data'Last then
            raise Data_Error with
                  "missing room ID (device " & Image (No) & ')';
         end if;
         ID := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Put ("(Name=" & Quote (Data (Name..Name + Length - 1)));
         Put (", Address=" & Image (Address));
         Put (", Serial=" & Data (Serial..Serial + 9));
         Put (", Type=" & Image (Kind_Of));
         if ID /= No_Room then
            Put (", Room=" & Image (Integer (ID)));
         end if;
         Put (")");
      end Get_Device;

      procedure Get_Room
                (  Data    : String;
                   Pointer : in out Integer;
                   No      : Positive
                )  is
         ID      : Room_ID;
         Name    : Integer;
         Length  : Natural;
         Address : RF_Address;
      begin
         if Pointer > Data'Last then
            raise Data_Error with
                  "missing room ID (room " & Image (No) & ')';
         end if;
         ID := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         if Pointer > Data'Last then
            raise Data_Error with
                  "missing room name length (room " & Image (No) & ')';
         end if;
         Length  := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Name    := Pointer;
         Pointer := Pointer + Length;
         if Pointer - Data'Last > 1 then
            raise Data_Error with
                  "wrong name length (room " & Image (No) & ')';
         end if;
         Get_Address (Data, Pointer, Address);
         Put ("(Name=" & Quote (Data (Name..Name + Length - 1)));
         Put (", Address=" & Image (Address));
         Put (", ID=" & Image (Integer (ID)) & ")");
      end Get_Room;

      Input : Integer := Data'First;
      Count : Integer;
   begin
      if (  Data (Input) /= 'V'
         or else
            Data (Input + 1) /= Character'Val (2)
         )  then
         raise Data_Error with "malformed data header";
      end if;
      Input := Input + 2;
      if Input <= Data'Last then
         Count := Character'Pos (Data (Input));
         Input := Input + 1;
         Put ("Rooms=(");
         for Room in 1..Count loop
            if First then
               First := False;
            else
               Put (", ");
            end if;
            Get_Room (Data, Input, Room);
         end loop;
         Put ("), Devices=(");
         if Input <= Data'Last then
            Count := Character'Pos (Data (Input));
            Input := Input + 1;
            First := True;
            for Device in 1..Count loop
               if First then
                  First := False;
               else
                  Put (", ");
               end if;
               Get_Device (Data, Input, Device);
            end loop;
         end if;
         Put (")");
      end if;
      Pointer := Output;
   end Put_Metadata;

   procedure Query_Device_Configuration
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address
             )  is
   begin
      Send (Client, "c:" & Image (Address) & CRLF);
   end Query_Device_Configuration;

   procedure Query_Devices (Client : in out ELV_MAX_Cube_Client) is
   begin
      Send (Client, "l:" & CRLF);
   end Query_Devices;

   procedure Query_NTP_Servers (Client : in out ELV_MAX_Cube_Client) is
   begin
      Send (Client, "f:" & CRLF);
   end Query_NTP_Servers;

   procedure Query_Unconfigured_Devices
             (  Client : in out ELV_MAX_Cube_Client
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      for Index in 1..Get_Size (Client.Topology.Devices) loop
         declare
            Device : Device_Descriptor'Class renames
                     Ptr (Get (Client.Topology.Devices, Index)).all;
         begin
            case Device.Kind_Of is
               when Cube | Eco_Button | Unknown =>
                  null;
               when Radiator_Thermostat..Shutter_Contact =>
                  if 0 = (Device.Init and Have_Settings) then
                     Send
                     (  Client,
                        "c:" & Image (Device.Data.Address) & CRLF
                     );
                  end if;
            end case;
         end;
      end loop;
   end Query_Unconfigured_Devices;

   procedure Reboot (Serial_No : String; Port : Port_Type := 23272) is
      Address : aliased Sock_Addr_Type;
      Last    : Stream_Element_Offset;
      Socket  : Socket_Type := No_Socket;
   begin
      if Serial_No'Length /= 10 then
         raise Constraint_Error;
      end if;
      declare
         Announce : constant String :=
                       "eQ3Max*" & Character'Val (0) & Serial_No & 'R';
      begin
         Create_Socket (Socket, Family_Inet, Socket_Datagram);
         Set_Socket_Option
         (  Socket,
            Socket_Level,
            (Reuse_Address, True)
         );
         Set_Socket_Option
         (  Socket,
            Socket_Level,
            (Broadcast, True)
         );
         Address.Addr := Broadcast_Inet_Addr;
         Address.Port := Port;
         Send_Socket
         (  Socket,
            From_String (Announce),
            Last,
            Address'Access
         );
         Close_Socket (Socket);
      end;
   exception
      when Error : others =>
         if Socket /= No_Socket then
            begin
               Close_Socket (Socket);
            exception
               when others =>
                  null;
            end;
         end if;
         raise;
   end Reboot;

   procedure Release (Lock : in out Topology_Holder) is
   begin
      if not Lock.Released then
         Lock.Released := True;
         Lock.Client.Topology.Self.Lock.Release;
      end if;
   end Release;

   procedure Rename_Device
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address;
                Name    : String
             )  is
      procedure Copy
                (  From : Device_Parameters;
                   To   : in out Device_Parameters
                )  is
      begin
         To.Room      := From.Room;
         To.Address   := From.Address;
         To.Serial_No := From.Serial_No;
         case To.Kind_Of is
            when Cube | Shutter_Contact | Eco_Button | Unknown =>
               null;
            when Radiator_Thermostat..Wall_Thermostat =>
               To.Comfort     := From.Comfort;
               To.Eco         := From.Eco;
               To.Max         := From.Max;
               To.Min         := From.Min;
               To.Offset      := From.Offset;
               To.Window_Open := From.Window_Open;
               To.Schedule := From.Schedule;
               case To.Kind_Of is
                  when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                     To.Window_Time     := From.Window_Time;
                     To.Boost_Time      := From.Boost_Time;
                     To.Boost_Valve     := From.Boost_Valve;
                     To.Decalcification := From.Decalcification;
                     To.Max_Valve       := From.Max_Valve;
                     To.Valve_Offset    := From.Valve_Offset;
                  when others =>
                     null;
               end case;
         end case;
      end Copy;
      Lock  : Topology_Holder (Client'Access);
      Index : constant Positive :=
                       Get_Device_Unchecked (Client, Address);
   begin
      if Name'Length > 255 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The device name is longer than 255 characters"
         );
      end if;
      declare
         Old_Handle     : constant Device_Handles.Handle :=
                                   Get (Client.Topology.Devices, Index);
         Old_Descriptor : Device_Descriptor'Class renames
                          Ptr (Old_Handle).all;
      begin
         if Old_Descriptor.Data.Name /= Name then
            if Old_Descriptor.Data.Name_Length = Name'Length then
               Old_Descriptor.Data.Name := Name;
            else
               declare
                  New_Handle : Device_Handles.Handle;
               begin
                  Set
                  (  New_Handle,
                     new Device_Descriptor
                         (  Kind_Of     => Old_Descriptor.Kind_Of,
                            Name_Length => Name'Length
                  )      );
                  declare
                     New_Descriptor : Device_Descriptor'Class renames
                                      Ptr (New_Handle).all;
                  begin
                     New_Descriptor.Init := Old_Descriptor.Init;
                     New_Descriptor.Room := Old_Descriptor.Room;
                     New_Descriptor.Last := Old_Descriptor.Last;
                     New_Descriptor.Data.Name := Name;
                     Copy (Old_Descriptor.Data, New_Descriptor.Data);
                  end;
                  Replace (Client.Topology.Devices, Index, New_Handle);
               end;
            end if;
            Set_Metadata_Unchecked (Client);
         end if;
      end;
   end Rename_Device;

   procedure Rename_Room
             (  Client : in out ELV_MAX_Cube_Client;
                Index  : Positive;
                Name   : String
             )  is
      Lock       : Topology_Holder (Client'Access);
      Old_Handle : constant Room_Handles.Handle :=
                            Get (Client.Topology.Rooms, Index);
   begin
      if Name'Length > 255 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The room name is longer than 255 characters"
         );
      end if;
      declare
         Old_Descriptor : Room_Descriptor'Class renames
                          Ptr (Old_Handle).all;
      begin
         if Old_Descriptor.Name /= Name then
            if Old_Descriptor.Name_Length = Name'Length then
               Old_Descriptor.Name := Name;
            else
               declare
                  New_Handle : Room_Handles.Handle;
               begin
                  Set (New_Handle, new Room_Descriptor (Name'Length));
                  declare
                     New_Descriptor : Room_Descriptor'Class renames
                                      Ptr (New_Handle).all;
                  begin
                     New_Descriptor.ID     := Old_Descriptor.ID;
                     New_Descriptor.Master := Old_Descriptor.Master;
                     New_Descriptor.Count  := Old_Descriptor.Count;
                     New_Descriptor.Name   := Name;
                  end;
                  Replace (Client.Topology.Rooms, Index, New_Handle);
               end;
            end if;
            Set_Metadata_Unchecked (Client);
         end if;
      end;
   end Rename_Room;

   procedure Rename_Room
             (  Client : in out ELV_MAX_Cube_Client;
                ID     : Room_ID;
                Name   : String
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Rename_Room
      (  Client,
         Find_Room_Unchecked (Client, ID),
         Name
      );
   end Rename_Room;

   procedure Reset_Devices (Client : in out ELV_MAX_Cube_Client) is
      Lock : Topology_Holder (Client'Access);
   begin
      Clean_Up (Client, False);
      Send (Client, "a:" & CRLF);
      Set_Metadata_Unchecked (Client);
   end Reset_Devices;

   procedure Reset_Error (Client : in out ELV_MAX_Cube_Client) is
   begin
      Send (Client, "r:" & CRLF);
   end Reset_Error;

   procedure Reset_Metadata
             (  Client     : in out ELV_MAX_Cube_Client;
                Data       : String;
                S_Commands : out Natural
             )  is
      Devices : Device_Maps.Map renames Client.Topology.Devices;
      Rooms   : Rooms_Maps.Map  renames Client.Topology.Rooms;
      Pointer : Integer := Data'First;
   begin
      S_Commands := 0;
      Clean_Up (Client, False);
--    Send (Client, "a:" & CRLF); -- Delete everything
      declare
         Lock : Topology_Holder (Client'Access);
      begin
         Set_From_Metadata_Unchecked (Client, Lock, Data, False);
      end;
      declare
         Lock : Topology_Holder (Client'Access);
      begin
         for Index in 1..Get_Size (Rooms) loop -- The rooms
            declare
               Location : Room_Descriptor'Class renames
                          Ptr (Get (Rooms, Index)).all;
            begin
               for Member in 1..Location.Count loop -- Room's devices
                  Wake_Up_Unchecked (Client, Location.ID);
                  declare
                     Device : Device_Descriptor'Class renames
                        Ptr (Get (Devices, (Location.ID, Member))).all;
                  begin
                     Link_Unchecked
                     (  Client,
                        Device.Kind_Of,
                        Device.Data.Address,
                        Location,
                        Is_New_Master (Client, Device.Kind_Of, Location),
                        S_Commands
                     );
                     Send
                     (  Client,  -- Set group address for the device
                        (  "s:"
                        &  To_Base64
                           (  S_Group
                           &  Encode (Device.Data.Address)
                           &  Character'Val (0)
                           &  Encode (Location.ID)
                           )
                        &  CRLF
                     )  );
                     S_Commands := S_Commands + 1;
                  end;
               end loop;
            end;
         end loop;
         Set_Metadata_Unchecked (Client);
      end;
   end Reset_Metadata;

   procedure Reset_Metadata
             (  Client : in out ELV_MAX_Cube_Client;
                Data   : String
             )  is
      S_Commands : Natural := 0;
   begin
      Reset_Metadata (Client, Data, S_Commands);
   end Reset_Metadata;

   procedure Send
             (  Client : in out ELV_MAX_Cube_Client;
                Data   : String
             )  is
      Pointer : Integer := Data'First;
      Full    : Boolean;
   begin
      if Client.Blocked or else not Is_Empty (Client.Secondary) then
         Put (Client.Secondary, Data, Full);
         if Full then
            Raise_Exception
            (  Use_Error'Identity,
               Busy_Error
            );
         end if;
      elsif Available_To_Send (Client) < Data'Length then
         Raise_Exception (Use_Error'Identity, Busy_Error);
      else
         Send (Client, Data, Pointer);
         Client.Blocked := Is_Blocking (Data);
      end if;
   end Send;

   procedure Set_From_Metadata_Unchecked
             (  Client     : in out ELV_MAX_Cube_Client;
                Lock       : in out Topology_Holder'Class;
                Data       : String;
                M_Response : Boolean
             )  is
      procedure Get_Device
                (  Data    : String;
                   Pointer : in out Integer;
                   No      : Positive
                )  is
         Kind_Of : Device_Type;
         Name    : Integer;
         Serial   : Integer;
         Length  : Natural;
         Address : RF_Address;
         ID      : Room_ID;
         Index   : Integer;
      begin
         Get_Type (Data, Pointer, Kind_Of);
         Get_Address (Data, Pointer, Address);
         if Pointer + 9 > Data'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "missing device serial number (device "
               &  Image (No)
               &  ')'
            )  );
         end if;
         Serial  := Pointer;
         Pointer := Pointer + 10;
         Length  := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Name    := Pointer;
         Pointer := Pointer + Length;
         if Pointer - Data'Last > 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "wrong name length (device "
               &  Image (No)
               &  ')'
            )  );
         end if;
         if Pointer > Data'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "missing room ID (device "
               &  Image (No)
               &  ')'
            )  );
         end if;
         ID := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Index := Find (Client.Topology.Rooms, ID);
         declare
            Object : constant Device_Descriptor_Ptr :=
                     new Device_Descriptor (Kind_Of, Length);
            Device : Device_Handles.Handle;
         begin
            Set (Device, Object);
            Object.Last.Address   := Address;
            Object.Data.Address   := Address;
            Object.Data.Serial_No := Data (Serial..Serial + 9);
            Object.Data.Name      := Data (Name..Name + Length - 1);
            if Index > 0 then -- Valid room ID
               Object.Data.Room := ID;
               Object.Room := Get (Client.Topology.Rooms, Index);
               Ptr (Object.Room).Count := Ptr (Object.Room).Count + 1;
               Add
               (  Client.Topology.Devices,
                  (ID, Ptr (Object.Room).Count),
                  Device
               );
            else
               Object.Data.Room := No_Room;
               Client.Roomless := Client.Roomless + 1;
               Add
               (  Client.Topology.Devices,
                  (No_Room, Client.Roomless),
                  Device
               );
            end if;
         end;
      end Get_Device;

      procedure Get_Room
                (  Data    : String;
                   Pointer : in out Integer;
                   No      : Positive
                )  is
         ID      : Room_ID;
         Name    : Integer;
         Length  : Natural;
         Address : RF_Address;
      begin
         if Pointer > Data'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "missing room ID (room "
               &  Image (No)
               &  ')'
            )  );
         end if;
         ID := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         if Is_In (Client.Topology.Rooms, ID) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "duplicated room ID "
               &  Image (Integer (ID))
            )  );
         end if;
         if Pointer > Data'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "missing room name length (room "
               &  Image (No)
               &  ')'
            )  );
         end if;
         Length  := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Name    := Pointer;
         Pointer := Pointer + Length;
         if Pointer - Data'Last > 1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "wrong name length (room "
               &  Image (No)
               &  ')'
            )  );
         end if;
         Get_Address (Data, Pointer, Address);
         declare
            Object : constant Room_Descriptor_Ptr :=
                     new Room_Descriptor (Length);
            Room   : Room_Handles.Handle;
         begin
            Set (Room, Object);
            Object.ID     := ID;
            Object.Count  := 0;
            Object.Master := Address;
            Object.Name   := Data (Name..Name + Length - 1);
            Add (Client.Topology.Rooms, ID, Room);
         end;
      end Get_Room;

      Pointer : Integer := Data'First;
      Count   : Integer;
   begin
      if (  Data (Pointer) /= 'V'
        or else
            Data (Pointer + 1) /= Character'Val (2)
         )
      then
         Raise_Exception
         (  Data_Error'Identity,
            "malformed data header"
         );
      end if;
      Pointer := Pointer + 2;
      if Pointer <= Data'Last then
         Count   := Character'Pos (Data (Pointer ));
         Pointer := Pointer + 1;
         for Room in 1..Count loop
            Get_Room (Data, Pointer, Room);
         end loop;
         if Pointer <= Data'Last then
            Count   := Character'Pos (Data (Pointer));
            Pointer := Pointer + 1;
            for Device in 1..Count loop
               Get_Device (Data, Pointer, Device);
            end loop;
         end if;
      end if;
      for Index in 1..Get_Size (Client.Topology.Devices) loop
        declare
           Device : Device_Descriptor'Class renames
                    Ptr (Get (Client.Topology.Devices, Index)).all;
        begin
           Replace
           (  Client.Topology.RF,
              Device.Data.Address,
              Index
           );
        end;
      end loop;
      if M_Response then
         Client.Ready := Client.Ready or Got_M;
      end if;
      begin
         Release (Lock);
         Configuration_Updated
         (  ELV_MAX_Cube_Client'Class (Client),
            (Kind_Of => Topology_Update)
         );
      exception
         when others =>
            null;
      end;
   exception
      when Error : Data_Error =>
         if M_Response then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid M-response, " & Exception_Message (Error)
            );
         else
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid metadata, " & Exception_Message (Error)
            );
         end if;
   end Set_From_Metadata_Unchecked;

   procedure Set_Metadata (Client : in out ELV_MAX_Cube_Client) is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Metadata_Unchecked (Client);
   end Set_Metadata;

   procedure Set_Metadata_Unchecked
             (  Client  : in out ELV_MAX_Cube_Client;
                Exclude : RF_Address := 0
             )  is
      Count : Natural := 0;

      procedure Send_Chunk (Chunk : String) is
      begin
         Send
         (  Client,
            "m:" & Image2 (Count) & "," & To_Base64 (Chunk) & CRLF
         );
         Count := Count + 1;
      end Send_Chunk;
      procedure Send_Metadata is
         new Get_Metadata_Unchecked (Send_Chunk);

      Data : String (1..Chunk_Size); -- Base64 encoding source
   begin
      Send_Metadata (Client, Data, Exclude);
   end Set_Metadata_Unchecked;

   procedure Set_NTP_Servers
             (  Client : in out ELV_MAX_Cube_Client;
                List   : String
             )  is
   begin
      Set_NTP_Servers (Client, Create_List (List));
   end Set_NTP_Servers;

   procedure Set_NTP_Servers
             (  Client : in out ELV_MAX_Cube_Client;
                List   : Servers_List.Set
             )  is
      use Servers_List;
      Length : Integer := Get_Size (List) - 1;
   begin
      if Length < 0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Empty list of NTP servers"
         );
      end if;
      for Index in 1..Get_Size (List) loop
         declare
            Server : constant String := Get (List, Index);
         begin
            Length := Length + Server'Length;
         end;
      end loop;
      declare
         Text    : String (1..Length + 4);
         Pointer : Integer := 3;
      begin
         Text (1..2) := "f:";
         for Index in 1..Get_Size (List) loop
            if Index > 1 then
               Text (Pointer) := ',';
               Pointer := Pointer + 1;
            end if;
            declare
               Server : constant String := Get (List, Index);
            begin
               Text (Pointer..Pointer + Server'Length - 1) := Server;
               Pointer := Pointer + Server'Length;
            end;
         end loop;
         Text (Pointer..Pointer + 1) := CRLF;
         Send (Client, Text);
      end;
   end Set_NTP_Servers;

   procedure Set_Thermostat_Automatic_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : Device_Descriptor'Class;
                Temperature : Centigrade
             )  is
   begin
      if Device.Kind_Of not in Radiator_Thermostat..Wall_Thermostat then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The device "
            &  Image (Device.Data.Address)
            &  " is not a thermostat"
         )  );
      end if;
      if Temperature = Centigrade'First then
         Send
         (  Client,
            (  "s:"
            &  To_Base64
               (  S_Head
               &  Encode (Device.Data.Address)
               &  Character'Val (Ptr (Device.Room).ID)
               &  Character'Val (0)
               )
            &  CRLF
         )  );
      else
         Send
         (  Client,
            (  "s:"
            &  To_Base64
               (  S_Head
               &  Encode (Device.Data.Address)
               &  Character'Val (Ptr (Device.Room).ID)
               &  Character'Val (Encode (Temperature))
               )
            &  CRLF
         )  );
      end if;
   end Set_Thermostat_Automatic_Unchecked;

   procedure Set_Thermostat_Automatic
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Temperature : Centigrade := Centigrade'First
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Automatic_Unchecked
      (  Client,
         Ptr (Get (Client.Topology.Devices, Index)).all,
         Temperature
      );
   end Set_Thermostat_Automatic;

   procedure Set_Thermostat_Automatic
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Temperature : Centigrade := Centigrade'First
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Automatic_Unchecked
      (  Client,
         Ptr
         (  Get
            (  Client.Topology.Devices,
               Get_Device_Unchecked (Client, Address)
         )  ) .all,
         Temperature
      );
   end Set_Thermostat_Automatic;

   procedure Set_Thermostat_Boost_Unchecked
             (  Client : in out ELV_MAX_Cube_Client;
                Device : Device_Descriptor'Class
             )  is
   begin
      if Device.Kind_Of not in Radiator_Thermostat..Wall_Thermostat then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The device "
            &  Image (Device.Data.Address)
            &  " is not a thermostat"
         )  );
      end if;
      Send
      (  Client,
         (  "s:"
         &  To_Base64
            (  S_Head
            &  Encode (Device.Data.Address)
            &  Character'Val (Ptr (Device.Room).ID)
            &  Character'Val (2#1100_0000#)
            )
         &  CRLF
      )  );
   end Set_Thermostat_Boost_Unchecked;

   procedure Set_Thermostat_Boost
             (  Client : in out ELV_MAX_Cube_Client;
                Index  : Positive
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Boost_Unchecked
      (  Client,
         Ptr (Get (Client.Topology.Devices, Index)).all
      );
   end Set_Thermostat_Boost;

   procedure Set_Thermostat_Boost
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Boost_Unchecked
      (  Client,
         Ptr
         (  Get
            (  Client.Topology.Devices,
               Get_Device_Unchecked (Client, Address)
         )  ) .all
      );
   end Set_Thermostat_Boost;

   procedure Set_Thermostat_Display
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : Device_Descriptor'Class;
                Temperature : Display_Mode
             )  is
   begin
      if Device.Kind_Of /= Wall_Thermostat then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The device "
            &  Image (Device.Data.Address)
            &  " is not a wall thermostat"
         )  );
      end if;
      case Temperature is
         when Display_Is_Temperature =>
            Send
            (  Client,
               (  "s:"
               &  To_Base64
                  (  S_Mode
                  &  Encode (Device.Data.Address)
                  &  Character'Val (Ptr (Device.Room).ID)
                  &  Character'Val (4)
                  )
               &  CRLF
            )  );
         when Display_Set_Temperature =>
            Send
            (  Client,
               (  "s:"
               &  To_Base64
                  (  S_Mode
                  &  Encode (Device.Data.Address)
                  &  Character'Val (Ptr (Device.Room).ID)
                  &  Character'Val (0)
                  )
               &  CRLF
            )  );
      end case;
   end Set_Thermostat_Display;

   procedure Set_Thermostat_Display
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Temperature : Display_Mode
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Display
      (  Client,
         Ptr (Get (Client.Topology.Devices, Index)).all,
         Temperature
      );
   end Set_Thermostat_Display;

   procedure Set_Thermostat_Display
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Temperature : Display_Mode
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Display
      (  Client,
         Ptr
         (  Get
            (  Client.Topology.Devices,
               Get_Device_Unchecked (Client, Address)
         )  ) .all,
         Temperature
      );
   end Set_Thermostat_Display;

   procedure Set_Thermostat_Parameters_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : in out Device_Descriptor'Class;
                Comfort     : Centigrade;
                Eco         : Centigrade;
                Max         : Centigrade;
                Min         : Centigrade;
                Offset      : Centigrade;
                Window_Open : Centigrade;
                Window_Time : Day_Duration;
                Mode        : Setting_Mode
             )  is
      Airing : constant Float :=
               Float'Max
               (  0.0,
                  Float'Min
                  (  Float (Window_Time) / Float (Five_Minutes),
                     255.0
               )  );
   begin
      if Device.Kind_Of not in Radiator_Thermostat
                            .. Radiator_Thermostat_Plus then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The device "
            &  Image (Device.Data.Address)
            &  " is not a radiator thermostat"
         )  );
      end if;
      if 0 /= (S_Command and Mode) then
         Send
         (  Client,
            (  "s:"
            &  To_Base64
               (  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Character'Val (16#11#)
               &  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Encode (Device.Data.Address)
               &  Character'Val (Ptr (Device.Room).ID)
               &  Character'Val (Encode (Comfort))
               &  Character'Val (Encode (Eco))
               &  Character'Val (Encode (Max))
               &  Character'Val (Encode (Min))
               &  Character'Val (Encode (Offset + 3.5))
               &  Character'Val (Encode (Window_Open))
               &  Character'Val (Integer (Float'Ceiling (Airing)))
               )
            &  CRLF
         )  );
      end if;
      if 0 /= (S_Response and Mode) then
         Device.Data.Comfort     := Comfort;
         Device.Data.Eco         := Eco;
         Device.Data.Min         := Min;
         Device.Data.Max         := Max;
         Device.Data.Offset      := Offset;
         Device.Data.Window_Open := Window_Open;
         Device.Data.Window_Time := Window_Time;
      end if;
   end Set_Thermostat_Parameters_Unchecked;

   procedure Set_Thermostat_Parameters
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Comfort     : Centigrade;
                Eco         : Centigrade;
                Max         : Centigrade;
                Min         : Centigrade;
                Offset      : Centigrade;
                Window_Open : Centigrade;
                Window_Time : Day_Duration;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Parameters_Unchecked
      (  Client      => Client,
         Device      => Ptr (Get (Client.Topology.Devices, Index)).all,
         Comfort     => Comfort,
         Eco         => Eco,
         Max         => Max,
         Min         => Min,
         Offset      => Offset,
         Window_Open => Window_Open,
         Window_Time => Window_Time,
         Mode        => Mode
      );
   end Set_Thermostat_Parameters;

   procedure Set_Thermostat_Parameters
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Comfort     : Centigrade;
                Eco         : Centigrade;
                Max         : Centigrade;
                Min         : Centigrade;
                Offset      : Centigrade;
                Window_Open : Centigrade;
                Window_Time : Day_Duration;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Parameters_Unchecked
      (  Client      => Client,
         Device      => Ptr
                        (  Get
                           (  Client.Topology.Devices,
                              Get_Device_Unchecked (Client, Address)
                         )  ) .all,
         Comfort     => Comfort,
         Eco         => Eco,
         Max         => Max,
         Min         => Min,
         Offset      => Offset,
         Window_Open => Window_Open,
         Window_Time => Window_Time,
         Mode        => Mode
      );
   end Set_Thermostat_Parameters;

   procedure Set_Thermostat_Parameters_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : in out Device_Descriptor'Class;
                Comfort     : Centigrade;
                Eco         : Centigrade;
                Max         : Centigrade;
                Min         : Centigrade;
                Offset      : Centigrade;
                Window_Open : Centigrade;
                Mode        : Setting_Mode
             )  is
   begin
      if Device.Kind_Of /= Wall_Thermostat then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The device "
            &  Image (Device.Data.Address)
            &  " is not a wall thermostat"
         )  );
      end if;
      if 0 /= (S_Command and Mode) then
         Send
         (  Client,
            (  "s:"
            &  To_Base64
               (  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Character'Val (16#11#)
               &  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Encode (Device.Data.Address)
               &  Character'Val (Ptr (Device.Room).ID)
               &  Character'Val (Encode (Comfort))
               &  Character'Val (Encode (Eco))
               &  Character'Val (Encode (Max))
               &  Character'Val (Encode (Min))
               &  Character'Val (Encode (Offset + 3.5))
               &  Character'Val (Encode (Window_Open))
               &  Character'Val (0)
               )
            &  CRLF
         )  );
      end if;
      if 0 /= (S_Response and Mode) then
         Device.Data.Comfort     := Comfort;
         Device.Data.Eco         := Eco;
         Device.Data.Min         := Min;
         Device.Data.Max         := Max;
         Device.Data.Offset      := Offset;
         Device.Data.Window_Open := Window_Open;
      end if;
   end Set_Thermostat_Parameters_Unchecked;

   procedure Set_Thermostat_Parameters
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Comfort     : Centigrade;
                Eco         : Centigrade;
                Max         : Centigrade;
                Min         : Centigrade;
                Offset      : Centigrade;
                Window_Open : Centigrade;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Parameters_Unchecked
      (  Client  => Client,
         Device  => Ptr (Get (Client.Topology.Devices, Index)).all,
         Comfort => Comfort,
         Eco     => Eco,
         Max     => Max,
         Min     => Min,
         Offset  => Offset,
         Window_Open => Window_Open,
         Mode    => Mode
      );
   end Set_Thermostat_Parameters;

   procedure Set_Thermostat_Parameters
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Comfort     : Centigrade;
                Eco         : Centigrade;
                Max         : Centigrade;
                Min         : Centigrade;
                Offset      : Centigrade;
                Window_Open : Centigrade;
                Mode        : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Parameters_Unchecked
      (  Client      => Client,
         Device      => Ptr
                        (  Get
                           (  Client.Topology.Devices,
                              Get_Device_Unchecked (Client, Address)
                        )  ) .all,
         Comfort     => Comfort,
         Eco         => Eco,
         Max         => Max,
         Min         => Min,
         Offset      => Offset,
         Window_Open => Window_Open,
         Mode        => Mode
      );
   end Set_Thermostat_Parameters;

   procedure Set_Thermostat_Schedule_Unchecked
             (  Client   : in out ELV_MAX_Cube_Client;
                Device   : in out Device_Descriptor'Class;
                Day      : Week_Day;
                Schedule : Points_List;
                Mode     : Setting_Mode
             )  is
      Last    : Day_Duration;
      Program : String (1..13 * 2);
      Pointer : Integer := 1;
   begin
      if Schedule'Length = 0 then
         Raise_Exception
         (  Mode_Error'Identity,
            "Empty day schedule"
         );
      end if;
      if Device.Kind_Of not in Radiator_Thermostat..Wall_Thermostat then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The device "
            &  Image (Device.Data.Address)
            &  " is not a thermostat"
         )  );
      end if;
      for Index in Schedule'Range loop
         if Index = Schedule'First then
            Last := Schedule (Index).Last;
         elsif Last >= Schedule (Index).Last then
            Raise_Exception
            (  Mode_Error'Identity,
               (  "Invalid day schedule, point "
               &  Image (Integer (Index))
               &  " has a lesser or equal scheduled time"
            )  );
         elsif Index = Schedule'Last then
            Last := Day_Duration'Last; -- Applies until the end
         else
            Last := Schedule (Index).Last;
         end if;
         declare
            Word : constant Integer :=
                   (  Integer (Last / 300.0)
                   +  Integer (Encode (Schedule (Index).Point)) * 2**9
                   );
         begin
            Program (Pointer)     := Character'Val (Word / 2**8);
            Program (Pointer + 1) := Character'Val (Word mod 2**8);
            Pointer := Pointer + 2;
         end;
      end loop;
      declare
         First  : constant Character := Program (Pointer - 2);
         Second : constant Character := Program (Pointer - 1);
      begin
         while Pointer <= Program'Last loop -- Fill the program
            Program (Pointer    ) := First;
            Program (Pointer + 1) := Second;
            Pointer := Pointer + 2;
         end loop;
      end;
      if 0 /= (S_Command and Mode) then
         Send
         (  Client,
            (  "s:"
            &  To_Base64
               (  Character'Val (16#00#)
               &  Character'Val (16#04#) -- Room mode
               &  Character'Val (16#10#) -- Week program
               &  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Encode (Device.Data.Address)
               &  Character'Val (Ptr (Device.Room).ID)
               &  Character'Val (Encode (Day))
               &  Program (1..7 * 2)     -- First 7 points
               )
            &  CRLF
         )  );
         Send
         (  Client,
            (  "s:"
            &  To_Base64
               (  Character'Val (16#00#)
               &  Character'Val (16#04#) -- Room mode
               &  Character'Val (16#10#) -- Week program
               &  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Encode (Device.Data.Address)
               &  Character'Val (Ptr (Device.Room).ID)
               &  Character'Val (Encode (Day) + 2#1_0000#)
               &  Program (7 * 2 + 1..Program'Last) -- Last 6 points
               )
            &  CRLF
         )  );
      end if;
      if 0 /= (S_Response and Mode) then
         Device.Data.Schedule (Day) := (Schedule'Length, Schedule);
      end if;
   end Set_Thermostat_Schedule_Unchecked;

   procedure Set_Thermostat_Schedule
             (  Client   : in out ELV_MAX_Cube_Client;
                Index    : Positive;
                Day      : Week_Day;
                Schedule : Points_List;
                Mode     : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Schedule_Unchecked
      (  Client,
         Ptr (Get (Client.Topology.Devices, Index)).all,
         Day,
         Schedule,
         Mode
      );
   end Set_Thermostat_Schedule;

   procedure Set_Thermostat_Schedule
             (  Client   : in out ELV_MAX_Cube_Client;
                Address  : RF_Address;
                Day      : Week_Day;
                Schedule : Points_List;
                Mode     : Setting_Mode := S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Schedule_Unchecked
      (  Client,
         Ptr
         (  Get
            (  Client.Topology.Devices,
               Get_Device_Unchecked (Client, Address)
         )  ) .all,
         Day,
         Schedule,
         Mode
      );
   end Set_Thermostat_Schedule;

   procedure Set_Thermostat_Temperature_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : in out Device_Descriptor'Class;
                Temperature : Centigrade;
                Manual      : Boolean
             )  is
   begin
      if Device.Kind_Of not in Radiator_Thermostat..Wall_Thermostat then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The device "
            &  Image (Device.Data.Address)
            &  " is not a thermostat"
         )  );
      end if;
      Device.Last.New_Temperature := Temperature;
      if Device.Last.Mode = Automatic and then not Manual then
         Send
         (  Client,
            (  "s:"
            &  To_Base64
               (  S_Head
               &  Encode (Device.Data.Address)
               &  Character'Val (Ptr (Device.Room).ID)
               &  Character'Val (Encode (Temperature))
               )
            &  CRLF
         )  );
      else
         Send
         (  Client,
            (  "s:"
            &  To_Base64
               (  S_Head
               &  Encode (Device.Data.Address)
               &  Character'Val (Ptr (Device.Room).ID)
               &  Character'Val (2#0100_0000# + Encode (Temperature))
               )
            &  CRLF
         )  );
      end if;
   end Set_Thermostat_Temperature_Unchecked;

   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Temperature : Centigrade;
                Manual      : Boolean := True
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Temperature_Unchecked
      (  Client,
         Ptr (Get (Client.Topology.Devices, Index)).all,
         Temperature,
         Manual
      );
   end Set_Thermostat_Temperature;

   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Temperature : Centigrade;
                Manual      : Boolean := True
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Temperature_Unchecked
      (  Client,
         Ptr
         (  Get
            (  Client.Topology.Devices,
               Get_Device_Unchecked (Client, Address)
         )  ) .all,
         Temperature,
         Manual
      );
   end Set_Thermostat_Temperature;

   procedure Set_Thermostat_Temperature_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : in out Device_Descriptor'Class;
                Temperature : Centigrade;
                Up_Until    : Time
             )  is
   begin
      if Device.Kind_Of not in Radiator_Thermostat..Wall_Thermostat then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The device "
            &  Image (Device.Data.Address)
            &  " is not a thermostat"
         )  );
      end if;
      Device.Last.New_Temperature := Temperature;
      Send
      (  Client,
         (  "s:"
         &  To_Base64
            (  S_Head
            &  Encode (Device.Data.Address)
            &  Character'Val (Ptr (Device.Room).ID)
            &  Character'Val (2#1000_0000# + Encode (Temperature))
            &  Encode (Up_Until + Client.Clock_Diff)
            )
         &  CRLF
      )  );
   end Set_Thermostat_Temperature_Unchecked;

   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Temperature : Centigrade;
                Up_Until    : Time
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Temperature_Unchecked
      (  Client,
         Ptr (Get (Client.Topology.Devices, Index)).all,
         Temperature,
         Up_Until
      );
   end Set_Thermostat_Temperature;

   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Temperature : Centigrade;
                Up_Until    : Time
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Temperature_Unchecked
      (  Client,
         Ptr
         (  Get
            (  Client.Topology.Devices,
               Get_Device_Unchecked (Client, Address)
         )  ) .all,
         Temperature,
         Up_Until
      );
   end Set_Thermostat_Temperature;

   procedure Set_Thermostat_Valve_Unchecked
             (  Client          : in out ELV_MAX_Cube_Client;
                Device          : in out Device_Descriptor'Class;
                Boost_Time      : Duration;
                Boost_Valve     : Ratio;
                Decalcification : Week_Time;
                Max_Valve       : Ratio;
                Valve_Offset    : Ratio;
                Mode            : Setting_Mode
             )  is
   begin
      if Device.Kind_Of not in Radiator_Thermostat
                            .. Radiator_Thermostat_Plus then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The device "
            &  Image (Device.Data.Address)
            &  " is not a radiator thermostat"
         )  );
      end if;
      if 0 /= (S_Command and Mode) then
         Send
         (  Client,
            (  "s:"
            &  To_Base64
               (  Character'Val (16#00#)
               &  Character'Val (16#04#)
               &  Character'Val (16#12#)
               &  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Character'Val (16#00#)
               &  Encode (Device.Data.Address)
               &  Character'Val (Ptr (Device.Room).ID)
               &  Encode (Boost_Time, Boost_Valve)
               &  Character'Val
                  (  Encode (Decalcification.Day) * 32
                  +  Integer (Decalcification.Time / 3600.0)
                  )
               &  Character'Val (Integer (Float (Max_Valve) * 255.0))
               &  Character'Val (Integer (Float (Valve_Offset) * 255.0))
               )
            &  CRLF
         )  );
      end if;
      if 0 /= (S_Response and Mode) then
         Device.Data.Boost_Time      := Boost_Time;
         Device.Data.Boost_Valve     := Boost_Valve;
         Device.Data.Decalcification := Decalcification;
         Device.Data.Max_Valve       := Max_Valve;
         Device.Data.Valve_Offset    := Valve_Offset;
      end if;
   end Set_Thermostat_Valve_Unchecked;

   procedure Set_Thermostat_Valve
             (  Client          : in out ELV_MAX_Cube_Client;
                Index           : Positive;
                Boost_Time      : Duration  := 3.0;
                Boost_Valve     : Ratio     := 1.0;
                Decalcification : Week_Time := (Mo, 12.0 * 3600.0);
                Max_Valve       : Ratio     := 1.0;
                Valve_Offset    : Ratio     := 0.0;
                Mode            : Setting_Mode :=
                                               S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Valve_Unchecked
      (  Client => Client,
         Device => Ptr (Get (Client.Topology.Devices, Index)).all,
         Boost_Time      => Boost_Time,
         Boost_Valve     => Boost_Valve,
         Decalcification => Decalcification,
         Max_Valve       => Max_Valve,
         Valve_Offset    => Valve_Offset,
         Mode            => Mode
      );
   end Set_Thermostat_Valve;

   procedure Set_Thermostat_Valve
             (  Client          : in out ELV_MAX_Cube_Client;
                Address         : RF_Address;
                Boost_Time      : Duration  := 3.0;
                Boost_Valve     : Ratio     := 1.0;
                Decalcification : Week_Time := (Mo, 12.0 * 3600.0);
                Max_Valve       : Ratio     := 1.0;
                Valve_Offset    : Ratio     := 0.0;
                Mode            : Setting_Mode :=
                                               S_Command or S_Response
             )  is
      Lock : Topology_Holder (Client'Access);
   begin
      Set_Thermostat_Valve_Unchecked
      (  Client => Client,
         Device => Ptr
                   (  Get
                      (  Client.Topology.Devices,
                         Get_Device_Unchecked (Client, Address)
                   )  ) .all,
         Boost_Time      => Boost_Time,
         Boost_Valve     => Boost_Valve,
         Decalcification => Decalcification,
         Max_Valve       => Max_Valve,
         Valve_Offset    => Valve_Offset,
         Mode            => Mode
      );
   end Set_Thermostat_Valve;

   procedure Set_Time
             (  Client : in out ELV_MAX_Cube_Client;
                Winter : Zone_Data := CET;
                Summer : Zone_Data := CEST
             )  is
      use Ada.Calendar.Time_Zones;
      use Ada.Characters.Handling;

      function Image (Value : Time_Zone_Offset) return String is
         Seconds : Unsigned_32;
      begin
         if Value >= 0.0 then
            Seconds := Unsigned_32 (Value);
         else
            Seconds := 16#FFFF_FFFF# - Unsigned_32 (1.0 - Value);
         end if;
         return
         (  4 => Character'Val (             Seconds      and 16#FF#),
            3 => Character'Val (Shift_Right (Seconds,  8) and 16#FF#),
            2 => Character'Val (Shift_Right (Seconds, 16) and 16#FF#),
            1 => Character'Val (Shift_Right (Seconds, 24)           )
         );
      end Image;
      Zone : String (1..24) := (others => Character'Val (0));
   begin
      Zone (1..Winter.Name'Length)       := Winter.Name;
      Zone (13..12 + Summer.Name'Length) := Summer.Name;
      Zone ( 6) := Character'Val (Winter.Start);
      Zone ( 7) := Character'Val (Day_Of_Week'Pos (Winter.Day));
      Zone ( 8) := Character'Val (Integer (Winter.Hour));
      Zone (18) := Character'Val (Summer.Start);
      Zone (19) := Character'Val (Day_Of_Week'Pos (Summer.Day));
      Zone (20) := Character'Val (Integer (Summer.Hour));
      if Winter.Start = Summer.Start then
         if Winter.Start < 6 then
            Zone (18) := Character'Val (Winter.Start + 6);
         else
            Zone (18) := Character'Val (Summer.Start - 6);
         end if;
      end if;
--    Zone ( 8) := Character'Val (3);
--    Zone (20) := Character'Val (2);
      Zone ( 9..12) := Image (Winter.Offset);
      Zone (21..24) := Image (Summer.Offset);
      declare
         Now  : constant Time := Clock;
      begin
         Send
         (  Client,
            (  "v:"
            &  To_Base64 (Zone)
            &  ","
            &  To_Lower
               (  Image
                  (  Integer
                     (  Now
--                  -  Duration (UTC_Time_Offset (Now)) * 60.0
                     -  Epoch
                     ),
                     Base => 16
               )  )
            &  CRLF
         )  );
      end;
   end Set_Time;

   function Sort_Unchecked
            (  Client : ELV_MAX_Cube_Client;
               Data   : RF_Address_Array
            )  return RF_Address_Array is
      RF      : Address_Maps.Map renames Client.Topology.RF;
      Devices : Device_Maps.Map  renames Client.Topology.Devices;
      Rooms   : Rooms_Maps.Map   renames Client.Topology.Rooms;
      Result  : RF_Address_Array (1..Data'Length);
      Size    : Natural := 0;

      function Less (Left, Right : RF_Address) return Boolean is
         Index : Integer;
      begin
         if Left = Right then
            return False;
         end if;
         Index := Find (RF, Left);
         if Index <= 0 then               -- Orphaned Left
            if Find (RF, Right) <= 0 then -- Orphaned Right
               return Left < Right;       -- Ordered orphaned by address
            else
               return True;               -- Orphaned left is less
            end if;
         end if;
         declare
            Left_Key : constant Device_Key := Get_Key (Devices, Index);
         begin
            Index := Find (RF, Right);
            if Index <= 0 then            -- Orphaned right
               return False;              -- Orphaned right is less
            end if;
            declare
               Right_Key : constant Device_Key :=
                                    Get_Key (Devices, Index);
            begin
               if Left_Key.Room /= Right_Key.Room then    -- Ordered by
                  return Left_Key.Room < Right_Key.Room;  -- room number
               end if;
               declare
                  Master : constant RF_Address :=
                           Ptr (Get (Rooms, Left_Key.Room)).Master;
               begin
                  if Right = Master then
                     return True;  -- Right room master is greater
                  elsif Left = Master then
                     return False; -- Left room master is greater
                  else             -- Compare by reverse number
                     return Left_Key.Index > Right_Key.Index;
                  end if;
               end;
            end;
         end;
      end Less;

      function Find (Item : RF_Address) return Integer is
         From : Natural := 0;
         To   : Natural := Size + 1;
         This : Natural;
      begin
         if Size = 0 then
            return -1;
         end if;
         loop
            This := (From + To) / 2;
            if Item = Result (This) then
               return This;
            elsif Less (Item, Result (This)) then
               if This - From <= 1 then
                  return -This;
               end if;
               To := This;
            else
               if To - This <= 1 then
                  return - This - 1;
               end if;
               From := This;
            end if;
         end loop;
      end Find;

   begin
      for Index in Data'Range loop
         declare
            Where : Integer := Find (Data (Index));
         begin
            if Where < 0 then
               Where := -Where;
               Result (Where + 1..Size + 1) := Result (Where..Size);
               Result (Where)               := Data (Index);
               Size := Size + 1;
            end if;
         end;
      end loop;
      return Result (1..Size);
   end Sort_Unchecked;

   procedure Status_Received
             (  Client : in out ELV_MAX_Cube_Client;
                Error  : Boolean;
                Duty   : Ratio;
                Slots  : Natural
             )  is
      Text    : String (1..40);
      Pointer : Integer := 1;
   begin
      if Error then
         Put (Text, Pointer, "Rejected, ");
      end if;
      Put (Text, Pointer, Integer (Duty * 100.0));
      Put (Text, Pointer, "%, ");
      Put (Text, Pointer, Slots);
      Put (Text, Pointer, " free");
      Trace
      (  ELV_MAX_Cube_Client'Class (Client),
         Text (1..Pointer - 1)
      );
   end Status_Received;

   procedure Trace
             (  Client  : in out ELV_MAX_Cube_Client;
                Message : String
             )  is
   begin
      Trace
      (  Client.Listener.Factory.all,
         (  Get_Client_Name (Client.Listener.Factory.all, Client)
         &  ' '
         &  Message
      )  );
   end Trace;

   procedure Unlink_Unchecked
             (  Client     : in out ELV_MAX_Cube_Client;
                Kind_Of    : Partner_Device_Type;
                Address    : RF_Address;
                From       : Room_Descriptor'Class;
                S_Commands : in out Natural
             )  is
      procedure Unlink (Data : Device_Parameters) is
      begin
         Send -- Unlink forward
         (  Client,
            (  "s:"
            &  To_Base64
               (  S_Unlink (False)
               &  Encode (Address)
               &  Encode (Data.Room)
               &  Encode (Data.Address)
               &  Encode (Data.Kind_Of)
               )
            &  CRLF
         )  );
         Send -- Unlink backward
         (  Client,
            (  "s:"
            &  To_Base64
               (  S_Unlink (Data.Address = From.Master)
               &  Encode (Data.Address)
               &  Encode (Data.Room)
               &  Encode (Address)
               &  Encode (Kind_Of)
               )
            &  CRLF
         )  );
         S_Commands := S_Commands + 2;
      end Unlink;
   begin
      for Index in 1..From.Count loop -- Devices in the room
         declare
            Device : Device_Descriptor'Class renames
                     Ptr
                     (  Get (Client.Topology.Devices, (From.ID, Index))
                     ) .all;
         begin
            if Device.Data.Address /= Address then
               case Device.Kind_Of is
                  when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                     case Kind_Of is -- Anything but radiator thermostat
                        when Wall_Thermostat | Shutter_Contact | Eco_Button =>
                           Unlink (Device.Data);
                        when others =>
                           null;
                     end case;
                  when Wall_Thermostat =>
                     case Kind_Of is -- Anything but wall thermostat
                        when Radiator_Thermostat      |
                             Radiator_Thermostat_Plus |
                             Shutter_Contact          |
                             Eco_Button               =>
                           Unlink (Device.Data);
                        when others =>
                           null;
                     end case;
                  when Shutter_Contact | Eco_Button =>
                     case Kind_Of is -- Any thermostat
                        when Radiator_Thermostat..Wall_Thermostat =>
                           Unlink (Device.Data);
                        when others =>
                           null;
                     end case;
                  when others =>
                     null;
               end case;
            end if;
         end;
      end loop;
   end Unlink_Unchecked;

   procedure Wake_Up
             (  Client   : in out ELV_MAX_Cube_Client;
                Address  : RF_Address;
                Interval : Duration := 30.0
             )  is
   begin
      if Is_In (Client, Address) then
--         Send (Client, "r:D," & Image (Address) & CRLF);
--         Send (Client, "r:" & CRLF);
         Send
         (  Client,
            (  "z:"
            &  Image (Integer (Float'Ceiling (Float (Interval))))
            &  ",D,"
            &  Image (Address)
            &  CRLF
         )  );
      else
         Raise_Exception
         (  End_Error'Identity,
            "No device with the address " & Image (Address) & " exists"
         );
      end if;
   end Wake_Up;

   procedure Wake_Up
             (  Client   : in out ELV_MAX_Cube_Client;
                Room     : Room_ID;
                Interval : Duration := 30.0
             )  is
      Index : Integer;
   begin
      declare
         Lock : Topology_Holder (Client'Access);
      begin
         Index := Find (Client.Topology.Rooms, Room);
      end;
      if Index > 0 then
         Wake_Up_Unchecked (Client, Room, Interval);
      else
         Raise_Exception
         (  End_Error'Identity,
            "No room " & Image (Integer (Room)) & " exists"
         );
      end if;
   end Wake_Up;

   procedure Wake_Up
             (  Client   : in out ELV_MAX_Cube_Client;
                Interval : Duration := 30.0
             )  is
   begin
      Send
      (  Client,
         (  "z:"
         &  Image (Integer (Float'Ceiling (Float (Interval))))
         &  ",A"
         &  CRLF
      )  );
   end Wake_Up;

   procedure Wake_Up_Unchecked
             (  Client   : in out ELV_MAX_Cube_Client;
                Room     : Room_ID;
                Interval : Duration := 30.0
             )  is
   begin
      Send
      (  Client,
         (  "z:"
         &  Image (Integer (Float'Ceiling (Float (Interval))))
         &  ",G,"
         &  Image (Integer (Room))
         &  CRLF
      )  );
   end Wake_Up_Unchecked;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Devices_Configuration
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Secondary_Buffer
             )  is
   begin
      null;
   end Write;

   function "<" (Left, Right : Device_Key) return Boolean is
   begin
      return
      (  Left.Room < Right.Room
      or else
         (  Left.Room = Right.Room
         and then
            Left.Index < Right.Index
      )  );
   end "<";

end GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
