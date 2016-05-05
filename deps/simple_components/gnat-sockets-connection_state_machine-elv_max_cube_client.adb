--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ELV_MAX_Cube_Client                         Summer, 2015       --
--  Implementation                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Base64;    use Strings_Edit.Base64;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body GNAT.Sockets.Connection_State_Machine.
             ELV_MAX_Cube_Client is
   use Stream_Element_Offset_Edit;
   use Device_Handles;
   use Room_Handles;

   CRLF : constant String := Character'Val (13) & Character'Val (10);
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

   S_Head : constant String := (  Character'Val (16#00#)
                               &  Character'Val (16#04#) -- Room mode
                               &  Character'Val (16#40#) -- Command mode
                               &  Character'Val (16#00#)
                               &  Character'Val (16#00#)
                               &  Character'Val (16#00#)
                               );
   S_Add  : constant String := (  Character'Val (16#00#)
                               &  Character'Val (16#00#) -- Room mode
                               &  Character'Val (16#20#) -- Command mode
                               &  Character'Val (16#00#)
                               &  Character'Val (16#00#)
                               &  Character'Val (16#00#)
                               );
   S_Rem  : constant String := (  Character'Val (16#00#)
                               &  Character'Val (16#00#) -- Room mode
                               &  Character'Val (16#21#) -- Command mode
                               &  Character'Val (16#00#)
                               &  Character'Val (16#00#)
                               &  Character'Val (16#00#)
                               );
   function Encode (Temperature : Centigrade) return Unsigned_8;
   function Encode (Day : Week_Day) return Integer;
   function Encode (Address : RF_Address) return String;
   function Encode (Date : Time) return String;
   function Encode (Mode : Operating_Mode) return Character;

--     procedure Add_New_Device
--               (  Client  : in out ELV_MAX_Cube_Client;
--                  Index   : Positive;
--                  Device  : Partner_Device_Type;
--                  Address : RF_Address
--               )  is
--        Lock : Holder (Client.Topology.Lock'Access);
--        Room : Room_Descriptor'Class renames
--                  Ptr (Get (Client.Topology.Rooms, Index)).all;
--     begin
--        Add_New_Device_Unchecked
--        (  Client  => Client,
--           Room    => Room,
--           Device  => Device,
--           Address => Address
--        );
--     end Add_New_Device;
--
--     procedure Add_New_Device
--               (  Client  : in out ELV_MAX_Cube_Client;
--                  ID      : Room_ID;
--                  Device  : Partner_Device_Type;
--                  Address : RF_Address
--               )  is
--        Lock : Holder (Client.Topology.Lock'Access);
--        Room : Room_Descriptor'Class renames
--                  Ptr
--                  (  Get
--                     (  Client.Topology.Rooms,
--                        Find_Room_Unchecked (Client, ID)
--                  )  ) .all;
--     begin
--        Add_New_Device_Unchecked
--        (  Client  => Client,
--           Room    => Room,
--           Device  => Device,
--           Address => Address
--        );
--     end Add_New_Device;
--
--     procedure Add_New_Device_Unchecked
--               (  Client  : in out ELV_MAX_Cube_Client;
--                  Room    : Room_Descriptor'Class;
--                  Device  : Partner_Device_Type;
--                  Address : RF_Address
--               )  is
--        function From_Type (Device : Partner_Device_Type)
--           return Character is
--        begin
--           case Device is
--              when Radiator_Thermostat =>
--                 return Character'Val (1);
--              when Radiator_Thermostat_Plus =>
--                 return Character'Val (2);
--              when Wall_Thermostat =>
--                 return Character'Val (3);
--              when Shutter_Contact =>
--                 return Character'Val (4);
--           end case;
--        end From_Type;
--     begin
--        if Is_In (Client.Topology.RF, Address) then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "A device " & Image (Address) & " already exists"
--           );
--        end if;
--        Send
--        (  Client,
--           (  "s:"
--           &  To_Base64
--              (  S_Add
--              &  Encode (Room.Master)
--              &  Character'Val (Room.ID)
--              &  Encode (Address)
--              &  From_Type (Device)
--              )
--           &  CRLF
--        )  );
--     end Add_New_Device_Unchecked;

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
   procedure Get_Type
             (  Line    : String;
                Pointer : in out Integer;
                Kind_Of : out Device_Type
             );

   procedure Clean_Up
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Full   : Boolean := True
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      if Full then
         Client.Ready    := 0;
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
         when Cube_Update | Topology_Update |
              Device_Parameters_Update =>
            null;
         when NTP_Servers_List_Update =>
            for Index in 1..Get_Size (Update.NTP_Servers_List) loop
               Trace
               (  Client,
                  (  "> NTP server "
                  &  Image (Index)
                  &  "/"
                  &  Image (Get_Size (Update.NTP_Servers_List))
                  &  ": "
                  &  Get (Update.NTP_Servers_List, Index)
               )  );
            end loop;
         when Device_Discovery_Update =>
            Trace
            (  Client,
               (  "> Device found: "
               &  Image (Update.Device)
               &  " "
               &  Image (Update.Address)
               &  " S/N "
               &  Update.Serial_No
            )  );
         when End_Discovery_Update =>
            Trace (Client, "> Device discovery finished");
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
      Trace (Client, "> " & Image (Data));
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
      Count     : Natural  := 0;
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
         Send_Socket (Socket, Announce, Last, Address'Access);
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
      (  Character'Val (Address / 2**16)
      &  Character'Val ((Address / 2**8) mod 2**8)
      &  Character'Val (Address mod 2**8)
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
      Result (3) := Character'Val (Integer (Seconds / (30.0 * 60.0)));
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

   function Find_Room
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return Positive is
      Lock : Holder (Client.Topology.Self.Lock'Access);
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
      Clean_Up (Client, False);
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
         Device.Data.Boost_Time :=
            Duration (Character'Pos (Data (Pointer)) / 2**5) * 300.0;
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
      end Get_Wall_Thermostat_Data;

      Pointer : Integer := Line'First;
      Address : RF_Address;
   begin
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
            Lock  : Holder (Client.Topology.Lock'Access);
            Index : constant Integer :=
                    Find (Client.Topology.RF, Address);
         begin
            if Index <= 0 then
               return;
            end if;
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
                  when Cube | Shutter_Contact | Eco_Button | Unknown =>
                     null;
                  when Wall_Thermostat =>
                     Get_Wall_Thermostat_Data (Data, Pointer, Device);
                  when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                     Get_Thermostat_Data (Data, Pointer, Device);
               end case;
            end;
         end;
         Client.Ready := Client.Ready or Got_C;
         begin
            Configuration_Updated
            (  Client,
               (  Kind_Of => Device_Parameters_Update,
                  Device  => Kind_Of,
                  Address => Address
            )  );
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
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Get_Device_Unchecked (Client, Address);
   end Get_Device;

   function Get_Device
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive;
               Device : Positive
            )  return Positive is
      Lock : Holder (Client.Topology.Self.Lock'Access);
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
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Find (Client.Topology.Devices, (ID, Device));
   end Get_Device;

   function Get_Device_Data
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Device_Data is
      Lock   : Holder (Client.Topology.Self.Lock'Access);
      Device : Device_Descriptor'Class renames
               Ptr (Get (Client.Topology.Devices, Index)).all;
   begin
      if Device.Init then
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
      Lock   : Holder (Client.Topology.Self.Lock'Access);
      Device : Device_Descriptor'Class renames
               Ptr
               (  Get
                  (  Client.Topology.Devices,
                     Get_Device_Unchecked (Client, Address)
               )  ) .all;
   begin
      if Device.Init then
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
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Data.Name;
   end Get_Device_Name;

   function Get_Device_Name
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return String is
      Lock : Holder (Client.Topology.Self.Lock'Access);
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
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Data;
   end Get_Device_Parameters;

   function Get_Device_Parameters
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Device_Parameters is
      Lock : Holder (Client.Topology.Self.Lock'Access);
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
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Data.Room;
   end Get_Device_Room;

   function Get_Device_Room
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Room_ID is
      Lock : Holder (Client.Topology.Self.Lock'Access);
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
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Data.Address;
   end Get_Device_RF_Address;

   function Get_Device_Serial_No
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return String is
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Data.Serial_No;
   end Get_Device_Serial_No;

   function Get_Device_Serial_No
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return String is
      Lock : Holder (Client.Topology.Self.Lock'Access);
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
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr (Get (Client.Topology.Devices, Index)).Kind_Of;
   end Get_Device_Type;

   function Get_Device_Type
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Device_Type is
      Lock : Holder (Client.Topology.Self.Lock'Access);
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
      Value := Duration (Character'Pos (Line (Pointer))) * 60.0;
      if Value > Day_Duration'Last then
         Value := Day_Duration'Last;
      end if;
      Pointer := Pointer + 1;
   end Get_Duration;

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
      Length      : Integer;
      Next        : Integer;
      Address     : RF_Address;
      Answer      : Boolean;
      Error       : Boolean;
      Valid       : Boolean;
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
            (  Float (Character'Pos (Data (Pointer))) / 255.0
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
         Lock   : Holder (Client.Topology.Lock'Access);
         Device : Device_Descriptor'Class renames
                     Ptr
                     (  Get
                        (  Client.Topology.Devices,
                           Get_Device_Unchecked (Client, Address)
                     )  ) .all;
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
         Device.Last.Error       := Error;
         Device.Last.Initialized := Initialized;
         Device.Init             := True;
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
            Byte : constant Unsigned_8 := Character'Pos (Data (Pointer));
         begin
            Valid       := 0 /= (Byte and 2**4);
            Error       := 0 /= (Byte and 2**3);
            Answer      := 0 /= (Byte and 2**2);
            Initialized := 0 /= (Byte and 2**1);
         end;
         Pointer := Pointer + 1;
         declare
            Data : constant Device_Data := Get_Data;
         begin
            if Data.Kind_Of in Radiator_Thermostat..Eco_Button then
               Data_Received (Client, Data);
            end if;
         end;
         Pointer := Next;
      end loop;
      Client.Ready := Client.Ready or Got_L;
   exception
      when End_Error =>
         null; -- Unknown device
   end Get_L;

   procedure Get_M
             (  Client : in out ELV_MAX_Cube_Client'Class;
                Line   : String
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
               (  "Invalid M-response, missing device serial number "
               &  "(device "
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
         if Pointer - 1 > Data'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid M-response, wrong name length (device "
               &  Image (No)
               &  ')'
            )  );
         end if;
         if Pointer > Data'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid M-response, missing room ID (device "
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
                  (ID, Client.Roomless),
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
               (  "Invalid M-response, missing room ID (room "
               &  Image (No)
               &  ')'
            )  );
         end if;
         ID := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         if Is_In (Client.Topology.Rooms, ID) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid M-response, duplicated room ID "
               &  Image (Integer (ID))
            )  );
         end if;
         if Pointer > Data'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid M-response, missing room name length (room "
               &  Image (No)
               &  ')'
            )  );
         end if;
         Length  := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
         Name    := Pointer;
         Pointer := Pointer + Length;
         if Pointer - 1 > Data'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Invalid M-response, wrong name length (room "
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

      Pointer : Integer := Line'First;
      I, J    : Integer;
   begin
      begin
         Get (Line, Pointer, I);
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid M-response, missing first field"
            );
      end;
      Get_Comma (Line, Pointer);
      begin
         Get (Line, Pointer, J);
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid M-response, missing second field"
            );
      end;
      Get_Comma (Line, Pointer);
      declare
         Lock : Holder (Client.Topology.Lock'Access);
      begin
         declare
            Data    : constant String :=
                      From_Base64 (Line (Pointer..Line'Last));
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
                  "Invalid M-response, malformed data header"
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
         end;
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
      end;
      Client.Ready := Client.Ready or Got_M;
      begin
         Configuration_Updated (Client, (Kind_Of => Topology_Update));
      exception
         when others =>
            null;
      end;
   end Get_M;

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

   function Get_Number_Of_Devices (Client : ELV_MAX_Cube_Client)
      return Natural is
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Get_Size (Client.Topology.Devices);
   end Get_Number_Of_Devices;

   function Get_Number_Of_Devices
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Natural is
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr (Get (Client.Topology.Rooms, Index)).Count;
   end Get_Number_Of_Devices;

   function Get_Number_Of_Devices
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return Natural is
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr
             (  Get
                (  Client.Topology.Rooms,
                   Find_Room_Unchecked (Client, ID)
             )  ).Count;
   end Get_Number_Of_Devices;

   function Get_Number_Of_Rooms (Client : ELV_MAX_Cube_Client)
      return Natural is
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Get_Size (Client.Topology.Rooms);
   end Get_Number_Of_Rooms;

   function Get_RF_Address (Client : ELV_MAX_Cube_Client)
      return RF_Address is
   begin
      if 0 /= (Client.Ready and Got_H) then
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
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr (Get (Client.Topology.Rooms, Index)).ID;
   end Get_Room_ID;

   function Get_Room_Name
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return String is
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr (Get (Client.Topology.Rooms, Index)).Name;
   end Get_Room_Name;

   function Get_Room_Name
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return String is
      Lock : Holder (Client.Topology.Self.Lock'Access);
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
      Lock : Holder (Client.Topology.Self.Lock'Access);
   begin
      return Ptr (Get (Client.Topology.Rooms, Index)).Master;
   end Get_Room_RF_Address;

   function Get_Room_RF_Address
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return RF_Address is
      Lock : Holder (Client.Topology.Self.Lock'Access);
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
         elsif Value >= 100 then
            Duty := Ratio'Last;
         else
            Duty := Ratio (Float (Value) / 100.0);
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

   function Has_Device_Data
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Boolean is
      Lock   : Holder (Client.Topology.Self.Lock'Access);
      Device : Device_Descriptor'Class renames
               Ptr (Get (Client.Topology.Devices, Index)).all;
   begin
      return Device.Init;
   end Has_Device_Data;

   function Has_Device_Data
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Boolean is
      Lock   : Holder (Client.Topology.Self.Lock'Access);
      Device : Device_Descriptor'Class renames
               Ptr
               (  Get
                  (  Client.Topology.Devices,
                     Get_Device_Unchecked (Client, Address)
               )  ) .all;
   begin
      return Device.Init;
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
            when Mo => return "Moday";
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

   procedure Initialize (Client : in out ELV_MAX_Cube_Client) is
   begin
      Client.LF.Value (1) := Stream_Element'Val (10);
   end Initialize;

   function Is_In
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive;
               Device : Positive
            )  return Boolean is
      Lock : Holder (Client.Topology.Self.Lock'Access);
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
      Lock  : Holder (Client.Topology.Self.Lock'Access);
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
            (  Client,
               "Unsupported message '" & Line (Line'First) & '''
            );
      end case;
   end Process_Packet;

   procedure Query_Devices (Client : in out ELV_MAX_Cube_Client) is
   begin
      Send (Client, "l:" & CRLF);
   end Query_Devices;

   procedure Query_NTP_Servers (Client : in out ELV_MAX_Cube_Client) is
   begin
      Send (Client, "f:" & CRLF);
   end Query_NTP_Servers;

   procedure Reset_Devices (Client : in out ELV_MAX_Cube_Client) is
   begin
      Send (Client, "a:" & CRLF);
   end Reset_Devices;

   procedure Reset_Error (Client : in out ELV_MAX_Cube_Client) is
   begin
      Send (Client, "r:" & CRLF);
   end Reset_Error;

   procedure Send
             (  Client : in out ELV_MAX_Cube_Client;
                Data   : String
             )  is
      Pointer : Integer := Data'First;
   begin
      if Available_To_Send (Client) < Data'Length then
         Raise_Exception
         (  Use_Error'Identity,
            "Cube is busy (cannot send data right now)"
         );
      else
         Send (Client, Data, Pointer);
      end if;
   end Send;

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

   procedure Set_Thermostat_Automatic
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
            &  Character'Val (0)
            )
         &  CRLF
      )  );
   end Set_Thermostat_Automatic;

   procedure Set_Thermostat_Automatic
             (  Client : in out ELV_MAX_Cube_Client;
                Index  : Positive
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Automatic
      (  Client,
         Ptr (Get (Client.Topology.Devices, Index)).all
     );
   end Set_Thermostat_Automatic;

   procedure Set_Thermostat_Automatic
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Automatic
      (  Client,
         Ptr
         (  Get
            (  Client.Topology.Devices,
               Get_Device_Unchecked (Client, Address)
         )  ) .all
      );
   end Set_Thermostat_Automatic;

   procedure Set_Thermostat_Boost
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
   end Set_Thermostat_Boost;

   procedure Set_Thermostat_Boost
             (  Client : in out ELV_MAX_Cube_Client;
                Index  : Positive
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Boost
      (  Client,
         Ptr (Get (Client.Topology.Devices, Index)).all
      );
   end Set_Thermostat_Boost;

   procedure Set_Thermostat_Boost
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Boost
      (  Client,
         Ptr
         (  Get
            (  Client.Topology.Devices,
               Get_Device_Unchecked (Client, Address)
         )  ) .all
      );
   end Set_Thermostat_Boost;

   procedure Set_Thermostat_Parameters
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : in out Device_Descriptor'Class;
                Comfort     : Centigrade;
                Eco         : Centigrade;
                Max         : Centigrade;
                Min         : Centigrade;
                Offset      : Centigrade;
                Window_Open : Centigrade;
                Window_Time : Day_Duration
             )  is
      Minutes : constant Integer := Integer (Window_Time / 60.0);
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
            &  Character'Val (Minutes)
            )
         &  CRLF
      )  );
      Device.Data.Comfort     := Comfort;
      Device.Data.Eco         := Eco;
      Device.Data.Min         := Min;
      Device.Data.Max         := Max;
      Device.Data.Offset      := Offset;
      Device.Data.Window_Open := Window_Open;
      Device.Data.Window_Time := Window_Time;
   end Set_Thermostat_Parameters;

   procedure Set_Thermostat_Parameters
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Comfort     : Centigrade;
                Eco         : Centigrade;
                Max         : Centigrade;
                Min         : Centigrade;
                Offset      : Centigrade;
                Window_Open : Centigrade;
                Window_Time : Day_Duration
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Parameters
      (  Client      => Client,
         Device      => Ptr (Get (Client.Topology.Devices, Index)).all,
         Comfort     => Comfort,
         Eco         => Eco,
         Max         => Max,
         Min         => Min,
         Offset      => Offset,
         Window_Open => Window_Open,
         Window_Time => Window_Time
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
                Window_Time : Day_Duration
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Parameters
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
         Window_Time => Window_Time
      );
   end Set_Thermostat_Parameters;

   procedure Set_Thermostat_Parameters
             (  Client  : in out ELV_MAX_Cube_Client;
                Device  : in out Device_Descriptor'Class;
                Comfort : Centigrade;
                Eco     : Centigrade;
                Max     : Centigrade;
                Min     : Centigrade
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
            &  Character'Val (0)
            &  Character'Val (0)
            &  Character'Val (0)
            )
         &  CRLF
      )  );
      Device.Data.Comfort := Comfort;
      Device.Data.Eco     := Eco;
      Device.Data.Min     := Min;
      Device.Data.Max     := Max;
   end Set_Thermostat_Parameters;

   procedure Set_Thermostat_Parameters
             (  Client  : in out ELV_MAX_Cube_Client;
                Index   : Positive;
                Comfort : Centigrade;
                Eco     : Centigrade;
                Max     : Centigrade;
                Min     : Centigrade
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Parameters
      (  Client  => Client,
         Device  => Ptr (Get (Client.Topology.Devices, Index)).all,
         Comfort => Comfort,
         Eco     => Eco,
         Max     => Max,
         Min     => Min
      );
   end Set_Thermostat_Parameters;

   procedure Set_Thermostat_Parameters
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address;
                Comfort : Centigrade;
                Eco     : Centigrade;
                Max     : Centigrade;
                Min     : Centigrade
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Parameters
      (  Client  => Client,
         Device  => Ptr
                    (  Get
                       (  Client.Topology.Devices,
                          Get_Device_Unchecked (Client, Address)
                    )  ) .all,
         Comfort => Comfort,
         Eco     => Eco,
         Max     => Max,
         Min     => Min
      );
   end Set_Thermostat_Parameters;

   procedure Set_Thermostat_Schedule
             (  Client   : in out ELV_MAX_Cube_Client;
                Device   : in out Device_Descriptor'Class;
                Day      : Week_Day;
                Schedule : Points_List
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
      Device.Data.Schedule (Day) := (Schedule'Length, Schedule);
   end Set_Thermostat_Schedule;

   procedure Set_Thermostat_Schedule
             (  Client   : in out ELV_MAX_Cube_Client;
                Index    : Positive;
                Day      : Week_Day;
                Schedule : Points_List
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Schedule
      (  Client,
         Ptr (Get (Client.Topology.Devices, Index)).all,
         Day,
         Schedule
      );
   end Set_Thermostat_Schedule;

   procedure Set_Thermostat_Schedule
             (  Client   : in out ELV_MAX_Cube_Client;
                Address  : RF_Address;
                Day      : Week_Day;
                Schedule : Points_List
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Schedule
      (  Client,
         Ptr
         (  Get
            (  Client.Topology.Devices,
               Get_Device_Unchecked (Client, Address)
         )  ) .all,
         Day,
         Schedule
      );
   end Set_Thermostat_Schedule;

   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : in out Device_Descriptor'Class;
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
      Device.Last.New_Temperature := Temperature;
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
   end Set_Thermostat_Temperature;

   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Temperature : Centigrade
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Temperature
      (  Client,
         Ptr (Get (Client.Topology.Devices, Index)).all,
         Temperature
      );
   end Set_Thermostat_Temperature;

   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Temperature : Centigrade
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Temperature
      (  Client,
         Ptr
         (  Get
            (  Client.Topology.Devices,
               Get_Device_Unchecked (Client, Address)
         )  ) .all,
         Temperature
      );
   end Set_Thermostat_Temperature;

   procedure Set_Thermostat_Temperature
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
   end Set_Thermostat_Temperature;

   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Temperature : Centigrade;
                Up_Until    : Time
             )  is
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Temperature
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
      Lock : Holder (Client.Topology.Lock'Access);
   begin
      Set_Thermostat_Temperature
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
      Trace (Client, "> " & Text (1..Pointer - 1));
   end Status_Received;

   procedure Trace
             (  Client  : in out ELV_MAX_Cube_Client;
                Message : String
             )  is
   begin
      Trace
      (  Client.Listener.Factory.all,
         Image (Get_Client_Address (Client)) & ' ' & Message
      );
   end Trace;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Devices_Configuration
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
