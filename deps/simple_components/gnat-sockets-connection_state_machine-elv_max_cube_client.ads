--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ELV_MAX_Cube_Client                         Summer, 2015       --
--  Interface                                                         --
--                                Last revision :  11:02 11 Apr 2021  --
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

with Ada.Calendar;    use Ada.Calendar;
with Ada.Exceptions;  use Ada.Exceptions;

with GNAT.Sockets.Connection_State_Machine.Expected_Sequence;
with GNAT.Sockets.Connection_State_Machine.Terminated_Strings;

with Generic_Indefinite_FIFO;
with Generic_Indefinite_Set;
with Generic_Map;
with Object.Handle;
with Synchronization.Mutexes;

package GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client is
--
-- ELV_MAX_Cube_Port -- The TCP/IP port used
--
   ELV_MAX_Cube_Port : constant Port_Type := 62910;

   type Room_ID is range 0..2**8-1;
   No_Room : constant Room_ID;
--
-- Ratio -- Valve positoin is 0 - closed, 1 - open
--
   type Ratio is delta 0.001 range 0.0..1.0;
--
-- Centigrade -- Set temperature in Celsius degree
--
   type Centigrade is delta 0.1 digits 4 range -4.0..135.0; -- Celsius
   subtype Set_Centigrade is Centigrade range 0.0..63.5;
   function Image (Value : Centigrade) return String;
--
-- RF_Address -- End device radio frequency address
--
   type RF_Address is range 0..2**24-1;
   function Image (Value : RF_Address) return String;
   type RF_Address_Array is array (Positive range <>) of RF_Address;
--
-- Device_Type -- The types of devices
--
   type Device_Type is
        (  Cube,
           Radiator_Thermostat,
           Radiator_Thermostat_Plus,
           Wall_Thermostat,
           Shutter_Contact,
           Eco_Button,
           Unknown
        );
   subtype Partner_Device_Type is Device_Type
      range Radiator_Thermostat..Eco_Button;
   subtype Room_Device_Type is Device_Type
      range Radiator_Thermostat..Shutter_Contact;

   function Image (Kind_Of : Device_Type) return String;

   type Week_Day is (Mo, Tu, We, Th, Fr, Sa, Su);
   function Image (Day : Week_Day; Short : Boolean := True)
      return String;

   type Week_Time is record
      Day  : Week_Day;
      Time : Day_Duration;
   end record;
   function Minutes (Time : Day_Duration) return String;
   function Image (Time : Week_Time; Short : Boolean := True)
      return String;
--
-- Set_Point -- A point of the temperature profile
--
   type Set_Point is record
      Last  : Day_Duration; -- The last time when the temperature must
      Point : Centigrade;   -- be upheld
   end record;

   subtype Point_Count is Integer range 0..13;
   subtype Point_Number is Point_Count range 1..Point_Count'Last;
   type Points_List is array (Point_Number range <>) of Set_Point;
   type Day_Schedule (Length : Point_Count := 0) is record
      Points : Points_List (1..Length);
   end record;
   type Week_Schedule is array (Week_Day) of Day_Schedule;
--
-- Device_Data -- Status and data of a device
--
   type Operating_Mode is (Automatic, Manual, Vacation, Boost);
   type Device_Data (Kind_Of : Device_Type) is record
      Address       : RF_Address := 0;
      Error         : Boolean    := False;
      Initialized   : Boolean    := False;
      Battery_Low   : Boolean    := False;
      Link_Error    : Boolean    := False;
      Panel_Locked  : Boolean    := False;
      Gateway_Known : Boolean    := False;
      DST           : Boolean    := False; -- Daylight saving time
      Mode          : Operating_Mode := Automatic;
      case Kind_Of is
         when Cube | Eco_Button | Unknown =>
            null;
         when Shutter_Contact =>
            Open : Boolean := False;
         when Radiator_Thermostat..Wall_Thermostat =>
            Set_Temperature : Centigrade := 18.0;
            New_Temperature : Centigrade := 18.0;
            Temperature     : Centigrade := Centigrade'First;
            case Kind_Of is
               when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                  Valve_Position     : Ratio := 0.0;
                  Latest_Temperature : Centigrade := Centigrade'First;
                  Received_At        : Time;
               when others =>
                  null;
            end case;
      end case;
   end record;
   function Image (Data : Device_Data) return String;
--
-- Device_Parameters -- Settings of a device
--
   type Device_Parameters
        (  Kind_Of     : Device_Type;
           Name_Length : Natural
        )  is
   record
      Room      : Room_ID;
      Address   : RF_Address;
      Serial_No : String (1..10);
      Name      : String (1..Name_Length);
      case Kind_Of is
         when Cube | Shutter_Contact | Eco_Button | Unknown =>
            null;
         when Radiator_Thermostat..Wall_Thermostat =>
            Comfort     : Centigrade := 18.0;
            Eco         : Centigrade := 18.0;
            Max         : Centigrade := 18.0;
            Min         : Centigrade := 18.0;
            Offset      : Centigrade := 0.0;
            Window_Open : Centigrade := 18.0;
            Schedule    : Week_Schedule :=
                          (others => (0, (others => (0.0, 19.0))));
            case Kind_Of is
               when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                  Window_Time     : Duration  := 10.0;
                  Boost_Time      : Duration  := 1.0;
                  Boost_Valve     : Ratio     := 0.5;
                  Decalcification : Week_Time := (Su, 12.0 * 3_600.0);
                  Max_Valve       : Ratio     := 0.8;
                  Valve_Offset    : Ratio     := 0.0;
               when others =>
                  null;
            end case;
      end case;
   end record;
--
-- Setting_Mode -- S-command execution mode
--
   type Setting_Mode is mod 4;
   S_Command  : constant Setting_Mode := 1;
   S_Response : constant Setting_Mode := 2;
--
-- Display_Mode -- The wall thermostat display mode
--
--   Display_Set_Temperature - Indicate the set temperature
--   Display_Is_Temperature  - Indicate the measured temperature
--
   type Display_Mode is
        (  Display_Set_Temperature,
           Display_Is_Temperature
        );
--
-- ELV_MAX_Cube_Client -- An object implementing ELV MAX! Cube client
--
--    Listener       - The connections server object
--    Line_Length    - The maximal response line length
--    Input_Size     - The size of the input buffer
--    Output_Size    - The size of the output buffer
--    Secondary_Size - The size of the secondary buffer
--
-- The  Output_Size can  be limited to 140 elements or so,  if  only one
-- request will be sent at a time. The Secondary_Size is the buffer used
-- to to queue more than one requests. It should be set correspondingly.
--
   type ELV_MAX_Cube_Client
        (  Listener       : access Connections_Server'Class;
           Line_Length    : Positive;
           Input_Size     : Buffer_Length;
           Output_Size    : Buffer_Length;
           Secondary_Size : Storage_Count
        )  is new State_Machine with private;
--
-- Add_New_Device -- Add new device
--
--    Client                   - The connection object
--  [ Index / ID / Room_Name ] - The room number 1..Get_Number_Of_Rooms
--  [ Kind_Of  ]               - The type of the new device to add
--    Serial_No                - The device serial number
--    Device_Name              - The device name
--    Address                  - The RF address of the device
--    ID                       - The new room ID
--  [ S_Commands ]             - The number of commands issued
--    Mode                     - Execution mode
--
-- The device is added to the specified room,  if the room is specified.
-- Otherwise the device  is added outside any room. This must be an eco-
-- button.  When  the room  is specified  using  a name,  a new room  is
-- created  with this  name.  The  parameter  Mode  specifies  execution
-- method. When S_Command is specified the device is linked to the cube.
-- When S_Response  is specified the device is added in the local cache.
-- The default is both sending to the cube and updating the local cache.
-- If there is a danger that  the cube may reject the command it must be
-- called first as S_Command and second as S_Response after confirmation
-- to update the cache. For a newly created room S_Command set ID to the
-- new room ID. It must be passed to the S_Respose call.
--
-- Exceptions :
--
--    Constraint_Error - Wrong room number, device type, illegal name
--    End_Error        - There is no such room
--    Name_Error       - A device with this address already exists
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
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
             );
   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Room_Name   : String;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                ID          : in out Room_ID;
                Mode        : Setting_Mode := S_Command or S_Response
             );
   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                S_Commands  : out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             );
   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                Mode        : Setting_Mode := S_Command or S_Response
             );
   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                ID          : Room_ID;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                S_Commands  : out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             );
   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                ID          : Room_ID;
                Kind_Of     : Room_Device_Type;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                Mode        : Setting_Mode := S_Command or S_Response
             );
   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                S_Commands  : out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             );
   procedure Add_New_Device
             (  Client      : in out ELV_MAX_Cube_Client;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                Mode        : Setting_Mode := S_Command or S_Response
             );
--
-- Attach_Device -- Add a detached device to another room
--
--    Client            - The connection object
--    Address           - The device RF-address
--    Index / ID / Name - The room index, ID or name
--  [ S_Commands ]      - The number of issued S-commands
--    Mode              - Execution mode
--
-- This  procedure  attaches previously detached device to another room.
-- It is two separate operations because the cube cannot move  a  device
-- in one single step. When room is specified by Name it can  be  a  new
-- room to create or an existing  room.  The  parameter  Mode  specifies
-- execution  method.  When  S_Command is specified the device is moved.
-- When S_Response is specified the topology is updated. The default  is
-- both  sending to the cube and updating. If there is a danger that the
-- cube may reject the command it must be called first as S_Command  and
-- second  as  S_Response  after  confirmation  to update the cache. The
-- parameter S_Commands returns the number of issued s-commands.
--
-- Exceptions :
--
--    Constraint_Error - Invalid room number
--    End_Error        - No such room ID or detached device
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Attach_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Device     : RF_Address;
                Index      : Positive;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             );
   procedure Attach_Device
             (  Client : in out ELV_MAX_Cube_Client;
                Device : RF_Address;
                Index  : Positive;
                Mode   : Setting_Mode := S_Command or S_Response
             );
   procedure Attach_Device
             (  Client : in out ELV_MAX_Cube_Client;
                Device     : RF_Address;
                ID         : Room_ID;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             );
   procedure Attach_Device
             (  Client : in out ELV_MAX_Cube_Client;
                Device : RF_Address;
                ID     : Room_ID;
                Mode   : Setting_Mode := S_Command or S_Response
             );
   procedure Attach_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Device     : RF_Address;
                Name       : String;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             );
   procedure Attach_Device
             (  Client : in out ELV_MAX_Cube_Client;
                Device : RF_Address;
                Name   : String;
                Mode   : Setting_Mode := S_Command or S_Response
             );
--
-- Cancel_Pairing -- Leave pairing mode
--
--    Client  - The connection object
--
-- The command terminates pairing.
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Cancel_Pairing
             (  Client : in out ELV_MAX_Cube_Client
             );
--
-- Servers_List -- List of names
--
   package Servers_List is new Generic_Indefinite_Set (String);
--
-- Configuration_Updated -- Configuration update notification
--
--    Client - The connection object
--    Update - The update data
--
-- The default implementation does nothing
--
   type Update_Type is
        (  Cube_Update,
           Topology_Update,
           Detached_Device_Update,
           Device_Parameters_Update,
           Device_Discovery_Update,
           End_Discovery_Update,
           NTP_Servers_List_Update
        );
   type Update_Data (Kind_Of : Update_Type) is record
      case Kind_Of is
         when Cube_Update          |
              Topology_Update      |
              End_Discovery_Update =>
            null;
         when Detached_Device_Update   |
              Device_Parameters_Update |
              Device_Discovery_Update  =>
            Device  : Device_Type; -- The device type
            Address : RF_Address;  -- Of the device updated
            case Kind_Of is
               when Device_Discovery_Update | Detached_Device_Update =>
                  Serial_No : String (1..10);
               when others =>
                  null;
            end case;
         when NTP_Servers_List_Update =>
            NTP_Servers_List : Servers_List.Set;
      end case;
   end record;
   procedure Configuration_Updated
             (  Client : in out ELV_MAX_Cube_Client;
                Update : Update_Data
             );
--
-- Data_Received -- Data reception callback
--
--    Client - The connection object
--    Data   - The device data
--
-- This procedure  is called when data  from a device are received.  The
-- cube  polls  the devices  and their  data  back  to the  client  upon
-- handshake or when data are requested (see Query_Devices). The default
-- implementation  calls to Trace. It likely  should be overridden, e.g.
-- to store the data.
--
   procedure Data_Received
             (  Client : in out ELV_MAX_Cube_Client;
                Data   : Device_Data
             );
--
-- Delete -- Devices and/or rooms
--
--    Client       - The connection object
--    List         - The list of devices and/or rooms to remove
--  [ S_Commands ] - The number of issued S-commands
--    Mode         - Execution mode
--
-- This procedure  deletes devices and rooms that lose all devices.  The
-- parameter   Mode  specifies  execution  method.   When  S_Command  is
-- specified  the devices are deleted from the cube.  When S_Response is
-- specified the  device and the rooms  are deleted in  the local cache.
-- The default is both sending to the cube and updating the local cache.
-- If there is a danger that  the cube may reject the command it must be
-- called first as S_Command and second as S_Response after confirmation
-- to update the cache.
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Delete
             (  Client     : in out ELV_MAX_Cube_Client;
                List       : RF_Address_Array;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             );
   procedure Delete
             (  Client : in out ELV_MAX_Cube_Client;
                List   : RF_Address_Array;
                Mode   : Setting_Mode := S_Command or S_Response
             );
--
-- Delete_Device -- By its RF address
--
--    Client       - The connection object
--    Address      - The device RF address
--    Name         - The new name
--  [ S_Commands ] - The number of issued S-commands
--    Mode         - Execution mode
--
-- This procedure deletes a device  and the room it contained  if it was
-- the single device in that room.
--
-- Exceptions :
--
--    End_Error    - No such device
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Delete_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Address    : RF_Address;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             );
   procedure Delete_Device
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address;
                Mode    : Setting_Mode := S_Command or S_Response
             );
--
-- Delete_Room -- By its ID or index
--
--    Client       - The connection object
--    Index / ID   - The room number 1..Get_Number_Of_Rooms
--    Name         - The new name
--  [ S_Commands ] - The number of issued S-commands
--    Mode         - Execution mode
--
-- These procedures delete a room and all devices in it.
--
-- Exceptions :
--
--    End_Error    - No such room
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Delete_Room
             (  Client     : in out ELV_MAX_Cube_Client;
                Index      : Positive;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             );
   procedure Delete_Room
             (  Client : in out ELV_MAX_Cube_Client;
                Index  : Positive;
                Mode   : Setting_Mode := S_Command or S_Response
             );
   procedure Delete_Room
             (  Client     : in out ELV_MAX_Cube_Client;
                ID         : Room_ID;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             );
   procedure Delete_Room
             (  Client : in out ELV_MAX_Cube_Client;
                ID     : Room_ID;
                Mode   : Setting_Mode := S_Command or S_Response
             );
--
-- Detach_Device -- Remove device from its room
--
--    Client       - The connection object
--    Address      - The device RF-address
--  [ S_Commands ] - The number of issued S-commands
--    Mode         - Execution mode
--
-- A detached device must be then attached to another room.  It  is  two
-- separate operations because the cube cannot  move  a  device  in  one
-- single step. The parameter  Mode  specifies  execution  method.  When
-- S_Command  is specified the device is unlinked from the room devices.
-- When S_Response is specified the topology is updated.  The default is
-- both  sending to the cube and updating the local cache. If there is a
-- danger  that  the cube may reject the command it must be called first
-- as  S_Command and second as S_Response after confirmation.
--
-- Exceptions :
--
--    End_Error    - No such device
--    Socket_Error - I/O error
--    Status_Error - The device has no room
--    Use_Error    - The device is busy
--
   procedure Detach_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Device     : RF_Address;
                S_Commands : out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             );
   procedure Detach_Device
             (  Client     : in out ELV_MAX_Cube_Client;
                Device     : RF_Address;
                Mode       : Setting_Mode := S_Command or S_Response
             );
--
-- Disconnected -- Connection loss notification
--
--    Client - The connection object
--
-- When overridden this procedure  must always be  called   from the new
-- implementation.
--
   procedure Disconnected (Client : in out ELV_MAX_Cube_Client);
--
-- Faulty_Device_Received -- Unconfigured or faulty device notification
--
--    Client      - The connection object
--    Address     - The device RF-Address
--    Length      - The data length
--    Error       - The device error
--    Initialized - The device initialization
--    Orphaned    - The device is not configured
--
-- This procedure is called to notify about faulty devices.  The default
-- implementation does tracing.
--
   procedure Faulty_Device_Received
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Length      : Natural;
                Error       : Boolean;
                Initialized : Boolean;
                Orphaned    : Boolean
             );
--
-- Finalize -- Object desstruction
--
--    Client - The connection object
--
-- When overridden this procedure  must always be  called   from the new
-- implementation.
--
   procedure Finalize (Client : in out ELV_MAX_Cube_Client);
--
-- Find_Room -- Find a room by its ID
--
--    Client - The connection object
--    ID     - The room ID
--
-- Returns :
--
--    The room number 1..Get_Number_Of_Rooms
--
-- Exceptions :
--
--    End_Error - No room found
--
   function Find_Room
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return Positive;
--
-- Get_Clock_Difference -- Clock adjustment
--
--    Client - The connection object
--
-- Returns :
--
--    Cube clock - host clock
--
-- Exceptions :
--
--    Status_Error -- No handshake yet
--
   function Get_Clock_Difference (Client : ELV_MAX_Cube_Client)
      return Duration;
--
-- Get_Device -- By its radio frequency address
--
--    Client  - The connection object
--    Address - The address
--
-- Returns :
--
--    The device number 1..Get_Number_Of_Devices
--
-- Exceptions :
--
--    End_Error - No such device
--
   function Get_Device
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Positive;
--
-- Get_Device -- In a room by its number
--
--    Client     - The connection object
--    Index / ID - The room number 1..Get_Number_Of_Rooms
--    Device     - The device number 1..Get_Number_Of_Devices (Room)
--
-- This  function can  be used  to enumerate devices  in the  same room.
-- Device is the device number limited to the specified room. The number
-- of devices  in the room  is returned  by Get_Number_Of_Devices.  Note
-- that some devices may have no specific room attached.
--
-- Returns :
--
--    The device number 1..Get_Number_Of_Devices (Room)
--
-- Exceptions :
--
--    Constraint_Error - Wrong room or device number
--    End_Error        - No such room
--
   function Get_Device
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive;
               Device : Positive
            )  return Positive;
   function Get_Device
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID;
               Device : Positive
            )  return Positive;
--
-- Get_Device_Data -- The device data
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--
-- Returns :
--
--    The device data last returned from the device
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--    Status_Error     - No device data yet
--
   function Get_Device_Data
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Device_Data;
   function Get_Device_Data
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Device_Data;
--
-- Get_Device_Name -- The device name
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--
-- Returns :
--
--    The device name
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--
   function Get_Device_Name
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return String;
   function Get_Device_Name
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return String;
--
-- Get_Device_Room -- The device room
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--
-- Note that some devices may have no room assigned.  For those  No_Room
-- is returned.
--
-- Returns :
--
--    The room ID
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--
   function Get_Device_Room
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Room_ID;
   function Get_Device_Room
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Room_ID;
--
-- Get_Device_Parameters -- The device data
--
--    Client         - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--
-- Returns :
--
--    The device parameters
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--
   function Get_Device_Parameters
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Device_Parameters;
   function Get_Device_Parameters
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Device_Parameters;
--
-- Get_Device_RF_Address -- The device radio frequency address
--
--    Client - The connection object
--    Index  - The device index 1..Get_Number_Of_Devices
--
-- Returns :
--
--    The device radio frequency address
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--
   function Get_Device_RF_Address
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return RF_Address;
--
-- Get_Device_Serial_No -- The device serial number
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--
-- Returns :
--
--    The serial number
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--
   function Get_Device_Serial_No
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return String;
   function Get_Device_Serial_No
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return String;
--
-- Get_Device_Type -- The device type
--
--    Client         - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--
-- Returns :
--
--    The device type
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--
   function Get_Device_Type
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Device_Type;
   function Get_Device_Type
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Device_Type;
--
-- Get_Duty -- The device duty cycle
--
--    Client - The connection object
--
-- Returns :
--
--    The current duty cycle value
--
   function Get_Duty (Client : ELV_MAX_Cube_Client) return Ratio;
--
-- Get_Error -- The cube's last error response
--
--    Client - The connection object
--
-- Returns :
--
--    True if the last 's' command was rejected by the cube
--
   function Get_Error (Client : ELV_MAX_Cube_Client) return Boolean;
--
-- Get_Metadata -- The data describing the topology of devices
--
--    Client - The connection object
--
-- Returns :
--
--    The metadata in the format used by the cube
--
   function Get_Metadata (Client : ELV_MAX_Cube_Client) return String;
--
-- Get_Number_Of_Devices -- Number of devices
--
--    Client       - The connection object
--  [ Index / ID ] - The room 1..Get_Number_Of_Rooms
--
-- When Room  is given the result  is the number of devices in the room.
-- Otherwise, it is the total number of devices.
--
-- Returns :
--
--    The number of devices
--
-- Exceptions :
--
--    Constraint_Error - Wrong room number
--    End_Error        - No such room
--
   function Get_Number_Of_Devices
            (  Client : ELV_MAX_Cube_Client
            )  return Natural;
   function Get_Number_Of_Devices
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Natural;
   function Get_Number_Of_Devices
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return Natural;
--
-- Get_Number_Of_Rooms -- Number of rooms
--
--    Client - The connection object
--
-- Returns :
--
--    The number of rooms
--
   function Get_Number_Of_Rooms (Client : ELV_MAX_Cube_Client)
      return Natural;
--
-- Get_RF_Address -- The cube radio frequency address
--
--    Client    - The connection object
--    Unchecked - If True the address can be 0 or wrong
--
-- The cube announces its frequency once connected to. When Unckecked is
-- True the last know address is returned. It can be 0 if unknown.
--
-- Returns :
--
--    The radio frequency address
--
-- Exceptions :
--
--    Status_Error -- No handshake yet
--
   function Get_RF_Address
            (  Client    : ELV_MAX_Cube_Client;
               Unchecked : Boolean := False
            )  return RF_Address;
--
-- Get_Room_ID -- Room ID
--
--    Client - The connection object
--    Index  - The room number 1..Get_Number_Of_Rooms
--
-- Returns :
--
--    The name of
--
-- Exceptions :
--
--    Constraint_Error - Wrong room number
--
   function Get_Room_ID
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Room_ID;
--
-- Get_Room_Name -- Room name
--
--    Client     - The connection object
--    Index / ID - The room number 1..Get_Number_Of_Rooms
--
-- Returns :
--
--    The name of
--
-- Exceptions :
--
--    Constraint_Error - Wrong room number
--
   function Get_Room_Name
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return String;
   function Get_Room_Name
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return String;
--
-- Get_Room_RF_Address -- Room RF address
--
--    Client     - The connection object
--    Index / ID - The room number 1..Get_Number_Of_Rooms
--
-- Returns :
--
--    The radio frequency address
--
-- Exceptions :
--
--    Constraint_Error - Wrong room number
--
   function Get_Room_RF_Address
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return RF_Address;
   function Get_Room_RF_Address
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return RF_Address;
--
-- Get_Serial_No -- The cube serial number
--
--    Client - The connection object
--
-- The cube announces its serial number once connected to
--
-- Returns :
--
--    The serial number
--
-- Exceptions :
--
--    Status_Error -- No handshake yet
--
   function Get_Serial_No (Client : ELV_MAX_Cube_Client) return String;
--
-- Get_Version -- The cube version
--
--    Client - The connection object
--
-- The cube announces its version once connected to
--
-- Returns :
--
--    The version
--
-- Exceptions :
--
--    Status_Error -- No handshake yet
--
   function Get_Version (Client : ELV_MAX_Cube_Client) return String;
--
-- Handshake_Received -- Notification of handshake completion
--
--    Client - The connection object
--
-- This  procedure  is  called  when  connection  to the cube  has  been
-- established  after receving and processinf  the handshake packet from
-- the cube. The default implementation does nothing.
--
   procedure Handshake_Received (Client : in out ELV_MAX_Cube_Client);
--
-- Has_Device_Configuration -- The device configuration
--
--    Client         - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--
-- The device announced  during  handshake  has  no configuration  known
-- until  the cube  sends it.  This function  is used  to check  if that
-- happened.
--
-- Returns :
--
--    True if the device and its configuration are known
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--
   function Has_Device_Configuration
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Boolean;
   function Has_Device_Configuration
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Boolean;
--
-- Has_Device_Data -- The device data
--
--    Client         - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--
-- Returns :
--
--    True if the device data available
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--
   function Has_Device_Data
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive
            )  return Boolean;
   function Has_Device_Data
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Boolean;
--
-- Initialize -- Object construction
--
--    Client - The connection object
--
-- When overridden this procedure  must always be  called   from the new
-- implementation.
--
   procedure Initialize (Client : in out ELV_MAX_Cube_Client);
--
-- Is_Configured -- Check if all devices of the cube have settings
--
--    Client - The connection object
--
-- Returns :
--
--    True if all devices have defined settings
--
   function Is_Configured (Client : ELV_MAX_Cube_Client) return Boolean;
--
-- Is_In -- Check if the device is attached to the cube
--
--    Client - The connection object
--    Device - The device address
--
-- Returns :
--
--    True if Device is attached to the device
--
   function Is_In
            (  Client : ELV_MAX_Cube_Client;
               Device : RF_Address
            )  return Boolean;
--
-- Is_In -- Check if the device is attached to the room
--
--    Client     - The connection object
--    Index / ID - The room number 1..Get_Number_Of_Rooms
--    Device     - The device number 1..Get_Number_Of_Devices
--
-- When Room or Device does not specify an existing entity the result is
-- False.
--
-- Returns :
--
--    True if Device is in the room
--
   function Is_In
            (  Client : ELV_MAX_Cube_Client;
               Index  : Positive;
               Device : Positive
            )  return Boolean;
   function Is_In
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID;
               Device : Positive
            )  return Boolean;
--
-- Pair -- Search for a new device to pair with
--
--    Client  - The connection object
--    Timeout - The time to search for
--
-- The command makes  the cube to search for new devices  to pair  with.
-- The parameter Timeout specifies how long the cube should do it.  When
-- a device  is found  Configuration_Updated is called.  Note  that  the
-- device to be paired must be set into the pairing mode.  Once detected
-- it remains paired with the cube. In order to unpair it Delete must be
-- called.
--
-- Exceptions :
--
--    Constraint_Error - Invalid timeout
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Pair
             (  Client  : in out ELV_MAX_Cube_Client;
                Timeout : Duration := 60.0
             );
--
-- Put -- The data describing the topology of devices
--
--    Destination - The buffer to store the metadata into
--    Pointer     - The position to start, advanced after the output
--    Client      - The connection object
--
-- Exceptions:
--
--    Layout_Error - Invalid pointer or no room for output
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Client      : ELV_MAX_Cube_Client
             );
--
-- Query_Device_Configuration -- Query device configuration
--
--    Client  - The connection object
--    Address - The device RF address
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Query_Device_Configuration
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address
             );
--
-- Query_Devices -- Devices status
--
--    Client - The connection object
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Query_Devices
             (  Client : in out ELV_MAX_Cube_Client
             );
--
-- Query_NTP_Servers -- Get the list of NTP servers of the cube
--
--    Client - The connection object
--
-- Upon response Configuration_Updated is called.
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Query_NTP_Servers
             (  Client : in out ELV_MAX_Cube_Client
             );
--
-- Query_Unconfigured_Devices -- Query device configurations
--
--    Client - The connection object
--
-- This procedure  queries configurations  of all devices  for which the
-- cube does not yet have a configuration.
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Query_Unconfigured_Devices
             (  Client : in out ELV_MAX_Cube_Client
             );
--
-- Rename_Device -- By its RF address
--
--    Client  - The connection object
--    Address - The device RF address
--    Name    - The new name
--
-- Exceptions :
--
--    Constraint_Error - The name is too long
--    End_Error        - No such device
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Rename_Device
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address;
                Name    : String
             );
--
-- Rename_Room -- By its ID or index
--
--    Client     - The connection object
--    Index / ID - The room number 1..Get_Number_Of_Rooms
--    Name       - The new name
--
-- Exceptions :
--
--    Constraint_Error - The name is too long
--    End_Error        - No such room
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Rename_Room
             (  Client : in out ELV_MAX_Cube_Client;
                Index  : Positive;
                Name   : String
             );
   procedure Rename_Room
             (  Client : in out ELV_MAX_Cube_Client;
                ID     : Room_ID;
                Name   : String
             );
--
-- Reset_Devices -- Reset the cube
--
--    Client - The connection object
--
-- The command removes all connected devices.
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Reset_Devices (Client : in out ELV_MAX_Cube_Client);
--
-- Reset_Error -- Reset error
--
--    Client - The connection object
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Reset_Error (Client : in out ELV_MAX_Cube_Client);
--
-- Reset_Metadata -- Reset the cube topology
--
--    Client       - The connection object
--    Data         - The metadata
--  [ S_Commands ] - The number of issued S-commands
--
-- The command  overwrites  the cube's metadata describing the rooms and
-- devices.  The data are same as reported in Get_Metadata.  It  removes
-- all existing devices  and rooms  replacing  them from  the  ones from
-- Data. The devices are linked again.  The number of  issued S-commands
-- is returned  via parameter S_Commands.  In addition one  A-command is
-- issued to store new metadata into the cube.
--
-- Exceptions :
--
--    Constraint_Error - The block is longer than 1900 characters
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Reset_Metadata
             (  Client     : in out ELV_MAX_Cube_Client;
                Data       : String
             );
   procedure Reset_Metadata
             (  Client     : in out ELV_MAX_Cube_Client;
                Data       : String;
                S_Commands : out Natural
             );
--
-- Set_Metadata -- Set the cube topology
--
--    Client - The connection object
--
-- The command  overwrites  the cube's metadata describing the rooms and
-- devices.  The metadata are taken  from  the actual  configuration  as
-- known to the client.
--
-- Exceptions :
--
--    Socket_Error - I/O error
--    Use_Error    - The device is busy
--
   procedure Set_Metadata (Client : in out ELV_MAX_Cube_Client);
--
-- Set_NTP_Servers -- Set list of NTP servers of the cube
--
--    Client - The connection object
--    List   - The list of NTP servers (set or comma separated list)
--
-- Upon response Configuration_Updated is called.
--
-- Exceptions :
--
--    Constraint_Error - Empty servers list
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Set_NTP_Servers
             (  Client : in out ELV_MAX_Cube_Client;
                List   : String
             );
   procedure Set_NTP_Servers
             (  Client : in out ELV_MAX_Cube_Client;
                List   : Servers_List.Set
             );
--
-- Set_Thermostat_Automatic -- Set device into automatic mode
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--  [ Temperature ]   - The temperature to set
--
-- In the automatic mode the device run the scheduled program
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--    Mode_Error       - The device is not a thermostat
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Set_Thermostat_Automatic
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Temperature : Centigrade := Centigrade'First
             );
   procedure Set_Thermostat_Automatic
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Temperature : Centigrade := Centigrade'First
             );
--
-- Set_Thermostat_Boost -- Boost device
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--    Mode_Error       - The device is not a thermostat
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Set_Thermostat_Boost
             (  Client : in out ELV_MAX_Cube_Client;
                Index  : Positive
             );
   procedure Set_Thermostat_Boost
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address
             );
--
-- Set_Thermostat_Display -- Set device display
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--    Temperature     - The temperature to indicate
--
-- In the automatic mode the device run the scheduled program
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--    Mode_Error       - The device is not a wall thermostat
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Set_Thermostat_Display
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Temperature : Display_Mode
             );
   procedure Set_Thermostat_Display
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Temperature : Display_Mode
             );
--
-- Set_Thermostat_Parameters -- Set device schedule
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--    Comfort         - Temperature
--    Eco             - When in eco mode
--    Max             - Temperature
--    Min             - Temperature
--    Offset          - Between the set and actual temperatures
--    Window_Open     - When associated window is open
--  [ Window_Time ]   - Before engaging window mode
--    Mode            - Execution mode
--
-- The parameter  Mode  specifies  execution method.  When S_Command  is
-- specified  the  parameters are sent to the cube.  When S_Response  is
-- specified the parameters are set in the local cache.  The  default is
-- both sending to the cube and updating the local cache.  If there is a
-- danger that  the cube may reject the command  it must be called first
-- as S_Command and  second as S_Response  after confirmation  to update
-- the cache.
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--    Socket_Error     - I/O error
--    Mode_Error       - The device is not a thermostat
--    Use_Error        - The device is busy
--
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
             );
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
             );
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
             );
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
             );
--
-- Set_Thermostat_Schedule -- Set device schedule
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--    Day             - The day for which to set the program
--    Schedule        - The list of point
--    Mode            - Execution mode
--
-- The list of  points must  be sorted  end  time ascending.  Note  that
-- there  is no way to get confirmation  that the  schedule is set.  The
-- object assumes  that it was and updates  the schedule  cached  in the
-- memory.  The actual schedule will be read only upon a new connection.
-- The parameter  Mode specifies  execution method  of schedule setting.
-- See  Set_Thermostat_Parameters.  Note that  this  procedure  uses two
-- s-commands to set the schedule.
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--    Socket_Error     - I/O error
--    Mode_Error       - Illegal program, not a thermostat
--    Use_Error        - The device is busy
--
   procedure Set_Thermostat_Schedule
             (  Client   : in out ELV_MAX_Cube_Client;
                Index    : Positive;
                Day      : Week_Day;
                Schedule : Points_List;
                Mode     : Setting_Mode := S_Command or S_Response
             );
   procedure Set_Thermostat_Schedule
             (  Client   : in out ELV_MAX_Cube_Client;
                Address  : RF_Address;
                Day      : Week_Day;
                Schedule : Points_List;
                Mode     : Setting_Mode := S_Command or S_Response
             );
--
-- Set_Thermostat_Temperature -- Set device temperature
--
--    Client              - The connection object
--    Index / Address     - The device index 1..Get_Number_Of_Devices
--    Temperature         - To set
--  [ Up_Until / Manual ] - Until this time or switch to manual flag
--
-- When Up_Until is specified the thermostat is switched to the vacation
-- mode. Otherwise, if Manual is set true the thermostat is switched  to
-- the manual mode. When Manual is false  and  the thermostat  is in the
-- automatic mode, it remains in this mode.  Otherwise it is switched to
-- the manual mode.
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--    Mode_Error       - The device is not a thermostat
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Temperature : Centigrade;
                Manual      : Boolean := True
             );
   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Temperature : Centigrade;
                Manual      : Boolean := True
             );
   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Index       : Positive;
                Temperature : Centigrade;
                Up_Until    : Time
             );
   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Temperature : Centigrade;
                Up_Until    : Time
             );
--
-- Set_Thermostat_Valve -- Set device valve parameters
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--    Boost_Time      - Duration of the boost mode
--    Boost_Valve     - Valve position in the boost mode
--    Decalcification - The time of decalcification
--    Max_Maximum     - Maximum valve position
--    Valve_Offset    - Valve position offset
--    Mode            - Execution mode
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    End_Error        - No device with the given address
--    Socket_Error     - I/O error
--    Mode_Error       - The device is not a thermostat
--    Use_Error        - The device is busy
--
   procedure Set_Thermostat_Valve
             (  Client          : in out ELV_MAX_Cube_Client;
                Index           : Positive;
                Boost_Time      : Duration  := 3.0;
                Boost_Valve     : Ratio     := 1.0;
                Decalcification : Week_Time := (Mo, 12.0 * 3600.0);
                Max_Valve       : Ratio     := 1.0;
                Valve_Offset    : Ratio     := 0.0;
                Mode         : Setting_Mode := S_Command or S_Response
             );
   procedure Set_Thermostat_Valve
             (  Client          : in out ELV_MAX_Cube_Client;
                Address         : RF_Address;
                Boost_Time      : Duration  := 3.0;
                Boost_Valve     : Ratio     := 1.0;
                Decalcification : Week_Time := (Mo, 12.0 * 3600.0);
                Max_Valve       : Ratio     := 1.0;
                Valve_Offset    : Ratio     := 0.0;
                Mode         : Setting_Mode := S_Command or S_Response
             );
--
-- Time_Zone -- Description of a time zone
--
   subtype Time_Zone_Offset is Duration range -12.0 * 3_600.0
                                           ..  13.0 * 3_600.0;
   subtype Time_Zone_Name_Count is Natural range 0..5;
   type Day_Of_Week is (Su, Mo, Tu, We, Th, Fr, Sa);
   type Hour_Number is range 0..23;
   type Zone_Data (Length : Time_Zone_Name_Count := 0) is record
      Start  : Month_Number;       -- The month when it start to apply
      Day    : Day_Of_Week;        -- The day of week
      Hour   : Hour_Number;        -- The hour
      Offset : Time_Zone_Offset;   -- The UTC offset
      Name   : String (1..Length); -- The abbreviated name
   end record;

   GMT  : constant Zone_Data := (3, 10, Su, 3, 0.0 * 3600.0, "GMT" );
   CET  : constant Zone_Data := (3, 10, Su, 3, 1.0 * 3600.0, "CET" );
   CEST : constant Zone_Data := (4,  3, Su, 2, 2.0 * 3600.0, "CEST");
--
-- Set_Time -- Set time and time zone
--
--    Client - The connection object
--    Winter - Winter time
--    Summer - Summer time
--
-- This procedure set the cube clock using the current time and the time
-- zones for the winter and summer time
--
-- Exceptions :
--
--    Constraint_Error - Wrong device number
--    Socket_Error     - I/O error
--    Use_Error        - The device is busy
--
   procedure Set_Time
             (  Client : in out ELV_MAX_Cube_Client;
                Winter : Zone_Data := CET;
                Summer : Zone_Data := CEST
             );
--
-- Status_Received -- Status reception callback
--
--    Client - The connection object
--    Error  - True if the latest command was rejected
--    Duty   - Duty
--    Slots  - Free slots
--
-- This procedure  is called when command  response  from  a  device  is
-- received.  The  default  implementation  calls  to Trace.  It  likely
-- should be overridden, e.g. to store the data.
--
   procedure Status_Received
             (  Client : in out ELV_MAX_Cube_Client;
                Error  : Boolean;
                Duty   : Ratio;
                Slots  : Natural
             );
--
-- Trace -- Higher level tracing of received data
--
   procedure Trace
             (  Client  : in out ELV_MAX_Cube_Client;
                Message : String
             );
--
-- Wake_Up -- Wake up device, room, all devices
--
--    Client           - The connection object
--  [ Address / Room ] - The device or room to wake up
--    Interval         - Wakeup time
--
-- Exceptions :
--
--    Constraint_Error - Invalid time interval
--    End_Error        - No room or no device with the given address
--    Socket_Error     - I/O error
--
   procedure Wake_Up
             (  Client   : in out ELV_MAX_Cube_Client;
                Address  : RF_Address;
                Interval : Duration := 30.0
             );
   procedure Wake_Up
             (  Client   : in out ELV_MAX_Cube_Client;
                Room     : Room_ID;
                Interval : Duration := 30.0
             );
   procedure Wake_Up
             (  Client   : in out ELV_MAX_Cube_Client;
                Interval : Duration := 30.0
             );
------------------------------------------------------------------------
   type Cube_Descriptor is record
      Address   : Inet_Addr_Type;
      Name      : String (1..8);
      Serial_No : String (1..10);
   end record;
   type Cube_Descriptor_Array is
      array (Positive range <>) of Cube_Descriptor;
--
-- Discover -- MAX! cubes in the local area network
--
--    Timeout  - The time of listening responses
--    Attempts - Number of tries
--    Host     - The host address, when missing the first official one
--    Port     - The broadcast port
--
-- Returns :
--
--    The list of discovered cubes
--
-- Exceptions :
--
--    Host_Error   - Host name error
--    Socket_Error - I/O errors
--
   function Discover
            (  Timeout  : Timeval_Duration := 2.0;
               Attempts : Positive  := 2;
               Host     : String    := "";
               Port     : Port_Type := 23272
            )  return Cube_Descriptor_Array;
------------------------------------------------------------------------
--
-- Reboot -- MAX! cube
--
--    Serial - The cube serial number (10 characters long)
--    Port   - The broadcast port
--
-- Exceptions :
--
--    Constraint_Error - Invalid serial number
--    Socket_Error     - I/O errors
--
   procedure Reboot (Serial_No : String; Port : Port_Type := 23272);
------------------------------------------------------------------------
--
-- Topology_Holder -- Topology lock
--
-- The object holds a lock on the client topology when created. The lock
-- is released upon finalization
--
   type Topology_Holder
        (  Client : access constant ELV_MAX_Cube_Client'Class
        )  is new Ada.Finalization.Limited_Controlled with private;
   procedure Finalize   (Lock : in out Topology_Holder);
   procedure Initialize (Lock : in out Topology_Holder);
------------------------------------------------------------------------
--
-- Image_Metadata -- Metadata in a human-readable format
--
--    Data - The metadata
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    Data_Error - Wrong metadata
--
   function Image_Metadata (Data : String) return String;
--
-- Put_Metadata -- Put metadata in a human-readable format
--
--    Destination - The destination string
--    Pointer     - The position in the string to start, advanced
--    Data        - The metadata
--
-- Exceptions :
--
--    Data_Error   - Wrong metadata
--    Layout_Error - No room for output or Pointer outside Destination
--
   procedure Put_Metadata
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : String
             );
private
   use GNAT.Sockets.Connection_State_Machine.Expected_Sequence;
   use GNAT.Sockets.Connection_State_Machine.Terminated_Strings;

   pragma Assert (Stream_Element'Size = 8);

   type Topology_Holder
        (  Client : access constant ELV_MAX_Cube_Client'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Released : Boolean := False;
   end record;
   procedure Release (Lock : in out Topology_Holder);

   type Device_Key is record
      Room  : Room_ID;
      Index : Positive; -- The device number in the room
   end record;
   function "<" (Left, Right : Device_Key) return Boolean;

   type Room_Descriptor (Name_Length : Natural) is
      new Object.Entity with
   record
      ID     : Room_ID;
      Master : RF_Address;
      Count  : Natural;
      Name   : String (1..Name_Length);
   end record;
   type Room_Descriptor_Ptr is access Room_Descriptor'Class;
   package Room_Handles is
      new Object.Handle (Room_Descriptor, Room_Descriptor_Ptr);
   package Rooms_Maps is
      new Generic_Map
          (  Key_Type    => Room_ID,
             Object_Type => Room_Handles.Handle
          );
   use Rooms_Maps;

   type Device_Status is mod 2**2;
   Have_Settings : constant Device_Status := 2**0;
   Have_Data     : constant Device_Status := 2**1;
   type Device_Descriptor
        (  Kind_Of     : Device_Type;
           Name_Length : Natural
        )  is new Object.Entity with
   record
      Init : Device_Status := 0;
      Room : Room_Handles.Handle;
      Data : Device_Parameters (Kind_Of, Name_Length);
      Last : Device_Data (Kind_Of);
   end record;
   type Device_Descriptor_Ptr is access Device_Descriptor'Class;
   package Device_Handles is
      new Object.Handle (Device_Descriptor, Device_Descriptor_Ptr);
   package Device_Maps is
      new Generic_Map
          (  Key_Type    => Device_Key,
             Object_Type => Device_Handles.Handle
          );
   use Device_Maps;

   package Device_Lists is
      new Generic_Map
          (  Key_Type    => RF_Address,
             Object_Type => Device_Handles.Handle
          );
   use Device_Lists;

   package Address_Maps is
      new Generic_Map
          (  Key_Type    => RF_Address,
             Object_Type => Positive
          );
   use Address_Maps;

   package String_FIFOs is new Generic_Indefinite_FIFO (String);
   type Secondary_Buffer is new String_FIFOs.FIFO with null record;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Secondary_Buffer
             );
   for Secondary_Buffer'Write use Write;

   type Handshake is mod 2**4;
   Got_H : constant Handshake := 2**0;
   Got_C : constant Handshake := 2**1;
   Got_M : constant Handshake := 2**2;
   Got_L : constant Handshake := 2**3;

   type Devices_Configuration;
   type Devices_Configuration_Ptr is access all Devices_Configuration;
   type Devices_Configuration is limited record
      Self     : Devices_Configuration_Ptr :=
                    Devices_Configuration'Unchecked_Access;
      Lock     : aliased Synchronization.Mutexes.Mutex;
      RF       : Address_Maps.Map; -- Address -> device descriptor
      Devices  : Device_Maps.Map;  -- Device key -> device descriptor
      Rooms    : Rooms_Maps.Map;   -- Room ID -> room descriptor
      Detached : Device_Lists.Map; -- Detached devices
   end record;
   procedure Build_Reference (Topology : in out Devices_Configuration);
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Devices_Configuration
             );
   for Devices_Configuration'Write use Write;

   type String_Buffer (Size : Natural) is record
      Length : Natural := 0;
      Count  : Natural := 0;
      Data   : String (1..Size);
   end record;
   type String_Buffer_Ptr is access String_Buffer;
   type ELV_MAX_Cube_Client
        (  Listener       : access Connections_Server'Class;
           Line_Length    : Positive;
           Input_Size     : Buffer_Length;
           Output_Size    : Buffer_Length;
           Secondary_Size : Storage_Count
        )  is new State_Machine (Input_Size, Output_Size) with
   record
      Ready      : Handshake  := 0;
      Duty       : Ratio      := 0.0;
      Error      : Boolean    := False;
      Blocked    : Boolean    := False; -- Redirect to secondary buffer
      Slots      : Integer    := 0;
      Address    : RF_Address := 0;
      Roomless   : Natural    := 0;
      Clock_Diff : Duration   := 0.0; -- Cube clock - host clock
      Topology   : Devices_Configuration;
      Version    : String (1..6)  := (' ', '1', '.', '0', '.', '0');
      Serial_No  : String (1..10) := (others => ' ');
      Metadata   : String_Buffer_Ptr;
      Secondary  : Secondary_Buffer (Secondary_Size);
         -- Response fields
      Line : String_Data_Item (Line_Length, Character'Val (13));
      LF   : Expected_Item (1);
      pragma Atomic (Duty);
      pragma Atomic (Error);
      pragma Atomic (Slots);
   end record;

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
             );
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
             );
   procedure Add_New_Device_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Serial_No   : String;
                Device_Name : String;
                Address     : RF_Address;
                S_Commands  : in out Natural;
                Mode        : Setting_Mode := S_Command or S_Response
             );
   procedure Attach_Device_Unchecked
             (  Client     : in out ELV_MAX_Cube_Client;
                Address    : RF_Address;
                To         : Room_Handles.Handle;
                S_Commands : in out Natural;
                Mode       : Setting_Mode := S_Command or S_Response
             );
   procedure Detach_Device_Unchecked
             (  Client       : in out ELV_MAX_Cube_Client;
                Address      : RF_Address;
                Set_Metadata : Boolean;
                Device       : out Device_Handles.Handle;
                S_Commands   : in out Natural;
                Mode         : Setting_Mode := S_Command or S_Response
             );
   function Find_Room_Unchecked
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return Positive;
   function Get_Device_Unchecked
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Positive;
   function Get_Device_Unchecked_Unsafe
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Natural;
   function Is_New_Master
            (  Client  : ELV_MAX_Cube_Client;
               Kind_Of : Device_Type;
               Room    : Room_Descriptor'Class
            )  return Boolean;
   procedure Link_Unchecked
             (  Client     : in out ELV_MAX_Cube_Client;
                Kind_Of    : Partner_Device_Type;
                Address    : RF_Address;
                To         : Room_Descriptor'Class;
                New_Master : Boolean;
                S_Commands : in out Natural
             );
   procedure Process_Packet (Client : in out ELV_MAX_Cube_Client);
   procedure Set_From_Metadata_Unchecked
             (  Client     : in out ELV_MAX_Cube_Client;
                Lock       : in out Topology_Holder'Class;
                Data       : String;
                M_Response : Boolean
             );
   procedure Set_Metadata_Unchecked
             (  Client  : in out ELV_MAX_Cube_Client;
                Exclude : RF_Address := 0
             );
   procedure Set_Thermostat_Automatic_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : Device_Descriptor'Class;
                Temperature : Centigrade
             );
   procedure Set_Thermostat_Boost_Unchecked
             (  Client : in out ELV_MAX_Cube_Client;
                Device : Device_Descriptor'Class
             );
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
             );
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
             );
   procedure Set_Thermostat_Schedule_Unchecked
             (  Client   : in out ELV_MAX_Cube_Client;
                Device   : in out Device_Descriptor'Class;
                Day      : Week_Day;
                Schedule : Points_List;
                Mode     : Setting_Mode
             );
   procedure Set_Thermostat_Temperature_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : in out Device_Descriptor'Class;
                Temperature : Centigrade;
                Manual      : Boolean
             );
   procedure Set_Thermostat_Temperature_Unchecked
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : in out Device_Descriptor'Class;
                Temperature : Centigrade;
                Up_Until    : Time
             );
   procedure Set_Thermostat_Valve_Unchecked
             (  Client          : in out ELV_MAX_Cube_Client;
                Device          : in out Device_Descriptor'Class;
                Boost_Time      : Duration;
                Boost_Valve     : Ratio;
                Decalcification : Week_Time;
                Max_Valve       : Ratio;
                Valve_Offset    : Ratio;
                Mode            : Setting_Mode
             );
   procedure Send
             (  Client : in out ELV_MAX_Cube_Client;
                Data   : String
             );
   function Sort_Unchecked
            (  Client : ELV_MAX_Cube_Client;
               Data   : RF_Address_Array
            )  return RF_Address_Array;
   procedure Unlink_Unchecked
             (  Client     : in out ELV_MAX_Cube_Client;
                Kind_Of    : Partner_Device_Type;
                Address    : RF_Address;
                From       : Room_Descriptor'Class;
                S_Commands : in out Natural
             );
   procedure Wake_Up_Unchecked
             (  Client   : in out ELV_MAX_Cube_Client;
                Room     : Room_ID;
                Interval : Duration := 30.0
             );

   No_Room : constant Room_ID := 0;

   procedure Get_Address
             (  Line    : String;
                Pointer : in out Integer;
                Address : out RF_Address
             );
   procedure Get_Comma
             (  Line    : String;
                Pointer : in out Integer
             );
   procedure Get_Type
             (  Line    : String;
                Pointer : in out Integer;
                Kind_Of : out Device_Type
             );

end GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
