--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ELV_MAX_Cube_Client                         Summer, 2015       --
--  Interface                                                         --
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

with Ada.Calendar;             use Ada.Calendar;
with Ada.Exceptions;           use Ada.Exceptions;
with Synchronization.Mutexes;  use Synchronization.Mutexes;

with GNAT.Sockets.Connection_State_Machine.Expected_Sequence;
with GNAT.Sockets.Connection_State_Machine.Terminated_Strings;

with Generic_Indefinite_Set;
with Generic_Map;
with Object.Handle;

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
      Error         : Boolean := False;
      Initialized   : Boolean := False;
      Battery_Low   : Boolean := False;
      Link_Error    : Boolean := False;
      Panel_Locked  : Boolean := False;
      Gateway_Known : Boolean := False;
      DST           : Boolean := False; -- Daylight saving time
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
            Comfort  : Centigrade := 18.0;
            Eco      : Centigrade := 18.0;
            Max      : Centigrade := 18.0;
            Min      : Centigrade := 18.0;
            Schedule : Week_Schedule;
            case Kind_Of is
               when Radiator_Thermostat | Radiator_Thermostat_Plus =>
                  Offset          : Centigrade := 0.0;
                  Window_Open     : Centigrade := 18.0;
                  Window_Time     : Duration   := 10.0;
                  Boost_Time      : Duration   := 1.0;
                  Boost_Valve     : Ratio      := 0.5;
                  Decalcification : Week_Time;
                  Max_Valve       : Ratio;
                  Valve_Offset    : Ratio;
               when others =>
                  null;
            end case;
      end case;
   end record;
--
-- ELV_MAX_Cube_Client -- An object implementing ELV MAX! Cube client
--
--    Listener    - The connections server object
--    Line_Length - The maximal response line length
--    Input_Size  - The size of the input buffer
--    Output_Size - The size of the output buffer
--
-- The  Output_Size can  be limited to 140 elements or so,  if  only one
-- request will be sent at a time.  If the client wanted  to queue  more
-- than one request it should be increased correspondingly.
--
   type ELV_MAX_Cube_Client
        (  Listener    : access Connections_Server'Class;
           Line_Length : Positive;
           Input_Size  : Buffer_Length;
           Output_Size : Buffer_Length
        )  is new State_Machine with private;
--  --
--  -- Add_New_Device -- Room RF address
--  --
--  --    Client     - The connection object
--  --    Index / ID - The room number 1..Get_Number_Of_Rooms
--  --    Device     - The type of the new device to add
--  --    Address    - The RF address of the device
--  --
--  -- Exceptions :
--  --
--  --    Constraint_Error - Wrong room number or wrong device type
--  --    Socket_Error     - I/O error
--  --    Use_Error        - The device is busy
--  --
--     procedure Add_New_Device
--               (  Client  : in out ELV_MAX_Cube_Client;
--                  Index   : Positive;
--                  Device  : Partner_Device_Type;
--                  Address : RF_Address
--               );
--     procedure Add_New_Device
--               (  Client  : in out ELV_MAX_Cube_Client;
--                  ID      : Room_ID;
--                  Device  : Partner_Device_Type;
--                  Address : RF_Address
--               );
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
           Device_Parameters_Update,
           Device_Discovery_Update,
           End_Discovery_Update,
           NTP_Servers_List_Update
        );
   type Update_Data (Kind_Of : Update_Type) is record
      case Kind_Of is
         when Cube_Update | Topology_Update | End_Discovery_Update =>
            null;
         when Device_Parameters_Update | Device_Discovery_Update =>
            Device  : Device_Type; -- The device type
            Address : RF_Address;  -- Of the device updated
            case Kind_Of is
               when Device_Discovery_Update =>
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
-- Disconnected -- Connection loss notification
--
--    Client - The connection object
--
-- When overridden this procedure  must always be  called   from the new
-- implementation.
--
   procedure Disconnected (Client : in out ELV_MAX_Cube_Client);
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
--    Client         - The connection object
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
--    Client - The connection object
--
-- The cube announces its frequency once connected to
--
-- Returns :
--
--    The radio frequency address
--
-- Exceptions :
--
--    Status_Error -- No handshake yet
--
   function Get_RF_Address (Client : ELV_MAX_Cube_Client)
      return RF_Address;
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
-- device  in order to be  paired must be set into the pairing mode.
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
             (  Client : in out ELV_MAX_Cube_Client;
                Index  : Positive
             );
   procedure Set_Thermostat_Automatic
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address
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
-- Set_Thermostat_Parameters -- Set device schedule
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--    Comfort         - Temperature
--    Eco             - When in eco mode
--    Max             - Temperature
--    Min             - Temperature
--  [ Offset          - Between the set and actual temperatures
--    Window_Open     - When associated window is open
--    Window_Time ]   - Before engaging window mode
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
                Window_Time : Day_Duration
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
                Window_Time : Day_Duration
             );
   procedure Set_Thermostat_Parameters
             (  Client  : in out ELV_MAX_Cube_Client;
                Index   : Positive;
                Comfort : Centigrade;
                Eco     : Centigrade;
                Max     : Centigrade;
                Min     : Centigrade
             );
   procedure Set_Thermostat_Parameters
             (  Client  : in out ELV_MAX_Cube_Client;
                Address : RF_Address;
                Comfort : Centigrade;
                Eco     : Centigrade;
                Max     : Centigrade;
                Min     : Centigrade
             );
--
-- Set_Thermostat_Schedule -- Set device schedule
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--    Day             - The day for which to set the program
--    Schedule        - The list of point
--
-- The list of  points must  be sorted  end  time ascending.  Note  that
-- there  is no way to get confirmation  that the  schedule is set.  The
-- object assumes  that it was and updates  the schedule  cached  in the
-- memory. The actual schedule will be read only upon a new connection.
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
                Schedule : Points_List
             );
   procedure Set_Thermostat_Schedule
             (  Client   : in out ELV_MAX_Cube_Client;
                Address  : RF_Address;
                Day      : Week_Day;
                Schedule : Points_List
             );
--
-- Set_Thermostat_Temperature -- Set device temperature
--
--    Client          - The connection object
--    Index / Address - The device index 1..Get_Number_Of_Devices
--    Temperature     - To set
--  [ Up_Until ]      - Until this time
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
                Temperature : Centigrade
             );
   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Address     : RF_Address;
                Temperature : Centigrade
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
private
   use GNAT.Sockets.Connection_State_Machine.Expected_Sequence;
   use GNAT.Sockets.Connection_State_Machine.Terminated_Strings;

   pragma Assert (Stream_Element'Size = 8);

   type Device_Key is record
      Room  : Room_ID;
      Index : Positive;
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

   type Device_Descriptor
        (  Kind_Of     : Device_Type;
           Name_Length : Natural
        )  is new Object.Entity with
   record
      Init : Boolean := False; -- Has actual data
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

   package Address_Maps is
      new Generic_Map
          (  Key_Type    => RF_Address,
             Object_Type => Positive
          );
   use Address_Maps;

   type Handshake is mod 2**4;
   Got_H : constant Handshake := 2**0;
   Got_C : constant Handshake := 2**1;
   Got_M : constant Handshake := 2**2;
   Got_L : constant Handshake := 2**3;

   type Devices_Configuration;
   type Devices_Configuration_Ptr is access all Devices_Configuration;
   type Devices_Configuration is limited record
      Self    : Devices_Configuration_Ptr :=
                   Devices_Configuration'Unchecked_Access;
      Lock    : aliased Mutex;
      RF      : Address_Maps.Map; -- Map address -> device descriptor
      Devices : Device_Maps.Map;  -- Map device key -> device descriptor
      Rooms   : Rooms_Maps.Map;   -- Map room ID -> room descriptor
   end record;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Devices_Configuration
             );
   for Devices_Configuration'Write use Write;

   type ELV_MAX_Cube_Client
        (  Listener    : access Connections_Server'Class;
           Line_Length : Positive;
           Input_Size  : Buffer_Length;
           Output_Size : Buffer_Length
        )  is new State_Machine (Input_Size, Output_Size) with
   record
      Ready      : Handshake  := 0;
      Duty       : Ratio      := 0.0;
      Error      : Boolean    := False;
      Slots      : Integer    := 0;
      Address    : RF_Address := 0;
      Roomless   : Natural    := 0;
      Clock_Diff : Duration   := 0.0; -- Cube clock - host clock
      Topology   : Devices_Configuration;
      Version    : String (1..6)  := (' ', '1', '.', '0', '.', '0');
      Serial_No  : String (1..10) := (others => ' ');
         -- Response fields
      Line : String_Data_Item (Line_Length, Character'Val (13));
      LF   : Expected_Item (1);
      pragma Atomic (Duty);
      pragma Atomic (Error);
      pragma Atomic (Slots);
   end record;

--     procedure Add_New_Device_Unchecked
--               (  Client  : in out ELV_MAX_Cube_Client;
--                  Room    : Room_Descriptor'Class;
--                  Device  : Partner_Device_Type;
--                  Address : RF_Address
--               );
   function Find_Room_Unchecked
            (  Client : ELV_MAX_Cube_Client;
               ID     : Room_ID
            )  return Positive;
   function Get_Device_Unchecked
            (  Client  : ELV_MAX_Cube_Client;
               Address : RF_Address
            )  return Positive;
   procedure Set_Thermostat_Automatic
             (  Client : in out ELV_MAX_Cube_Client;
                Device : Device_Descriptor'Class
             );
   procedure Set_Thermostat_Boost
             (  Client : in out ELV_MAX_Cube_Client;
                Device : Device_Descriptor'Class
             );
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
             );
   procedure Set_Thermostat_Parameters
             (  Client  : in out ELV_MAX_Cube_Client;
                Device  : in out Device_Descriptor'Class;
                Comfort : Centigrade;
                Eco     : Centigrade;
                Max     : Centigrade;
                Min     : Centigrade
             );
   procedure Set_Thermostat_Schedule
             (  Client   : in out ELV_MAX_Cube_Client;
                Device   : in out Device_Descriptor'Class;
                Day      : Week_Day;
                Schedule : Points_List
             );
   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : in out Device_Descriptor'Class;
                Temperature : Centigrade
             );
   procedure Set_Thermostat_Temperature
             (  Client      : in out ELV_MAX_Cube_Client;
                Device      : in out Device_Descriptor'Class;
                Temperature : Centigrade;
                Up_Until    : Time
             );

   procedure Process_Packet (Client : in out ELV_MAX_Cube_Client);

   procedure Send
             (  Client : in out ELV_MAX_Cube_Client;
                Data   : String
             );
   procedure Trace
             (  Client  : in out ELV_MAX_Cube_Client;
                Message : String
             );

   No_Room : constant Room_ID := 0;

   procedure Get_Comma
             (  Line    : String;
                Pointer : in out Integer
             );

end GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
