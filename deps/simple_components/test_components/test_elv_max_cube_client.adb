--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_ELV_MAX_Cube_Client                    Luebeck            --
--  ELV MAX! Cube client test                      Summer, 2015       --
--                                                                    --
--                                Last revision :  16:49 28 Feb 2016  --
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

with Ada.Calendar;                 use Ada.Calendar;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;     use Ada.Text_IO.Text_Streams;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Interfaces;                   use Interfaces;
with Strings_Edit;                 use Strings_Edit;
with Strings_Edit.Floats;          use Strings_Edit.Floats;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.Streams;         use Strings_Edit.Streams;

with GNAT.Sockets.Connection_State_Machine.
     ELV_MAX_Cube_Client.Stream_IO;
with Strings_Edit.Streams;

procedure Test_ELV_MAX_Cube_Client is
   Timeout : Duration := 3.0;

   procedure Test_Asynchronous (Cube : String) is
      use GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
      Factory   : aliased Connections_Factory;
      Server    : aliased Connections_Server (Factory'Access, 0);
      Reference : Handle;
   begin
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Decoded,
         Sent     => GNAT.Sockets.Server.Trace_Decoded
      );
      Set
      (  Reference,
         new ELV_MAX_Cube_Client
             (  Listener    => Server'Unchecked_Access,
                Line_Length => 1024,
                Input_Size  => 80,
                Output_Size => 200
      )      );
      declare
         Client : ELV_MAX_Cube_Client renames
                  ELV_MAX_Cube_Client (Ptr (Reference).all);
      begin
         Connect
         (  Server,
            Client'Unchecked_Access,
            Cube,
            ELV_MAX_Cube_Port
         );
         while not Is_Connected (Client) loop -- Busy waiting
            delay Timeout;
         end loop;
         Put_Line ("ELV_MAX_Cube client connected");
         Put_Line ("Serial No : " & Get_Serial_No (Client));
         Put_Line ("Version   : " & Get_Version (Client));
         Put_Line ("Address   : " &
                 Image (Integer (Get_RF_Address (Client)), Base => 16));
         Put_Line ("Clock skew: " &
                 Image (Float (Get_Clock_Difference (Client))) & "s");
         Put_Line ("Rooms -------------------------------");
         declare
            Line    : String (1..200);
            Pointer : Integer;
         begin
            for Index in 1..Get_Number_Of_Rooms (Client) loop
               Pointer := 1;
               Put (Line, Pointer, "  ");
               Put
               (  Destination => Line,
                  Pointer     => Pointer,
                  Value       => Integer (Get_Room_ID (Client, Index)),
                  Field       => 3,
                  Justify     => Right,
                  Fill        => ' '
               );
               Put (Line, Pointer, " ");
               Put
               (  Destination => Line,
                  Pointer     => Pointer,
                  Value       =>
                     Image (Get_Room_RF_Address (Client, Index))
               );
               Put (Line, Pointer, " ");
               Put (Line, Pointer, Get_Room_Name (Client, Index));
               Put (Line, Pointer, " ");
               Put
               (  Line,
                  Pointer,
                  Get_Number_Of_Devices (Client, Index)
               );
               Put (Line, Pointer, " devices");
               Put_Line (Line (1..Pointer - 1));
            end loop;
         end;
         Put_Line ("Devices ----------------------------");
         declare
            Line    : String (1..200);
            Room    : Room_ID;
            Pointer : Integer;
         begin
            for Index in 1..Get_Number_Of_Devices (Client) loop
               Pointer := 1;
               Put (Line, Pointer, "   ");
               Put
               (  Destination => Line,
                  Pointer     => Pointer,
                  Field       => 26,
                  Fill        => ' ',
                  Value       => Image (Get_Device_Type (Client, Index))
               );
               Put (Line, Pointer, " ");
               Put
               (  Destination => Line,
                  Pointer     => Pointer,
                  Value       =>
                           Image (Get_Device_RF_Address (Client, Index))
               );
               Put (Line, Pointer, " ");
               Put
               (  Line,
                  Pointer,
                  Get_Device_Serial_No (Client, Index)
               );
               Put (Line, Pointer, " ");
               Put (Line, Pointer, Get_Device_Name (Client, Index));
               Room := Get_Device_Room (Client, Index);
               Put (Line, Pointer, " in the room ");
               Put (Line, Pointer, Integer (Room));
               if Room /= No_Room then
                  Put (Line, Pointer, " '");
                  Put
                  (  Line,
                     Pointer,
                     Get_Room_Name (Client, Find_Room (Client, Room))
                  );
                  Put (Line, Pointer, "'");
               end if;
               Put_Line (Line (1..Pointer - 1));
               declare
                  Data : Device_Parameters :=
                         Get_Device_Parameters (Client, Index);
               begin
                  case Data.Kind_Of is
                     when Radiator_Thermostat |
                          Radiator_Thermostat_Plus =>
                        Put_Line
                        (  "      Comfort temperature: "
                        &  Image (Data.Comfort)
                        );
                        Put_Line
                        (  "      Eco temperature    : "
                        &  Image (Data.Eco)
                        );
                        Put_Line
                        (  "      Max set temperature: "
                        &  Image (Data.Max)
                        );
                        Put_Line
                        (  "      Min set temperature: "
                        &  Image (Data.Min)
                        );
                        Put_Line
                        (  "      Offset             : "
                        &  Image (Data.Offset)
                        );
                        Put_Line
                        (  "      Window open        : "
                        &  Image (Data.Window_Open)
                        );
                        Put_Line
                        (  "      Window time        : "
                        &  Image (Integer (Data.Window_Time) / 60)
                        &  " min"
                        );
                        Put_Line
                        (  "      Boost time         : "
                        &  Image (Integer (Data.Boost_Time) / 60)
                        &  " min"
                        );
                        Put_Line
                        (  "      Boost valve pos.   : "
                        &  Image (Data.Boost_Valve * 100.0)
                        &  '%'
                        );
                        Put_Line
                        (  "      Decalcification at : "
                        &  Image (Data.Decalcification)
                        );
                        Put_Line
                        (  "      Max valve setting  : "
                        &  Image (Data.Max_Valve * 100.0)
                        &  '%'
                        );
                        Put_Line
                        (  "      Valve offset       : "
                        &  Image (Data.Valve_Offset * 100.0)
                        &  '%'
                        );
                        Put_Line (  "      Schedule           : ");
                        for Day in Data.Schedule'Range loop
                           Put ("         " & Image (Day) & " ");
                           declare
                              List : Points_List :=
                                     Data.Schedule (Day).Points;
                           begin
                              for Point in List'Range loop
                                 if Point > 1 then
                                    Put (", ");
                                 end if;
                                 declare
                                    This : Set_Point := List (Point);
                                 begin
                                    Put (Minutes (This.Last));
                                    Put ("=");
                                    Put (Image (This.Point));
                                 end;
                              end loop;
                              New_Line;
                           end;
                        end loop;
                     when Wall_Thermostat =>
                        Put_Line
                        (  "      Comfort temperature: "
                        &  Image (Data.Comfort)
                        );
                        Put_Line
                        (  "      Eco temperature    : "
                        &  Image (Data.Eco)
                        );
                        Put_Line
                        (  "      Max set temperature: "
                        &  Image (Data.Max)
                        );
                        Put_Line
                        (  "      Min set temperature: "
                        &  Image (Data.Min)
                        );
                        Put_Line (  "      Schedule           : ");
                        for Day in Data.Schedule'Range loop
                           Put ("         " & Image (Day) & " ");
                           declare
                              List : Points_List :=
                                     Data.Schedule (Day).Points;
                           begin
                              for Point in List'Range loop
                                 if Point > 1 then
                                    Put (", ");
                                 end if;
                                 declare
                                    This : Set_Point := List (Point);
                                 begin
                                    Put (Minutes (This.Last));
                                    Put ("=");
                                    Put (Image (This.Point));
                                 end;
                              end loop;
                              New_Line;
                           end;
                        end loop;
                     when others =>
                        null;
                  end case;
               end;
            end loop;
         end;
         delay Timeout;
--         Put_Line ("Reset error");
--         Reset_Error (Client);
         Put_Line ("Query devices status");
         Query_Devices (Client);
         delay Timeout;
if false then
         declare
            Address : RF_Address := 16#0B76DA#;
            Data    : Device_Data := Get_Device_Data (Client, Address);
            procedure To_Automatic is
            begin
               Put_Line
               (   "Set automatic "
               &  Image (Address)
               );
               Set_Thermostat_Automatic (Client, Address);
               delay Timeout;
               if Get_Error (Client) then
                  raise Status_Error;
               end if;
               loop
                  Query_Devices (Client);
                  delay Timeout;
                  exit when Get_Device_Data (Client, Address).Mode
                          = Automatic;
                  Put_Line ("Themostat not ready, waiting");
               end loop;
            end To_Automatic;
            procedure To_Manual is
            begin
               Put_Line
               (   "Set manual temperature "
               &  Image (Address)
               &  " -> "
               &  Image (Data.Set_Temperature)
               );
               Set_Thermostat_Temperature
               (  Client,
                  Address,
                  Data.Set_Temperature
               );
               delay Timeout;
               if Get_Error (Client) then
                  raise Status_Error;
               end if;
               loop
                  Query_Devices (Client);
                  delay Timeout;
                  exit when Get_Device_Data (Client, Address).Mode
                          = Manual;
                  Put_Line ("Themostat not ready, waiting");
               end loop;
            end To_Manual;
         begin
            if Data.Mode = Automatic then
               To_Manual;
               Query_Devices (Client);
               delay 60.0;
               Query_Devices (Client);
               delay 60.0;
               Query_Devices (Client);
               delay 60.0;
               Query_Devices (Client);
               delay 60.0;
               To_Automatic;
            elsif Data.Mode = Manual then
               To_Automatic;
               Query_Devices (Client);
               delay 60.0;
               Query_Devices (Client);
               delay 60.0;
               Query_Devices (Client);
               delay 60.0;
               Query_Devices (Client);
               delay 60.0;
               To_Manual;
            end if;
         exception
            when Status_Error =>
               Put_Line ("***** Failed to change mode");
         end;
         delay Timeout;
end if;
--           Put_Line ("Set device mode automatic");
--           Set_Thermostat_Automatic (Client, RF_Address'(16#0C8EA2#));
--           Put_Line ("Set device mode boost");
--           Set_Thermostat_Boost (Client, RF_Address'(16#0C8EA2#));
--           Put_Line ("Set device temperature");
--           Set_Thermostat_Temperature (Client, RF_Address'(16#0C8EA2#), 18.0);
--           Put_Line ("Set device temperature for one hour");
--           Set_Thermostat_Temperature
--           (  Client,
--              RF_Address'(16#0C8EA2#),
--              18.0,
--              Clock + 3600.0
--           );
--            Put_Line ("Set device schedule");
--            Set_Thermostat_Schedule
--            (  Client,
--               RF_Address'(16#0C8EA2#),
--               Su,
--               ((2.0 * 3600.0, 17.5), (5.0 * 3600.0,18.5))
--           );
if false then
         Put_Line ("Query NTP servers");
         Query_NTP_Servers (Client);
         delay Timeout;
         Put_Line ("Set NTP servers");
         Set_NTP_Servers (Client, "de.pool.ntp.org,ntp.homematic.com");
         delay Timeout;
end if;
if false then
         Put_Line ("Device pairing ----------------------------------");
         Pair (Client, 50.0);
         delay 60.0;
end if;
--           Put_Line ("Add device 050F6D");
--           Add_New_Device
--           (  Client  => Client,
--              Index   => 4,
--              Device  => Shutter_Contact,
--              Address => 16#050F6D#
--           );
--           delay Timeout;
      end;
   end Test_Asynchronous;

begin
   declare
      use GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
      use Stream_IO;
      use Strings_Edit.Streams;
      Stream : aliased String_Stream (1024 * 2);
   begin
      declare
         Value : Centigrade;
      begin
         Write (Stream'Access, Centigrade'(10.5));
         Write (Stream'Access, Centigrade'(18.0));
         Write (Stream'Access, Centigrade'(27.5));
         Set (Stream, Get (Stream));
         Value := Read (Stream'Access);
         if Centigrade'(10.5) /= Value then
            Raise_Exception
            (  Data_Error'Identity,
               "Centigrade I/O error 10.5 /= " & Image (Value)
            );
         end if;
         Value := Read (Stream'Access);
         if Centigrade'(18.0) /= Value then
            Raise_Exception
            (  Data_Error'Identity,
               "Centigrade I/O error 18.0 /= " & Image (Value)
            );
         end if;
         Value := Read (Stream'Access);
         if Centigrade'(27.5) /= Value then
            Raise_Exception
            (  Data_Error'Identity,
               "Centigrade I/O error 27.5 /= " & Image (Value)
            );
         end if;
      end;
      Rewind (Stream);
      for Device in Device_Type'Range loop
         Write (Stream'Access, Device);
      end loop;
      Set (Stream, Get (Stream));
      for Device in Device_Type'Range loop
         if Device /= Read (Stream'Access) then
            Raise_Exception
            (  Data_Error'Identity,
               "Device type I/O error"
            );
         end if;
      end loop;
      Rewind (Stream);
      declare
         H  : Day_Duration := 3600.0;
         M  : Day_Duration := 60.0;
         V1 : Day_Schedule (9);
      begin
         V1.Points (1) := ( 1 * H, 18.0);
         V1.Points (2) := ( 2 * H, 18.5);
         V1.Points (3) := ( 3 * H, 30.5);
         V1.Points (4) := ( 4 * H, 27.5);
         V1.Points (5) := ( 5 * H, 10.0);
         V1.Points (6) := ( 6 * H, 10.5);
         V1.Points (7) := ( 7 * H, 19.5);
         V1.Points (8) := (10 * H, 20.0);
         V1.Points (9) := (18 * H, 21.5);
         Write (Stream'Access, V1);
         Set (Stream, Get (Stream));
         declare
            V2 : Day_Schedule := Read (Stream'Access);
         begin
            if V1.Length /= V2.Length then
               Raise_Exception
               (  Data_Error'Identity,
                  "Day_Schedule I/O error, wrong length"
               );
            end if;
            for Index in V1.Points'Range loop
               declare
                  P1 : Set_Point := V1.Points (Index);
                  P2 : Set_Point := V2.Points (Index);
               begin
                  if P1.Point /= P2.Point then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Day_Schedule I/O error, point "
                        &  Image (Index)
                        &  ", "
                        &  Image (P1.Point)
                        &  " /= "
                        &  Image (P2.Point)
                     )  );
                  end if;
                  if P1.Last /= P2.Last then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Day_Schedule I/O error, point "
                        &  Image (Index)
                        &  ", "
                        &  Minutes (P1.Last)
                        &  " /= "
                        &  Minutes (P2.Last)
                     )  );
                  end if;
               end;
            end loop;
         end;
      end;
      Rewind (Stream);
   end;
   declare
      use GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
      Cubes : Cube_Descriptor_Array := Discover;
   begin
      if Cubes'Length = 0 then
         Put_Line ("No MAX! cube discovered");
      else
         Put_Line ("MAX! cubes discovered:");
         Put_Line ("---------------------------------");
         for Index in Cubes'Range loop
            Put_Line
            (  GNAT.Sockets.Image (Cubes (Index).Address)
            &  " "
            &  Cubes (Index).Name
            &  " "
            &  Cubes (Index).Serial_No
            );
         end loop;
         Put_Line ("---------------------------------");
         Test_Asynchronous
         (  GNAT.Sockets.Image (Cubes (Cubes'First).Address)
         );
      end if;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_ELV_MAX_Cube_Client;
