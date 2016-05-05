--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ELV_MAX_Cube_Client.Stream_IO               Summer, 2015       --
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
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
             Stream_IO is

   pragma Assert (Stream_Element'Size = 8);

   function Encode (Last : Day_Duration) return Unsigned_16 is
   begin
      return Unsigned_16 (Last / 300.0);
   end Encode;

   function Encode (Temperature : Centigrade) return Unsigned_16 is
      Value : constant Float := Float (Temperature) * 2.0;
   begin
      if Value <= 0.0 then
         return 0;
      elsif Value >= 127.0 then
         return 127;
      else
         return Unsigned_16 (Value);
      end if;
   end Encode;

   function Pack (Point : Set_Point) return Set_Point_Packed is
   begin
       return Set_Point_Packed
              (  Encode (Point.Point) * 2**9
              +  Encode (Point.Last)
              );
   end Pack;

   procedure Put
             (  Data     : in out Stream_Element_Array;
                Pointer  : in out Stream_Element_Offset;
                Schedule : Day_Schedule
             )  is
      Value : Unsigned_16;
   begin
      Data (Pointer) := Stream_Element (Schedule.Length);
      Pointer := Pointer + 1;
      for Index in Schedule.Points'Range loop
         Value := Encode (Schedule.Points (Index).Point) * 2**9
                + Encode (Schedule.Points (Index).Last);
         Data (Pointer)     := Stream_Element (Value / 2**8);
         Data (Pointer + 1) := Stream_Element (Value mod 2**8);
         Pointer := Pointer + 2;
      end loop;
   end Put;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Centigrade is
   begin
      return Centigrade (Float (Stream_Element'Input (Stream)) * 0.5);
   end Read;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Device_Duration is
      Value : constant Duration :=
              Duration (Stream_Element'Input (Stream)) * 300.0;
   begin
      if Value >= Device_Duration'Last then
         return Device_Duration'Last;
      else
         return Value;
      end if;
   end Read;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Device_Type is
      Byte : constant Stream_Element := Stream_Element'Input (Stream);
   begin
      if Byte > Device_Type'Pos (Device_Type'Last) then
         Raise_Exception (Data_Error'Identity, "Invalid device type");
      end if;
      return Device_Type'Val (Byte);
   end Read;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Day_Schedule is
      Byte : constant Stream_Element := Stream_Element'Input (Stream);
   begin
      if Byte > Stream_Element (Point_Count'Last) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Day schedule "
            &  Image (Integer (Byte))
            &  " length is longer than 13 items"
         )  );
      end if;
      declare
         Result : Day_Schedule (Point_Count (Byte));
         Last   : Duration := -1.0;
      begin
         for Index in Result.Points'Range loop
            declare
               Data  : Unsigned_16;
               Value : Duration;
               This  : Set_Point renames Result.Points (Index);
            begin
               Data :=
                  Unsigned_16 (Stream_Element'Input (Stream)) * 2**8;
               Data :=
                  Data + Unsigned_16 (Stream_Element'Input (Stream));
               This.Point := Centigrade (Float (Data / 2**9) * 0.5);
               Value := Duration (Data mod 2**9) * 300.0;
               if Value > Day_Duration'Last then
                  This.Last := Day_Duration'Last;
               else
                  This.Last := Value;
               end if;
               if Last >= This.Last then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Day schedule last time not ascending"
                  );
               else
                  Last := This.Last;
               end if;
            end;
         end loop;
         return Result;
      end;
   end Read;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Device_Parameters is
      Kind_Of : constant Device_Type := Read (Stream);
   begin
      declare
         Result : Device_Parameters
                  (  Kind_Of,
                     Natural (Stream_Element'Input (Stream))
                  );
      begin
         Result.Room    := Read (Stream);
         Result.Address := Read (Stream);
         String'Read (Stream, Result.Serial_No);
         String'Read (Stream, Result.Name);
         case Kind_Of is
            when Cube | Wall_Thermostat | Shutter_Contact | Eco_Button |
                 Unknown =>
               null;
            when Radiator_Thermostat | Radiator_Thermostat_Plus =>
               Result.Comfort         := Read (Stream);
               Result.Eco             := Read (Stream);
               Result.Max             := Read (Stream);
               Result.Min             := Read (Stream);
               Result.Offset          := Read (Stream);
               Result.Window_Open     := Read (Stream);
               Result.Window_Time     := Read (Stream);
               Result.Boost_Time      := Read (Stream);
               Result.Boost_Valve     := Read (Stream);
               Result.Decalcification := Read (Stream);
               Result.Max_Valve       := Read (Stream);
               Result.Valve_Offset    := Read (Stream);
               Result.Schedule        := Read (Stream);
         end case;
         return Result;
      end;
   end Read;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Ratio is
      Value : constant Float :=
              Float (Stream_Element'Input (Stream)) / 255.0;
   begin
      if Value >= Float (Ratio'Last) then
         return Ratio'Last;
      else
         return Ratio (Value);
      end if;
   end Read;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return RF_Address is
      Triplet : Stream_Element_Array (1..3);
   begin
      Stream_Element_Array'Read (Stream, Triplet);
      return
      (  RF_Address (Triplet (1)) * 2**16
      +  RF_Address (Triplet (2)) * 2**8
      +  RF_Address (Triplet (3))
      );
   end Read;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Room_ID is
   begin
      return Room_ID (Stream_Element'Input (Stream));
   end Read;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Week_Schedule is
      Result : Week_Schedule;
   begin
      for Day in Result'Range loop
         Result (Day) := Read (Stream);
      end loop;
      return Result;
   end Read;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Week_Time is
      Byte   : Stream_Element := Stream_Element'Input (Stream);
      Result : Week_Time;
   begin
      if Byte / 2**5 > 6 then
         Raise_Exception
         (  Data_Error'Identity,
            "Inavalid week time day number"
         );
      end if;
      Result.Day := Week_Day'Val (Byte / 2**5);
      Byte := Byte mod 2**5;
      if Byte > 24 then
         Raise_Exception
         (  Data_Error'Identity,
            "Inavalid week time hour number"
         );
      end if;
      Result.Time := Duration (Byte) * 3600.0;
      return Result;
   end Read;

   function Unpack (Point : Set_Point_Packed) return Set_Point is
      Value  : Duration;
      Result : Set_Point;
   begin
      Result.Point := Centigrade (Float (Point / 2**9) * 0.5);
      Value := Duration (Point mod 2**9) * 300.0;
      if Value > Day_Duration'Last then
         Result.Last := Day_Duration'Last;
      else
         Result.Last := Value;
      end if;
      return Result;
   end Unpack;

   procedure Write
             (  Stream      : access Root_Stream_Type'Class;
                Temperature : Centigrade
             )  is
      Value : constant Float := Float (Temperature) * 2.0;
   begin
      if Value >= 255.5 then
         Stream_Element'Write (Stream, Stream_Element'Val (255));
      elsif Value <= 0.0 then
         Stream_Element'Write (Stream, Stream_Element'Val (0));
      else
         Stream_Element'Write
         (  Stream,
            Stream_Element'Val (Integer (Value))
         );
      end if;
   end Write;

   procedure Write
             (  Stream  : access Root_Stream_Type'Class;
                Kind_Of : Device_Type
             )  is
   begin
      Stream_Element'Write
      (  Stream,
         Stream_Element'Val (Device_Type'Pos (Kind_Of))
      );
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Value  : Device_Duration
             )  is
   begin
      Stream_Element'Write
      (  Stream,
         Stream_Element'Val (Integer (Value / 300.0))
      );
   end Write;

   procedure Write
             (  Stream   : access Root_Stream_Type'Class;
                Schedule : Day_Schedule
             )  is
      Data    : Stream_Element_Array (0..26);
      Pointer : Stream_Element_Offset := Data'First;
   begin
      Put (Data, Pointer, Schedule);
      Write (Stream.all, Data (Data'First..Pointer - 1));
   end Write;

   procedure Write
             (  Stream     : access Root_Stream_Type'Class;
                Parameters : Device_Parameters
             )  is
   begin
      Write (Stream, Parameters.Kind_Of);
      Stream_Element'Write
      (  Stream,
         Stream_Element (Parameters.Name_Length)
      );
      Write (Stream, Parameters.Room);
      Write (Stream, Parameters.Address);
      String'Write (Stream, Parameters.Serial_No);
      String'Write (Stream, Parameters.Name);
      case Parameters.Kind_Of is
         when Cube | Wall_Thermostat | Shutter_Contact | Eco_Button |
              Unknown =>
            null;
         when Radiator_Thermostat | Radiator_Thermostat_Plus =>
            Write (Stream, Parameters.Comfort);
            Write (Stream, Parameters.Eco);
            Write (Stream, Parameters.Max);
            Write (Stream, Parameters.Min);
            Write (Stream, Parameters.Offset);
            Write (Stream, Parameters.Window_Open);
            Write (Stream, Parameters.Window_Time);
            Write (Stream, Parameters.Boost_Time);
            Write (Stream, Parameters.Boost_Valve);
            Write (Stream, Parameters.Decalcification);
            Write (Stream, Parameters.Max_Valve);
            Write (Stream, Parameters.Valve_Offset);
            Write (Stream, Parameters.Schedule);
      end case;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Value  : Ratio
             )  is
      Data : constant Float := Float (Value) * 255.0;
   begin
      if Data >= 255.5 then
         Stream_Element'Write (Stream, Stream_Element'Val (255));
      elsif Data <= 0.0 then
         Stream_Element'Write (Stream, Stream_Element'Val (0));
      else
         Stream_Element'Write
         (  Stream,
            Stream_Element'Val (Integer (Data))
         );
      end if;
   end Write;

   procedure Write
             (  Stream  : access Root_Stream_Type'Class;
                Address : RF_Address
             )  is
      Data : Stream_Element_Array (1..3);
   begin
      Data (1) := Stream_Element'Val (Address / 2**16);
      Data (2) := Stream_Element'Val ((Address / 2**8) mod 2**8);
      Data (3) := Stream_Element'Val (Address mod 2**8);
      Write (Stream.all, Data);
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Room   : Room_ID
             )  is
   begin
      Stream_Element'Write (Stream, Stream_Element'Val (Room));
   end Write;

   procedure Write
             (  Stream   : access Root_Stream_Type'Class;
                Schedule : Week_Schedule
             )  is
      Data    : Stream_Element_Array (1..27 * 7);
      Pointer : Stream_Element_Offset := Data'First;
   begin
      for Day in Schedule'Range loop
         Put (Data, Pointer, Schedule (Day));
      end loop;
      Write (Stream.all, Data (Data'First..Pointer - 1));
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Time   : Week_Time
             )  is
   begin
      Stream_Element'Write
      (  Stream,
         Stream_Element'Val
         (  Week_Day'Pos (Time.Day) * 2**5
         +  Integer (Time.Time / 3600.0)
      )  );
   end Write;

end GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.Stream_IO;
