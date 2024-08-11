--                                                                    --
--  package Strings_Edit.ISO_8601   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2019       --
--                                                                    --
--                                Last revision :  08:30 04 Aug 2022  --
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

with Ada.Calendar.Time_Zones;  use Ada.Calendar.Time_Zones;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit.Fields;      use Strings_Edit.Fields;
with Strings_Edit.Floats;      use Strings_Edit.Floats;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

with Ada.Calendar.Formatting;

package body Strings_Edit.ISO_8601 is

   Day_Length   : constant :=        24.0 * 60.0 * 60.0;
   Month_Length : constant := 365.0 * 2.0 * 60.0 * 60.0;

   function Get_Hour (Source : String) return Integer is
   begin
      return Value (Source => Source, First => 0, Last => 24);
   exception
      when Constraint_Error | End_Error =>
         raise Data_Error;
   end Get_Hour;

   function Get_Minute (Source : String) return Integer is
   begin
      return Value (Source => Source, First => 0, Last => 59);
   exception
      when Constraint_Error | End_Error =>
         raise Data_Error;
   end Get_Minute;

--  function Get_Second (Source : String) return Integer is
--  begin
--     return Value (Source => Source, First => 0, Last => 59);
--  exception
--     when Constraint_Error | End_Error =>
--        raise Data_Error;
--  end Get_Second;
  
   procedure Get_Second
             (  Source  : String;
                Pointer : in out Integer;
                Seconds : out Day_Duration
             )  is
      Index  : Integer := Pointer;
      Result : Float;
   begin
      while Index <= Source'Last loop
         case Source (Index) is
            when ',' | '.' | '0'..'9' =>
               Index := Index + 1;
            when others =>
               exit;
         end case;
      end loop;
      if Index = Pointer then
         Seconds := 0.0;
         return;
      end if;
      declare
         Fraction : String := Source (Pointer..Index - 1);
      begin
         for This in Fraction'Range loop
            if Fraction (This) = ',' then
               Fraction (This) := '.';
            end if;
         end loop;
         Result := Value
                   (  Source => Fraction,
                      First  => 0.0,
                      Last   => Float'Pred (60.0)
                   );
      exception
         when End_Error =>
            raise Data_Error;
      end;
      begin
         Seconds := Day_Duration (Result);
      exception
         when Constraint_Error =>
            Seconds := Day_Duration'Last;
      end;
      Pointer := Index;
   end Get_Second;

   procedure Get_Time
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Day_Duration
             )  is
      Hour    : Integer;
      Minute  : Integer := 0;
      Seconds : Duration;
   begin
      if Pointer > Source'Last then
         Value := 0.0;
         return;
      end if;
      case Source (Pointer) is
         when '0'..'9' =>
            null;
         when others =>
            raise Data_Error;
      end case;
      Hour    := Get_Hour (Source (Pointer..Pointer + 1));
      Pointer := Pointer + 2;
      if Pointer > Source'Last then
         Value := Duration (Hour * 60) * 60.0;
         return;
      end if;
      case Source (Pointer) is
         when ':' =>
            Pointer := Pointer + 1;
         when '0'..'9' =>
            null;
         when others =>
            Value := Duration (Hour * 60) * 60.0;
            return;
      end case;
      if Pointer > Source'Last - 1 then
          raise Data_Error;
      end if;
      Minute  := Get_Minute (Source (Pointer..Pointer + 1));
      Pointer := Pointer + 2;
      if Pointer > Source'Last then
         Value := Duration (Hour * 60 + Minute) * 60.0;
         return;
      end if;
      if Source (Pointer) = ':' then
         Pointer := Pointer + 1;
      end if;
      Get_Second (Source, Pointer, Seconds);
      if Hour = 24 and then (Minute > 0 or else Seconds > 0.0) then
         raise Data_Error;
      end if;
      Value := Duration (Hour * 60 + Minute) * 60.0 + Seconds;
   end Get_Time;

   procedure Get_Zoned_Time
             (  Source  : String;
                Pointer : in out Integer;
                Year    : Year_Number;
                Month   : Month_Number;
                Day     : Day_Number;
                Seconds : Duration;
                Result  : out Time
             )  is
      Hour   : Integer;
      Minute : Integer := 0;
   begin
      if Pointer > Source'Last then
         Result := Ada.Calendar.Time_Of
                   (  Year    => Year,
                      Month   => Month,
                      Day     => Day,
                      Seconds => Seconds
                   );
         return;
      end if;
      case Source (Pointer) is
         when 'Z' =>
            Pointer := Pointer + 1;
            Result := Ada.Calendar.Formatting.Time_Of
                      (  Year      => Year,
                         Month     => Month,
                         Day       => Day,
                         Seconds   => Seconds,
                         Time_Zone => 0
                      );
         when '-' | '+' =>
            Pointer := Pointer + 1;
            if Pointer > Source'Last - 1 then
               raise Data_Error;
            end if;
            Hour    := Get_Hour (Source (Pointer..Pointer + 1));
            Pointer := Pointer + 2;
            if Pointer <= Source'Last then
               if Source (Pointer) = ':' then
                  Pointer := Pointer + 1;
               end if;
               if Pointer > Source'Last - 1 then
                  raise Data_Error;
               end if;
               Minute  := Get_Minute (Source (Pointer..Pointer + 1));
               Pointer := Pointer + 2;
            end if;
            Result := Ada.Calendar.Formatting.Time_Of
                      (  Year      => Year,
                         Month     => Month,
                         Day       => Day,
                         Seconds   => Seconds,
                         Time_Zone => Time_Offset (Hour * 60 + Minute)
                      );
         when others =>
            raise Data_Error;
      end case;
   end Get_Zoned_Time;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Time
             )  is
      Year    : Integer;
      Month   : Integer;
      Day     : Integer;
      Seconds : Day_Duration := 0.0;
      Index   : Integer;

      procedure Get_Suffix is
      begin
         if Index <= Source'Last then
            case Source (Index) is
               when 'T' | ' ' =>
                  Index := Index + 1;
                  Get_Time (Source, Index, Seconds);
               when others =>
                  null;
            end case;
         end if;
      end Get_Suffix;

      function Is_Terminator (Value : Character) return Boolean is
      begin
         case Value is
            when 'T' | ' ' | 'Z' | '+' | '-' =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Terminator;
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer > Source'Last + 1
         )  )  then
         raise Layout_Error;
      end if;
      Index := Pointer;
      if Index > Source'Last then
         raise End_Error;
      end if;
      case Source (Index) is
         when '-' | '+' =>
            if Index > Source'Last - 4 then
               raise Data_Error;
            end if;
            Year  := Strings_Edit.Integers.Value
                     (  Source (Pointer..Pointer + 4)
                     );
            Index := Index + 5;
         when '0'..'9' =>
            if Index > Source'Last - 3 then
               raise Data_Error;
            end if;
            Year  := Strings_Edit.Integers.Value
                     (  Source (Pointer..Pointer + 3)
                     );
            Index := Index + 4;
         when others =>
            raise End_Error;
      end case;
      if Index > Source'Last then
         Value :=
            Ada.Calendar.Time_Of
            (  Year    => Year_Number (Year),
               Month   => 1,
               Day     => 1,
               Seconds => 0.0
            );
         Pointer := Index;
         return;
      end if;
      if Source (Index) = '-' then
         Index := Index + 1;
         if Index > Source'Last then
            raise Data_Error;
         end if;
      end if;
      if Source (Index) = 'W' then -- YYYY-Www-D
         declare
            Week  : Integer;
            Start : Time;
         begin
            Index := Index + 1;
            if Index > Source'Last - 1 then
               raise Data_Error;
            end if;
            Week := Strings_Edit.Integers.Value -- Week number
                    (  Source => Source (Index..Index + 1),
                       First  => 1,
                       Last   => 53
                    );
            Index := Index + 2;
            if Index <= Source'Last then
               if Source (Index) = '-' then
                  Index := Index + 1;
               end if;
               if Index > Source'Last then
                  raise Data_Error;
               end if;
               Day := Strings_Edit.Integers.Value -- Week day
                      (  Source => Source (Index..Index),
                         First  => 1,
                         Last   => 7
                      );
               Index := Index + 1;
            else
               Day := 1;
            end if;
            Get_Suffix;
            Get_Zoned_Time
            (  Source  => Source,
               Pointer => Index,
               Year    => Year,
               Month   => 1,
               Day     => 4,
               Seconds => 0.0,
               Result  => Start
            );
            Value :=
               (  Start
               +  Duration
                  (  (Week - 1) * 7
                  +  Day
                  -  1
                  -  Ada.Calendar.Formatting.Day_Name'Pos
                     (  Ada.Calendar.Formatting.Day_Of_Week (Start)
                  )  )
               *  Day_Length
               +  Seconds
               ); 
         end;
      else
         if (  Index = Source'Last - 2
            or else
               (  Index <= Source'Last - 3
               and then
                  Is_Terminator (Source (Index + 3))
            )  )  then -- YYYY-DDD
            declare
               Start : Time;
            begin
               Day := Strings_Edit.Integers.Value
                      (  Source => Source (Index..Index + 2),
                         First  => 1,
                         Last   => 366
                      );
               Index := Index + 3;
               Get_Suffix;
               Get_Zoned_Time
               (  Source  => Source,
                  Pointer => Index,
                  Year    => Year,
                  Month   => 1,
                  Day     => 1,
                  Seconds => 0.0,
                  Result  => Start
               );
               Value :=
                  Start + Duration (Day - 1) * Day_Length + Seconds;
            end;
         elsif Pointer <= Source'Last - 1 then -- YYYY-MM-DD
            Month :=
               Strings_Edit.Integers.Value (Source (Index..Index + 1));
            Index := Index + 2;
            if Index > Source'Last then
               raise Data_Error;
            end if;
            if Source (Index) = '-' then
               Index := Index + 1;
            end if;
            if Index > Source'Last - 1 then
               raise Data_Error;
            end if;
            Day :=
               Strings_Edit.Integers.Value (Source (Index..Index + 1));
            Index := Index + 2;
            Get_Suffix;
            Get_Zoned_Time
            (  Source  => Source,
               Pointer => Index,
               Year    => Year,
               Month   => Month,
               Day     => Day,
               Seconds => Seconds,
               Result  => Value
            );
         else
            raise Data_Error;
         end if;
      end if;
      Pointer := Index;
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Duration
             )  is
      type Component is
           (None, Year, Month, Week, Day, Hour, Minute, Second);
      Data   : String (Source'Range) := Source;
      Got_It : Component := None;
      Time   : Boolean   := False;
      Result : Float     := 0.0;
      Index  : Integer;

      procedure Get is
         Value     : Float;
         Start     : constant Integer := Index;
         Translate : Boolean := False;
      begin
         while Index <= Source'Last loop
            case Source (Index) is
               when ',' =>
                  Translate := True;
               when '.' | '0'..'9' =>
                  null;
               when others =>
                  exit;
            end case;
            Index := Index + 1;
         end loop;
         if Translate then
            declare
               Text : String := Source (Start..Index - 1);
            begin
               for This in Text'Range loop
                  if Text (This) = ',' then
                     Text (This) := '.';
                  end if;
               end loop;
               Value := Strings_Edit.Floats.Value (Text);
            end;
         else
            Value :=
               Strings_Edit.Floats.Value (Source (Start..Index - 1));
         end if;
         if Index > Data'Last then
            raise Data_Error;
         end if;
         case Data (Index) is
            when 'Y' =>
               if Got_It < Year then
                  Result := 365.0 * Day_Length * Value;
                  Got_It := Year;
               else
                  raise Data_Error;
               end if;
            when 'M' =>
               if Got_It < Month then
                  Result := Result + Month_Length * Value;
                  Got_It := Month;
               elsif Got_It < Minute and Time then
                  Result := Result + 60.0 * Value;
                  Got_It := Minute;
               else
                  raise Data_Error;
               end if;
            when 'W' =>
               if Got_It < Week then
                  Result := Result + 7.0 * Day_Length * Value;
                  Got_It := Week;
               else
                  raise Data_Error;
               end if;
            when 'D' =>
               if Got_It < Day then
                  Result :=  Result + Day_Length * Value;
                  Got_It := Day;
               else
                  raise Data_Error;
               end if;
            when 'H' =>
               if Got_It < Hour and Time then
                  Result := Result + 60.0 * 60.0 * Value;
                  Got_It := Hour;
               else
                  raise Data_Error;
               end if;
            when 'S' =>
               if Got_It < Second and Time then
                  Result := Result + Value;
                  Got_It := Second;
               else
                  raise Data_Error;
               end if;
            when others =>
               raise Data_Error;
         end case;
         Index := Index + 1;
      exception
         when Data_Error =>
            raise;
         when others =>
            raise Data_Error;
      end Get;
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer > Source'Last + 1
         )  )  then
         raise Layout_Error;
      end if;
      Index := Pointer;
      for Index in Data'Range loop
         if Data (Index) = ',' then
            Data (Index) := '.';
         end if;
      end loop;
      if Index > Data'Last or else Data (Index) /= 'P' then
         raise End_Error;
      end if;
      Index := Index + 1;
      while Index <= Data'Last loop
         if Data (Index) = 'T' then
            if Time then
               raise Data_Error;
            else
               Time   := True;
               Got_It := Day;
            end if;
            Index := Index + 1;
            exit when Index > Data'Last;
         end if;
         Get;
      end loop;
      Value   := Duration (Result);
      Pointer := Index;
   end Get;

   function Image
            (  Value    : Time;
               Fraction : Second_Fraction := 0;
               Local    : Boolean := False
            )  return String is
      Buffer  : String (1..80);
      Pointer : Integer := Buffer'First;
   begin
      Put (Buffer, Pointer, Value, Fraction, Local);
      return Buffer (1..Pointer - 1);
   end Image;

   function Image
            (  Value    : Duration;
               Fraction : Second_Fraction := 0
            )  return String is
      Buffer  : String (1..80);
      Pointer : Integer := Buffer'First;
   begin
      Put (Buffer, Pointer, Value, Fraction);
      return Buffer (1..Pointer - 1);
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Time;
                Fraction    : Second_Fraction := 0;
                Local       : Boolean         := False;
                Field       : Natural         := 0;
                Justify     : Alignment       := Left;
                Fill        : Character       := ' '
             )  is
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Ada.Calendar.Formatting.Hour_Number;
      Minute     : Ada.Calendar.Formatting.Minute_Number;
      Second     : Ada.Calendar.Formatting.Second_Number;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration;

      Out_Field : constant Natural :=
         Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text : Output renames
                    Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
   begin
      if Local then
         Ada.Calendar.Formatting.Split
         (  Date       => Value,
            Year       => Year,
            Month      => Month,
            Day        => Day,
            Hour       => Hour,
            Minute     => Minute,
            Second     => Second,
            Sub_Second => Sub_Second,
            Time_Zone  => UTC_Time_Offset (Value)
         );
      else
         Ada.Calendar.Formatting.Split
         (  Date       => Value,
            Year       => Year,
            Month      => Month,
            Day        => Day,
            Hour       => Hour,
            Minute     => Minute,
            Second     => Second,
            Sub_Second => Sub_Second
         );
      end if;
      Put
      (  Destination => Text,
         Pointer     => Index,
         Value       => Integer (Year),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 4
      );
      Put (Text, Index, "-");
      Put
      (  Destination => Text,
         Pointer     => Index,
         Value       => Integer (Month),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put (Text, Index, "-");
      Put
      (  Destination => Text,
         Pointer     => Index,
         Value       => Integer (Day),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put (Text, Index, "T");
      Put
      (  Destination => Text,
         Pointer     => Index,
         Value       => Hour,
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put (Text, Index, ":");
      Put
      (  Destination => Text,
         Pointer     => Index,
         Value       => Minute,
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put (Destination, Index, ":");
      Put
      (  Destination => Text,
         Pointer     => Index,
         Value       => Second,
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      if Fraction > 0 then
         Strings_Edit.Put
         (  Destination => Text,
            Pointer     => Index,
            Value       => "."
         );
         Put
         (  Destination => Text,
            Pointer     => Index,
            Value       => Integer
                           (  Float'Floor
                              (  Float (Sub_Second) * 10.0 ** Fraction
                           )  ),
            Justify     => Strings_Edit.Right,
            Fill        => '0',
            Field       => Fraction
         );
      end if;
      Put
      (  Destination => Text,
         Pointer     => Index,
         Value       => "Z"
      );
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Duration;
                Fraction    : Second_Fraction := 0;
                Field       : Natural         := 0;
                Justify     : Alignment       := Left;
                Fill        : Character       := ' '
             )  is
      procedure Divide
                (  Value    : in out Duration;
                   Interval : Duration;
                   Result   : out Integer
                )  is
      begin
         Result := Integer (Value / Interval);
         if Duration (Result) * Interval > Value then
            Result := Result - 1;
         end if;
         Value := Value - Duration (Result) * Interval;
      end Divide;

      Out_Field : constant Natural :=
         Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text : Output renames
                    Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer  := Pointer;
      Count : Duration := Value;
      Lead  : Integer;
   begin
      Put (Text, Index, "P");
      Divide (Count, 365.0 * Day_Length, Lead);
      if Lead > 0 then
         Put (Text, Index, Lead);
         Put (Text, Index, "Y");
      end if;
      Divide (Count, Month_Length, Lead);
      if Lead > 0 then
         Put (Text, Index, Lead);
         Put (Text, Index, "M");
      end if;
      Divide (Count, Day_Length, Lead);
      if Lead > 0 then
         Put (Text, Index, Lead);
         Put (Text, Index, "D");
      end if;
      Put (Text, Index, "T");
      Divide (Count, 60.0 * 60.0, Lead);
      if Lead > 0 then
         Put (Text, Index, Lead);
         Put (Text, Index, "H");
      end if;
      Divide (Count, 60.0, Lead);
      if Lead > 0 then
         Put (Text, Index, Lead);
         Put (Text, Index, "M");
      end if;
      Divide (Count, 1.0, Lead);
      if Fraction > 0 then
         Put (Text, Index, Lead);
         Strings_Edit.Put
         (  Destination => Text,
            Pointer     => Index,
            Value       => "."
         );
         Put
         (  Destination => Text,
            Pointer     => Index,
            Value       => Integer
                           (  Float'Floor
                              (  Float (Count) * 10.0 ** Fraction
                           )  ),
            Justify     => Strings_Edit.Right,
            Fill        => '0',
            Field       => Fraction
         );
         Put (Text, Index, "S");
      else
         if Index - Pointer <= 2 or else Lead > 0 then
            Put (Text, Index, Lead);
            Put (Text, Index, "S");
         end if;
      end if;
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

   function Value (Source : String) return Time is
      Pointer : Integer := Source'First;
      Result  : Time;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Result);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer <= Source'Last then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   function Value (Source : String) return Duration is
      Pointer : Integer := Source'First;
      Result  : Duration;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Result);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer <= Source'Last then
         raise Data_Error;
      end if;
      return Result;
   end Value;

end Strings_Edit.ISO_8601;
