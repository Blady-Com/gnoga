--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

with Ada.Strings.Wide_Unbounded;
with ZanyBlue.Text.Buffer;
with ZanyBlue.Text.Utils;

package body ZanyBlue.Text.Times is

   use Ada.Strings.Wide_Unbounded;
   use ZanyBlue.Text.Buffer;
   use ZanyBlue.Text.Utils;

   function Format_Value
     (Format_String : Wide_String;
      Data          : Time;
      TZ_Offset     : Time_Offset;
      Locale        : Locale_Type)
      return Wide_String;
   --  Apply the date/time format string which includes the Unicode.org
   --  value strings, e.g., EEEE for the full day name.

   function Compose_Values
     (Format_String : Wide_String;
      Date_String   : Wide_String;
      Time_String   : Wide_String)
      return Wide_String;
   --  Compose the date and time values into a single date-time string
   --  based on the formatting information from Unicode.org.

   function To_Month
     (Number : Month_Number)
      return Month_Type;
   --  Convert a Month_Number to the Month_Type enumeration.

   --------------------
   -- Compose_Values --
   --------------------
   --
   --  Since the values composed are already formatted, the quote characters
   --  in the format string can simply be ignored.
   --

   function Compose_Values
     (Format_String : Wide_String;
      Date_String   : Wide_String;
      Time_String   : Wide_String)
      return Wide_String
   is

      Last   : constant Positive := Format_String'Last;
      Result : Unbounded_Wide_String;
      I      : Positive          := Format_String'First;

   begin
      Debug;
      while I <= Last loop
         if I <= Last - 2 and then Format_String (I .. I + 2) = "{0}" then
            Append (Result, Time_String);
            I := I + 2;
         elsif I <= Last - 2 and then Format_String (I .. I + 2) = "{1}" then
            Append (Result, Date_String);
            I := I + 2;
         elsif Format_String (I) /= ''' then
            Append (Result, Format_String (I));
         end if;
         I := I + 1;
      end loop;
      return To_Wide_String (Result);
   end Compose_Values;

   ------------
   -- Create --
   ------------

   function Create
     (Time_Value : Time)
      return Time_Argument_Type
   is
   begin
      return Create (Time_Value, UTC_Time_Offset (Date => Time_Value));
   exception
      when Unknown_Zone_Error =>
         return Create (Time_Value, 0);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Time_Value : Time;
      TZ_Offset  : Time_Offset)
      return Time_Argument_Type
   is
   begin
      return Time_Argument_Type'(Data => Time_Value, TZ_Offset => TZ_Offset);
   end Create;

   -----------------
   -- Day_In_Week --
   -----------------
   --
   --  The following algorithm is taken from the Wikipedia entry for
   --  calculating the day of the week:
   --
   --      http://en.wikipedia.org/wiki/Calculating_the_day_of_the_week
   --

   function Day_In_Week
     (Day   : Day_Number;
      Month : Month_Number;
      Year  : Year_Number)
      return Day_Type
   is

      subtype Leap_Year_P is Boolean;
      subtype Day_Number_Type is Natural range 0 .. 6;
      type Month_Map_Type is
        array (Month_Number, Leap_Year_P) of Day_Number_Type;
      type Num_To_Day_Type is array (Day_Number_Type) of Day_Type;
      Month_Map : constant Month_Map_Type :=
        (1  => (True => 6, False => 0), 2 => (True => 2, False => 3),
         3  => (others => 3), 4 => (others => 6), 5 => (others => 1),
         6  => (others => 4), 7 => (others => 6), 8 => (others => 2),
         9  => (others => 5), 10 => (others => 0), 11 => (others => 3),
         12 => (others => 5));
      Num_To_Day : constant Num_To_Day_Type :=
        (0 => Sun, 1 => Mon, 2 => Tue, 3 => Wed, 4 => Thu, 5 => Fri, 6 => Sat);
      Is_Leap : constant Leap_Year_P :=
        (Year rem 4 = 0 and then Year rem 100 /= 0) or else Year rem 400 = 0;
      CC   : constant Day_Number_Type := 2 * (3 - ((Year / 100) mod 4));
      YY   : constant Natural         := Year mod 100;
      YY_4 : constant Natural         := YY / 4;
      MM   : constant Day_Number_Type := Month_Map (Month, Is_Leap);
      Sum  : constant Natural         := CC + YY + YY_4 + MM + Natural (Day);

   begin
      return Num_To_Day (Sum mod 7);
   end Day_In_Week;

   ------------
   -- Format --
   ------------

   overriding function Format
     (Value     : Time_Argument_Type;
      Type_Name : Wide_String;
      Template  : Wide_String;
      Locale    : Locale_Type)
      return Wide_String
   is

      T : constant Time        := Value.Data;
      Z : constant Time_Offset := Value.TZ_Offset;

   begin
      if Type_Name = "time" then
         if Template = "full" then
            return Format_Value (Time_Format (Locale, Full), T, Z, Locale);
         elsif Template = "long" then
            return Format_Value (Time_Format (Locale, Long), T, Z, Locale);
         elsif Template = "medium" then
            return Format_Value (Time_Format (Locale, Medium), T, Z, Locale);
         elsif Template = "short" or else Template'Length = 0 then
            return Format_Value (Time_Format (Locale, Short), T, Z, Locale);
         end if;
      elsif Type_Name = "date" then
         if Template = "full" then
            return Format_Value (Date_Format (Locale, Full), T, Z, Locale);
         elsif Template = "long" then
            return Format_Value (Date_Format (Locale, Long), T, Z, Locale);
         elsif Template = "medium" then
            return Format_Value (Date_Format (Locale, Medium), T, Z, Locale);
         elsif Template = "short" or else Template'Length = 0 then
            return Format_Value (Date_Format (Locale, Short), T, Z, Locale);
         end if;
      elsif Type_Name = "datetime" or Type_Name = "" then
         if Template = "full" then
            return
              Compose_Values
                (Date_Time_Format (Locale, Full),
                 Format_Value (Date_Format (Locale, Full), T, Z, Locale),
                 Format_Value (Time_Format (Locale, Full), T, Z, Locale));
         elsif Template = "long" then
            return
              Compose_Values
                (Date_Time_Format (Locale, Long),
                 Format_Value (Date_Format (Locale, Long), T, Z, Locale),
                 Format_Value (Time_Format (Locale, Long), T, Z, Locale));
         elsif Template = "medium" then
            return
              Compose_Values
                (Date_Time_Format (Locale, Medium),
                 Format_Value (Date_Format (Locale, Medium), T, Z, Locale),
                 Format_Value (Time_Format (Locale, Medium), T, Z, Locale));
         elsif Template = "short" or else Template'Length = 0 then
            return
              Compose_Values
                (Date_Time_Format (Locale, Short),
                 Format_Value (Date_Format (Locale, Short), T, Z, Locale),
                 Format_Value (Time_Format (Locale, Short), T, Z, Locale));
         end if;
      end if;
      return Format_Value (Template, T, Z, Locale);
   end Format;

   ------------------
   -- Format_Value --
   ------------------

   function Format_Value
     (Format_String : Wide_String;
      Data          : Time;
      TZ_Offset     : Time_Offset;
      Locale        : Locale_Type)
      return Wide_String
   is

      Quote_Ch : constant Wide_Character := ''';
      Year     : Year_Number;
      Month    : Month_Number;
      Day      : Day_Number;
      DSeconds : Day_Duration;
      Hours    : Hour_Type;
      Minutes  : Minute_Type;
      Seconds  : Integer range 0 .. 60;
      Period   : Day_Period_Type;
      Position : Natural                 := Format_String'First;
      Buffer   : Buffer_Type;
      Quoted   : Boolean                 := False;
      Ch       : Wide_Character;

   begin
      Split (Data, Year, Month, Day, DSeconds);
      Seconds := Integer (DSeconds) rem 60;
      Minutes := (Integer (DSeconds) / 60) rem 60;
      Hours   := Integer (DSeconds) / 3_600;
      Period  := Day_Period_For_Time (Locale, Hours, Minutes, Seconds);
      Interpret_Format :
      loop
         exit Interpret_Format when Position > Format_String'Last;
         Ch       := Format_String (Position);
         Position := Position + 1;
         if Quoted then
            if Ch = Quote_Ch then
               Quoted := False;
            else
               Add (Buffer, Ch);
            end if;
         else
            case Ch is
               when 'a' =>
                  Add (Buffer, Day_Period_Name (Locale, Period));
               when 'd' =>      --  dd
                  if Starts_With (Format_String, Position, "d") then
                     Accumulate (Buffer, Integer (Day), Locale, Width => 2);
                     Position := Position + 1;
                  else
                     Accumulate (Buffer, Integer (Day), Locale);
                  end if;
               when 'E' =>      --  EEEE
                  if Starts_With (Format_String, Position, "EEE") then
                     Add
                       (Buffer,
                        Full_Day_Name
                          (Locale, Day_In_Week (Day, Month, Year)));
                     Position := Position + 3;
                  elsif Starts_With (Format_String, Position, "EE") then
                     Add
                       (Buffer,
                        Short_Day_Name
                          (Locale, Day_In_Week (Day, Month, Year)));
                     Position := Position + 2;
                  else
                     Add (Buffer, Ch);
                  end if;
               when 'G' =>
               --  Only supports CE
                  Add (Buffer, Era_Name (Locale, CE));
               when 'h' =>
                  if Hours > 12 then
                     Accumulate (Buffer, Hours rem 12, Locale);
                  else
                     Accumulate (Buffer, Hours, Locale);
                  end if;
               when 'H' =>      --  HH
                  if Starts_With (Format_String, Position, "H") then
                     Accumulate (Buffer, Hours, Locale, Width => 2);
                     Position := Position + 1;
                  else
                     Accumulate (Buffer, Hours, Locale);
                  end if;
               when 'm' =>      --  mm
                  if Starts_With (Format_String, Position, "m") then
                     Accumulate (Buffer, Minutes, Locale, Width => 2);
                     Position := Position + 1;
                  else
                     Accumulate (Buffer, Minutes, Locale);
                  end if;
               when 'M' =>      --  MM MMM MMMM
                  if Starts_With (Format_String, Position, "MMM") then
                     Add (Buffer, Full_Month_Name (Locale, To_Month (Month)));
                     Position := Position + 3;
                  elsif Starts_With (Format_String, Position, "MM") then
                     Add (Buffer, Short_Month_Name (Locale, To_Month (Month)));
                     Position := Position + 2;
                  elsif Starts_With (Format_String, Position, "M") then
                     Accumulate (Buffer, Integer (Month), Locale, Width => 2);
                     Position := Position + 1;
                  else
                     Accumulate (Buffer, Integer (Month), Locale);
                  end if;
               when 's' =>      --  ss
                  if Starts_With (Format_String, Position, "s") then
                     Accumulate (Buffer, Seconds, Locale, Width => 2);
                     Position := Position + 1;
                  else
                     Accumulate (Buffer, Seconds, Locale);
                  end if;
               when 'y' =>      --  yy yyyy
                  if Starts_With (Format_String, Position, "yyy") then
                     Accumulate (Buffer, Integer (Year), Locale, Width => 4);
                     Position := Position + 3;
                  elsif Starts_With (Format_String, Position, "y") then
                     Accumulate
                       (Buffer, Integer (Year) rem 100, Locale, Width => 2);
                     Position := Position + 1;
                  else
                     Accumulate (Buffer, Integer (Year), Locale);
                  end if;
               when 'z' =>      --  zzzz
                  if Starts_With (Format_String, Position, "zzz") then
                  --  'z' and 'zzzz' are the same here.
                     Position := Position + 3;
                  end if;
                  if TZ_Offset < 0 then
                     Add (Buffer, '-');
                  else
                     Add (Buffer, '+');
                  end if;
                  Accumulate
                    (Buffer, (abs Integer (TZ_Offset)) / 60, Locale,
                     Width => 2);
                  Accumulate
                    (Buffer, (abs Integer (TZ_Offset)) mod 60, Locale,
                     Width => 2);
               when Quote_Ch =>
                  Quoted := True;
               when others =>
                  Add (Buffer, Ch);
            end case;
         end if;
      end loop Interpret_Format;
      return To_String (Buffer);
   end Format_Value;

   --------------
   -- To_Month --
   --------------

   function To_Month
     (Number : Month_Number)
      return Month_Type
   is
   begin
      return Month_Type'Val (Month_Type'Pos (Jan) + Number - 1);
   end To_Month;

end ZanyBlue.Text.Times;
