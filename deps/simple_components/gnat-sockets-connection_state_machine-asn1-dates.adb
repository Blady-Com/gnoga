--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Dates                                  Spring, 2019       --
--  Implementation                                                    --
--                                Last revision :  13:37 03 Aug 2019  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Floats;    use Strings_Edit.Floats;

with Ada.Calendar.Formatting;
with Strings_Edit.ISO_8601;

with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Dates is
   use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;

   Invalid_Time : constant String := "Invalid ASN.1 time";

   procedure Explicit_Put_Date is new Generic_Put (Time, Put_Date);

   procedure Encode
             (  Item    : Date_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Put_Date (Data, Pointer, Date_Tag, Item.Value);
   end Encode;

   procedure Explicit_Put_Date_Time is
      new Generic_Put (Time, Put_Date_Time);

   procedure Encode
             (  Item    : Date_Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Put_Date_Time (Data, Pointer, Date_Time_Tag, Item.Value);
   end Encode;

   procedure Explicit_Put_Duration is
      new Generic_Put (Duration, Put_Duration);

   procedure Encode
             (  Item    : Duration_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Put_Duration (Data, Pointer, Duration_Tag, Item.Value);
   end Encode;

   procedure Explicit_Put is new Generic_Put (Time);

   procedure Encode
             (  Item    : Generalized_Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Put (Data, Pointer, Generalized_Time_Tag, Item.Value);
   end Encode;

   procedure Encode
             (  Item    : Implicit_Date_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put_Date (Data, Pointer, Item.Value);
   end Encode;

   procedure Encode
             (  Item    : Implicit_Date_Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put_Date_Time (Data, Pointer, Item.Value);
   end Encode;

   procedure Encode
             (  Item    : Implicit_Duration_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put_Duration (Data, Pointer, Item.Value);
   end Encode;

   procedure Encode
             (  Item    : Implicit_Generalized_Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put (Data, Pointer, Item.Value);
   end Encode;

   procedure Encode
             (  Item    : Implicit_Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put (Data, Pointer, Item.Value);
   end Encode;

   procedure Encode
             (  Item    : Implicit_UTC_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put_UTC (Data, Pointer, Item.Value);
   end Encode;

   procedure Encode
             (  Item    : Implicit_Time_Of_Day_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put_Time_Of_Day (Data, Pointer, Item.Value);
   end Encode;

   procedure Explicit_Put_Time is new Generic_Put (Time, Put_Time);

   procedure Encode
             (  Item    : Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Put_Time (Data, Pointer, Time_Tag, Item.Value);
   end Encode;

   procedure Explicit_Put_Time_Of_Day is
      new Generic_Put (Day_Duration, Put_Time_Of_Day);

   procedure Encode
             (  Item    : Time_Of_Day_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Put_Time_Of_Day
      (  Data,
         Pointer,
         Time_Of_Day_Tag,
         Item.Value
      );
   end Encode;

   procedure Explicit_Put_UTC is new Generic_Put (Time, Put_UTC);

   procedure Encode
             (  Item    : UTC_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Put_UTC (Data, Pointer, UTC_Time_Tag, Item.Value);
   end Encode;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Date_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Time_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Date_Time_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Time_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Duration_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Duration_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Generalized_Time_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Time_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Date_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Time_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Date_Time_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Time_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Duration_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Duration_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Generalized_Time_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Time_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Time_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Time_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Time_Of_Day_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Duration_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_UTC_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Time_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Time_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Time_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Time_Of_Day_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Duration_Data_Item (Item));
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : UTC_Data_Item
             )  is
   begin
      Enumerate (Stream, Public_Time_Data_Item (Item));
   end Enumerate;

   procedure Feed
             (  Item    : in out Date_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Get_Date (Get_Value (Item.Text));
   end Feed;

   procedure Feed
             (  Item    : in out Date_Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Get_Date_Time (Get_Value (Item.Text));
   end Feed;

   procedure Feed
             (  Item    : in out Duration_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Strings_Edit.ISO_8601.Value (Get_Value (Item.Text));
   end Feed;

   procedure Feed
             (  Item    : in out Generalized_Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Get (Get_Value (Item.Text), False);
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Date_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Get_Date (Get_Value (Item.Text));
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Date_Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Get_Date_Time (Get_Value (Item.Text));
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Duration_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Strings_Edit.ISO_8601.Value (Get_Value (Item.Text));
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Generalized_Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Get (Get_Value (Item.Text), False);
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Strings_Edit.ISO_8601.Value (Get_Value (Item.Text));
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Time_Of_Day_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Get_Time_Of_Day (Get_Value (Item.Text));
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_UTC_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Get (Get_Value (Item.Text), True);
   end Feed;

   procedure Feed
             (  Item    : in out Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Strings_Edit.ISO_8601.Value (Get_Value (Item.Text));
   end Feed;

   procedure Feed
             (  Item    : in out Time_Of_Day_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Get_Time_Of_Day (Get_Value (Item.Text));
   end Feed;

   procedure Feed
             (  Item    : in out UTC_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Item    => Item.Text,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
      if State /= 0 then
         return;
      end if;
      Item.Value := Get (Get_Value (Item.Text), True);
   end Feed;

   function Get_Hour (Source : String) return Integer is
   begin
      return Value (Source => Source, First => 0, Last => 24);
   end Get_Hour;

   function Get_Minute (Source : String) return Integer is
   begin
      return Value (Source => Source, First => 0, Last => 59);
   end Get_Minute;

   function Get_Second (Source : String) return Integer is
   begin
      return Value (Source => Source, First => 0, Last => 59);
   end Get_Second;

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

   function Get (Source : String; Short : Boolean) return Time is
      use Ada.Calendar.Time_Zones;
      Year    : Integer;
      Month   : Integer;
      Day     : Integer;
      Hour    : Integer;
      Minute  : Integer      := 0;
      Seconds : Day_Duration := 0.0;
      Index   : Integer      := Source'First;
   begin
      if Short then
         if Source'Length < 2+2+2 then
            raise Data_Error;
         end if;
      else
         if Source'Length < 4+2+2 then
            raise Data_Error;
         end if;
      end if;
      if Short then
         if Index > Source'Last - 1 then
            raise Data_Error;
         end if;
         Year := Value (Source (Index..Index + 1));
         if Year > 50 then
            Year := Year + 1900;
         else
            Year := Year + 2000;
         end if;
         Index := Index + 2;
      else
         if Index > Source'Last - 3 then
            raise Data_Error;
         end if;
         Year  := Value (Source (Index..Index + 3));
         Index := Index + 4;
      end if;
      if Index > Source'Last - 1 then
         raise Data_Error;
      end if;
      Month := Value (Source (Index..Index + 1));
      Index := Index + 2;
      if Index > Source'Last - 1 then
         raise Data_Error;
      end if;
      Day   := Value (Source (Index..Index + 1));
      Index := Index + 2;
      if Index > Source'Last - 1 then
         raise Data_Error;
      end if;
      Hour  := Get_Hour (Source (Index..Index + 1));
      Index := Index + 2;
      if Index <= Source'Last then
         case Source (Index) is
            when '0'..'9' =>
               if Index > Source'Last - 1 then
                  raise Data_Error;
               end if;
               Minute := Value (Source (Index..Index + 1));
               Index  := Index + 2;
               if Index <= Source'Last then
                  case Source (Index) is
                     when '0'..'9' =>
                        if Short then
                           if Index > Source'Last - 1 then
                              raise Data_Error;
                           end if;
                           Seconds :=
                              Day_Duration
                              (  Integer'
                                 (  Value (Source (Index..Index + 1))
                              )  );
                           Index := Index + 2;
                        else
                           Get_Second (Source, Index, Seconds);
                        end if;
                     when others =>
                        null;
                  end case;
               end if;
               Seconds := Seconds + Duration (Minute * 60);
            when others =>
               null;
         end case;
      end if;
      if Hour = 24 and then Seconds /= 0.0 then
         raise Data_Error;
      end if;
      return Get_Zoned_Time
             (  Source  => Source (Index..Source'Last),
                Year    => Year,
                Month   => Month,
                Day     => Day,
                Seconds => Duration (Hour * 60) * 60.0 + Seconds
             );
   exception
      when others =>
         Raise_Exception (Data_Error'Identity, Invalid_Time);
   end Get;

   function Get (Source : String) return Day_Duration is
      Result : Integer;
      Index  : Integer := Source'First;
      Minute : Integer := 0;
   begin
      if Source'Length < 2+2+2 then
         raise Data_Error;
      end if;
      Result := Get_Hour (Source (Index..Index + 1)) * 3_600;
      Index  := Index + 2;
      if Source (Index) = ':' then
         Index := Index + 1;
      end if;
      Result := Result + Get_Minute (Source (Index..Index + 1)) * 60;
      Index  := Index + 2;
      if Source (Index) = ':' then
         Index := Index + 1;
      end if;
      if Index > Source'Last - 1 then
         raise Data_Error;
      end if;
      Result := Result + Get_Second (Source (Index..Index + 1));
      Index  := Index + 1;
      if Index /= Source'Last then
         raise Data_Error;
      end if;
      return Duration (Result);
   exception
      when others =>
         Raise_Exception (Data_Error'Identity, Invalid_Time);
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Time
             )  is
   begin
      if Data'Last - Pointer < Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      elsif Length > Maximum_Time_Length then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      Value :=
         Get (To_String (Data (Pointer..Pointer + Length - 1)), False);
      Pointer := Pointer + Length;
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Duration
             )  is
   begin
      if Data'Last - Pointer < Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      elsif Length > 8 then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      Value   := Get (To_String (Data (Pointer..Pointer + Length - 1)));
      Pointer := Pointer + Length;
   end Get;

   function Get_ASN1_Type (Item : Date_Data_Item) return ASN1_Type is
   begin
       return Date_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Date_Time_Data_Item
            )  return ASN1_Type is
   begin
       return Date_Time_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Duration_Data_Item
            )  return ASN1_Type is
   begin
       return Duration_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Generalized_Time_Data_Item
            )  return ASN1_Type is
   begin
       return Generalized_Time_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_Date_Data_Item
            )  return ASN1_Type is
   begin
       return Date_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_Date_Time_Data_Item
            )  return ASN1_Type is
   begin
       return Date_Time_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_Duration_Data_Item
            )  return ASN1_Type is
   begin
       return Duration_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_Generalized_Time_Data_Item
            )  return ASN1_Type is
   begin
       return Generalized_Time_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_Time_Data_Item
            )  return ASN1_Type is
   begin
       return Time_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_Time_Of_Day_Data_Item
            )  return ASN1_Type is
   begin
       return Time_Of_Day_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_UTC_Data_Item
            )  return ASN1_Type is
   begin
       return UTC_Time_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Time_Data_Item
            )  return ASN1_Type is
   begin
       return Time_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Time_Of_Day_Data_Item
            )  return ASN1_Type is
   begin
       return Time_Of_Day_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : UTC_Data_Item
            )  return ASN1_Type is
   begin
       return UTC_Time_Tag;
   end Get_ASN1_Type;

   procedure Get_Date
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Time
             )  is
   begin
      if Data'Last - Pointer < Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      elsif Length /= 10 then
         Raise_Exception (Constraint_Error'Identity, Invalid_Length);
      end if;
      Value :=
         Get_Date (To_String (Data (Pointer..Pointer + Length - 1)));
      Pointer := Pointer + Length;
   end Get_Date;

   function Get_Date (Source : String) return Time is
      use Ada.Calendar.Time_Zones;
      Year  : Integer;
      Month : Integer;
      Day   : Integer;
      Index : Integer := Source'First;
   begin
      if Source'Length /= 10 then
         raise Data_Error;
      end if;
      Year  := Value (Source (Index..Index + 3));
      Index := Index + 4;
      if Source (Index) /= '-' then
         raise Data_Error;
      end if;
      Index := Index + 1;
      Month := Value (Source (Index..Index + 1));
      Index := Index + 2;
      if Source (Index) /= '-' then
         raise Data_Error;
      end if;
      Index := Index + 1;
      Day   := Value (Source (Index..Index + 1));
      return Ada.Calendar.Time_Of
             (  Year      => Year_Number  (Year),
                Month     => Month_Number (Month),
                Day       => Day_Number   (Day),
                Seconds   => 0.0
             );
   exception
      when others =>
         Raise_Exception (Data_Error'Identity, Invalid_Time);
   end Get_Date;

   procedure Get_Date_Time
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Time
             )  is
   begin
      if Data'Last - Pointer < Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      elsif Length /= 19 then
         Raise_Exception (Constraint_Error'Identity, Invalid_Length);
      end if;
      Value :=
         Get_Date_Time
         (  To_String (Data (Pointer..Pointer + Length - 1))
         );
      Pointer := Pointer + Length;
   end Get_Date_Time;

   function Get_Date_Time (Source : String) return Time is
      use Ada.Calendar.Time_Zones;
      Year    : Integer;
      Month   : Integer;
      Day     : Integer;
      Hour    : Integer;
      Minute  : Integer;
      Second  : Integer;
      Index   : Integer := Source'First;
   begin
      if Source'Length /= 19 then
         raise Data_Error;
      end if;
      Year  := Value (Source (Index..Index + 3));
      Index := Index + 4;
      if Source (Index) /= '-' then
         raise Data_Error;
      end if;
      Index := Index + 1;
      Month := Value (Source (Index..Index + 1));
      Index := Index + 2;
      if Source (Index) /= '-' then
         raise Data_Error;
      end if;
      Index := Index + 1;
      Day   := Value (Source (Index..Index + 1));
      Index := Index + 2;
      if Source (Index) /= 'T' then
         raise Data_Error;
      end if;
      Index := Index + 1;
      Hour  := Get_Hour (Source (Index..Index + 1));
      Index := Index + 2;
      if Source (Index) /= ':' then
         raise Data_Error;
      end if;
      Index  := Index + 1;
      Minute := Get_Minute (Source (Index..Index + 1));
      Index  := Index + 2;
      if Source (Index) /= ':' then
         raise Data_Error;
      end if;
      Index  := Index + 1;
      Second := Get_Second (Source (Index..Index + 1));
      return Ada.Calendar.Time_Of
             (  Year      => Year_Number  (Year),
                Month     => Month_Number (Month),
                Day       => Day_Number   (Day),
                Seconds   => Duration
                             (  Float (Hour) * 3_600.0
                             +  Float (Minute * 60)
                             +  Float (Second)
             )               );
   exception
      when others =>
         Raise_Exception (Data_Error'Identity, Invalid_Time);
   end Get_Date_Time;

   procedure Get_Duration
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Duration
             )  is
   begin
      if Data'Last - Pointer < Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      elsif Length > Maximum_Time_Length then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      Value :=
         Strings_Edit.ISO_8601.Value
         (  To_String (Data (Pointer..Pointer + Length - 1))
         );
      Pointer := Pointer + Length;
   end Get_Duration;

   function Get_Duration
            (  Item : Public_Duration_Data_Item
            )  return Duration is
   begin
      return Item.Value;
   end Get_Duration;

   function Get_Time
            (  Item : Public_Time_Data_Item
            )  return Time is
   begin
      return Item.Value;
   end Get_Time;

   procedure Get_Time
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Time
             )  is
   begin
      if Data'Last - Pointer < Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      elsif Length > Maximum_Time_Length then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      Value := Strings_Edit.ISO_8601.Value
               (  To_String (Data (Pointer..Pointer + Length - 1))
               );
      Pointer := Pointer + Length;
   end Get_Time;

   function Get_Time_Of_Day (Source : String) return Day_Duration is
      Result : Day_Duration;
      Index  : Integer := Source'First;
   begin
      if Source'Length /= 8 then
         raise Data_Error;
      end if;
      Result :=
         Day_Duration (Integer'(Value (Source (Index..Index + 1))));
      Index := Index + 2;
      if Source (Index) /= ':' then
         raise Data_Error;
      end if;
      Index := Index + 1;
      Result :=
         (  Result * 60.0
         +  Day_Duration (Integer'(Value (Source (Index..Index + 1))))
         );
      Index := Index + 2;
      if Source (Index) /= ':' then
         raise Data_Error;
      end if;
      Index := Index + 1;
      Result :=
         (  Result * 60.0
         +  Day_Duration (Integer'(Value (Source (Index..Index + 1))))
         );
      return Result;
   exception
      when others =>
         Raise_Exception (Data_Error'Identity, Invalid_Time);
   end Get_Time_Of_Day;

   procedure Get_Time_Of_Day
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Day_Duration
             )  is
   begin
      if Data'Last - Pointer < Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      elsif Length /= 8 then
         Raise_Exception (Constraint_Error'Identity, Invalid_Length);
      end if;
      Value :=
         Get_Time_Of_Day
         (  To_String (Data (Pointer..Pointer + Length - 1))
         );
      Pointer := Pointer + Length;
   end Get_Time_Of_Day;

   procedure Get_UTC
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Time
             )  is
   begin
      if Data'Last - Pointer < Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      elsif Length > Maximum_Time_Length then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      Value :=
         Get (To_String (Data (Pointer..Pointer + Length - 1)), True);
      Pointer := Pointer + Length;
   end Get_UTC;

   function Get_Zoned_Time
            (  Source  : String;
               Year    : Integer;
               Month   : Integer;
               Day     : Integer;
               Seconds : Duration
            )  return Time is
      use Ada.Calendar.Time_Zones;
      Hour   : Integer;
      Minute : Integer := 0;
      Index  : Integer := Source'First;
   begin
      if Index > Source'Last then
         return Ada.Calendar.Time_Of
                (  Year    => Year_Number  (Year),
                   Month   => Month_Number (Month),
                   Day     => Day_Number   (Day),
                   Seconds => Seconds
                );

      end if;
      if Source (Index) = 'Z' then
         Index := Index + 1;
         if Index <= Source'Last then
            raise Data_Error;
         end if;
         return Ada.Calendar.Formatting.Time_Of
                (  Year      => Year_Number  (Year),
                   Month     => Month_Number (Month),
                   Day       => Day_Number   (Day),
                   Seconds   => Seconds,
                   Time_Zone => 0
                );
      end if;
      case Source (Index) is
         when '-' | '+' =>
            Index := Index + 1;
            if Index > Source'Last - 1 then
               raise Data_Error;
            end if;
         when others =>
            raise Data_Error;
      end case;
      if Index > Source'Last - 1 then
         raise Data_Error;
      end if;
      Hour  := Get_Hour (Source (Index..Index + 1));
      Index := Index + 2;
      if Index <= Source'Last then
         if Source (Index) = ':' then
            Index := Index + 1;
            if Index > Source'Last then
               raise Data_Error;
            end if;
         end if;
         if Index > Source'Last - 1 then
            raise Data_Error;
         end if;
         Minute := Get_Minute (Source (Index..Index + 1));
         Index := Index + 2;
         if Index <= Source'Last then
            raise Data_Error;
         end if;
      end if;
      return Ada.Calendar.Formatting.Time_Of
             (  Year      => Year_Number  (Year),
                Month     => Month_Number (Month),
                Day       => Day_Number   (Day),
                Seconds   => Seconds,
                Time_Zone => Time_Offset (Hour * 60 + Minute)
             );
   end Get_Zoned_Time;

   function Is_Implicit (Item : Date_Data_Item) return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Is_Implicit (Item : Date_Time_Data_Item) return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Is_Implicit (Item : Duration_Data_Item) return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Is_Implicit (Item : Generalized_Time_Data_Item)
      return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Is_Implicit (Item : Implicit_Date_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Implicit_Date_Time_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Implicit_Duration_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Implicit_Generalized_Time_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Implicit_Time_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Implicit_Time_Of_Day_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Implicit_UTC_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Time_Data_Item) return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Is_Implicit (Item : Time_Of_Day_Data_Item) return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Is_Implicit (Item : UTC_Data_Item) return Boolean is
   begin
      return False;
   end Is_Implicit;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Short       : Boolean;
                Value       : Time
             )  is
      use Strings_Edit;
      Year     : Year_Number;
      Month    : Month_Number;
      Day      : Day_Number;
      Hour     : Ada.Calendar.Formatting.Hour_Number;
      Minute   : Ada.Calendar.Formatting.Minute_Number;
      Second   : Ada.Calendar.Formatting.Second_Number;
      Fraction : Ada.Calendar.Formatting.Second_Duration;
      Index    : Integer := Pointer;
   begin
      Ada.Calendar.Formatting.Split
      (  Date       => Value,
         Year       => Year,
         Month      => Month,
         Day        => Day,
         Hour       => Hour,
         Minute     => Minute,
         Second     => Second,
         Sub_Second => Fraction
      );
      if Short then
         Put
         (  Destination => Destination,
            Pointer     => Index,
            Value       => Integer (Year) mod 100,
            Justify     => Strings_Edit.Right,
            Fill        => '0',
            Field       => 2
         );
      else
         Put
         (  Destination => Destination,
            Pointer     => Index,
            Value       => Integer (Year),
            Justify     => Strings_Edit.Right,
            Fill        => '0',
            Field       => 4
         );
      end if;
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Month),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Day),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Hour,
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Minute,
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Second,
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      if not Short and then Fraction > 0.0 then
         Strings_Edit.Put
         (  Destination => Destination,
            Pointer     => Index,
            Value       => "."
         );
         Put
         (  Destination => Destination,
            Pointer     => Index,
            Value       => Second,
            Justify     => Strings_Edit.Right,
            Fill        => '0',
            Field       => 6
         );
      end if;
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => "Z"
      );
      Pointer := Index;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Day_Duration
             )  is
      use Ada.Calendar.Formatting;
      use Strings_Edit;
      Hour     : Hour_Number;
      Minute   : Minute_Number;
      Second   : Second_Number;
      Fraction : Second_Duration;
      Index    : Integer := Pointer;
   begin
      Ada.Calendar.Formatting.Split
      (  Seconds    => Value,
         Hour       => Hour,
         Minute     => Minute,
         Second     => Second,
         Sub_Second => Fraction
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Hour),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Minute),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Second),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Pointer := Index;
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Time
             )  is
      Buffer : String (1..Maximum_Time_Length);
      Index  : Integer := 1;
   begin
      Put (Buffer, Index, False, Value);
      Put (Data, Pointer, Buffer (1..Index - 1));
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Day_Duration
             )  is
      Buffer : String (1..6);
      Index  : Integer := 1;
   begin
      Put (Buffer, Index, Value);
      Put (Data, Pointer, Buffer);
   end Put;

   procedure Put_Date
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Time
             )  is
      Buffer : String (1..10);
      Index  : Integer := 1;
   begin
      Put_Date (Buffer, Index, Value);
      Put (Data, Pointer, Buffer);
   end Put_Date;

   procedure Put_Date
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Time
             )  is
      use Strings_Edit;
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
      Index   : Integer := Pointer;
   begin
      Ada.Calendar.Split
      (  Date       => Value,
         Year       => Year,
         Month      => Month,
         Day        => Day,
         Seconds    => Seconds
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Year),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 4
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => "-"
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Month),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => "-"
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Day),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Pointer := Index;
   end Put_Date;

   procedure Put_Date_Time
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Time
             )  is
      Buffer : String (1..19);
      Index  : Integer := 1;
   begin
      Put_Date_Time (Buffer, Index, Value);
      Put (Data, Pointer, Buffer);
   end Put_Date_Time;

   procedure Put_Date_Time
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Time
             )  is
      use Strings_Edit;
      use Ada.Calendar.Formatting;
      Year     : Year_Number;
      Month    : Month_Number;
      Day      : Day_Number;
      Hour     : Hour_Number;
      Minute   : Minute_Number;
      Second   : Second_Number;
      Seconds  : Day_Duration;
      Fraction : Second_Duration;
      Index    : Integer := Pointer;
   begin
      Ada.Calendar.Split
      (  Date    => Value,
         Year    => Year,
         Month   => Month,
         Day     => Day,
         Seconds => Seconds
      );
      Ada.Calendar.Formatting.Split
      (  Seconds    => Seconds,
         Hour       => Hour,
         Minute     => Minute,
         Second     => Second,
         Sub_Second => Fraction
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Year),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 4
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => "-"
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Month),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => "-"
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Day),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => "T"
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Hour),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => ":"
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Minute),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => ":"
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Integer (Second),
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Pointer := Index;
   end Put_Date_Time;

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

   procedure Put_Duration
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Duration
             )  is
   begin
      Put (Data, Pointer, Strings_Edit.ISO_8601.Image (Value, 6));
   end Put_Duration;

   procedure Put_Time
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Time
             )  is
   begin
      Put (Data, Pointer, Strings_Edit.ISO_8601.Image (Value, 6));
   end Put_Time;

   procedure Put_Time_Of_Day
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Day_Duration
             )  is
      Buffer : String (1..8);
      Index  : Integer := 1;
   begin
      Put_Time_Of_Day (Buffer, Index, Value);
      Put (Data, Pointer, Buffer);
   end Put_Time_Of_Day;

   procedure Put_Time_Of_Day
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Day_Duration
             )  is
      use Strings_Edit;
      Seconds : constant Integer := Integer (Value);
      Index   : Integer := Pointer;
   begin
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Seconds / 3_600,
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => ":"
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => (Seconds / 60) mod 60,
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => ":"
      );
      Put
      (  Destination => Destination,
         Pointer     => Index,
         Value       => Seconds mod 60,
         Justify     => Strings_Edit.Right,
         Fill        => '0',
         Field       => 2
      );
      Pointer := Index;
   end Put_Time_Of_Day;

   procedure Put_UTC
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Time
             )  is
      Buffer : String (1..Maximum_Time_Length);
      Index  : Integer := 1;
   begin
      Put (Buffer, Index, True, Value);
      Put (Data, Pointer, Buffer (1..Index - 1));
   end Put_UTC;

   procedure Set_Duration
             (  Item  : in out Public_Duration_Data_Item;
                Value : Duration
             )  is
   begin
      Item.Value := Value;
   end Set_Duration;

   procedure Set_Duration
             (  Item  : in out Implicit_Time_Of_Day_Data_Item;
                Value : Duration
             )  is
   begin
      if Value not in Day_Duration then
         raise Constraint_Error;
      end if;
      Item.Value := Value;
   end Set_Duration;

   procedure Set_Duration
             (  Item  : in out Time_Of_Day_Data_Item;
                Value : Duration
             )  is
   begin
      if Value not in Day_Duration then
         raise Constraint_Error;
      end if;
      Item.Value := Value;
   end Set_Duration;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Date_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      Set_Implicit_Tag (Item.Text, Tag, Length);
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Date_Time_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      Set_Implicit_Tag (Item.Text, Tag, Length);
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Duration_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      Set_Implicit_Tag (Item.Text, Tag, Length);
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Generalized_Time_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      Set_Implicit_Tag (Item.Text, Tag, Length);
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Time_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      Set_Implicit_Tag (Item.Text, Tag, Length);
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Time_Of_Day_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      Set_Implicit_Tag (Item.Text, Tag, Length);
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_UTC_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      Set_Implicit_Tag (Item.Text, Tag, Length);
   end Set_Implicit_Tag;

   procedure Set_Time
             (  Item  : in out Public_Time_Data_Item;
                Value : Time
             )  is
   begin
      Item.Value := Value;
   end Set_Time;

end GNAT.Sockets.Connection_State_Machine.ASN1.Dates;
