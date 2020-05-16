--                                                                    --
--  procedure Test_Data_Server      Copyright (c)  Dmitry A. Kazakov  --
--  Test server                                    Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  13:13 14 Sep 2019  --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Streams;                    use Ada.Streams;
with Ada.Text_IO;                    use Ada.Text_IO;
with GNAT.Sockets.Server;            use GNAT.Sockets.Server;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Strings_Edit.Long_Floats;       use Strings_Edit.Long_Floats;
with Strings_Edit.Quoted;            use Strings_Edit.Quoted;
with Strings_Edit.Time_Conversions;  use Strings_Edit.Time_Conversions;
with Test_Data_Servers;              use Test_Data_Servers;

with Ada.Calendar.Formatting;
with Ada.Tags;
with GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.Explicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Booleans;
with GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
with GNAT.Sockets.Connection_State_Machine.ASN1.Dates;
with GNAT.Sockets.Connection_State_Machine.ASN1.Distinguished_Names;
with GNAT.Sockets.Connection_State_Machine.ASN1.Floats;
with GNAT.Sockets.Connection_State_Machine.ASN1.Indefinite_Unsigneds;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_8;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_16;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_64;
with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
with GNAT.Sockets.Connection_State_Machine.ASN1.Long_Floats;
with GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;
with GNAT.Sockets.Connection_State_Machine.ASN1.Objects;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sets;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Unsigneds_8;
with GNAT.Sockets.Connection_State_Machine.ASN1.Unsigneds_16;
with GNAT.Sockets.Connection_State_Machine.ASN1.Unsigneds_32;
with GNAT.Sockets.Connection_State_Machine.ASN1.Unsigneds_64;
with GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
with GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates.
     Stream_IO;
with Interfaces;
with Strings_Edit.Distinguished_Names;
with Strings_Edit.Object_Identifiers;
with Strings_Edit.UTF8.Handling;
with GNAT.Exception_Traces;

with Ada.Streams.Stream_IO;
with GNAT.Sockets.Connection_State_Machine.ASN1.Text_IO;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Text_IO;

procedure Test_Data_Server is
   use GNAT.Sockets.Connection_State_Machine;
   Minutes : constant := 10.0;
   Port    : constant := 5876;

   function Bits (Data : Stream_Element_Array)
      return ASN1.Bit_Strings.Boolean_Array is
      use ASN1.Bit_Strings;
      Result  : Boolean_Array (1..Data'Length * 8);
      Octet   : Stream_Element;
      Pointer : Integer := 1;
   begin
      for Index in Data'Range loop
         Octet := Data (Index);
         Result (Pointer..Pointer + 7) :=
            (  0 /= (Octet and 2#1000_0000#),
               0 /= (Octet and 2#0100_0000#),
               0 /= (Octet and 2#0010_0000#),
               0 /= (Octet and 2#0001_0000#),
               0 /= (Octet and 2#0000_1000#),
               0 /= (Octet and 2#0000_0100#),
               0 /= (Octet and 2#0000_0010#),
               0 /= (Octet and 2#0000_0001#)
            );
         Pointer := Pointer + 8;
      end loop;
      return Result;
   end Bits;

   function Bit_Image (Value : ASN1.Bit_Strings.Boolean_Array)
      return String is
      Result  : String (1..((Value'Length + 7) / 8) * 3);
      Pointer : Integer := 1;
      Accum   : Integer := 0;
      Mask    : Integer := 128;
   begin
      if Value'Length = 0 then
         return "";
      end if;
      for Index in Value'Range loop
         if Value (Index) then
            Accum := Accum + Mask;
         end if;
         Mask := Mask / 2;
         if Mask = 0 or else Index = Value'Last then
            if Pointer > 1 then
               Result (Pointer) := ' ';
               Pointer := Pointer + 1;
            end if;
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => Accum,
               Field       => 2,
               Justify     => Strings_Edit.Right,
               Fill        => '0',
               Base        => 16
            );
            Accum := 0;
            Mask  := 128;
         end if;
      end loop;
      return Result (1..Pointer - 1);
   end Bit_Image;

   function "+" (Data : String) return Stream_Element_Array is
      use Strings_Edit;
      Result  : Stream_Element_Array (1..Data'Length);
      Element : Stream_Element_Offset := 1;
      Pointer : Integer := Data'First;
   begin
      Get (Data, Pointer);
      while Pointer <= Data'Last loop
         Get (Data, Pointer, Integer (Result (Element)), Base => 16);
         Get (Data, Pointer);
         Element := Element + 1;
      end loop;
      return Result (1..Element - 1);
   end "+";

   function "+"
            (  Left  : Stream_Element_Array;
               Right : String
            )  return Stream_Element_Array is
   begin
      return Left & (+Right);
   end "+";

   procedure Feed
             (  Title : String;
                Data  : Stream_Element_Array;
                Item  : in out Data_Item'Class
             )  is
      Client  : Null_Machine;
      State   : Stream_Element_Offset := 0;
      Pointer : Stream_Element_Offset := Data'First;
   begin
      loop
         if Pointer > Data'Last then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Feed overrun Pointer"
               &  Stream_Element_Offset'Image (Pointer)
               &  " >"
               &  Stream_Element_Offset'Image (Data'Last)
               &  " (last) State ="
               &  Stream_Element_Offset'Image (State)
               &  " ["
               &  Image (Data)
               &  "]"
            )  );
         end if;
         Feed
         (  Item    => Item,
            Data    => Data (Pointer..Pointer),
            Client  => Client,
            Pointer => Pointer,
            State   => State
         );
         exit when State = 0;
      end loop;
      if Pointer /= Data'Last + 1 then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Feed fault. Pointer"
            &  Stream_Element_Offset'Image (Pointer)
            &  " /="
            &  Stream_Element_Offset'Image (Data'Last + 1)
            &  " (expected) ["
            &  Image (Data)
            &  "]"
         )  );
      end if;
   end Feed;

   generic
      type Value_Type (<>) is private;
      type Item_Type is
         new GNAT.Sockets.Connection_State_Machine.ASN1.
             ASN1_Data_Item with private;
      with function Take  (Item  : Item_Type ) return Value_Type;
      with procedure Set_Value
                     (  Item  : in out Item_Type;
                        Value : Value_Type
                     )  is <>;
      with function Image (Value : Value_Type) return String;
      with procedure Get
                     (  Data    : Stream_Element_Array;
                        Pointer : in out Stream_Element_Offset;
                        Length  : Stream_Element_Offset;
                        Value   : out Value_Type
                     );
      with procedure Put
                     (  Data    : in out Stream_Element_Array;
                        Pointer : in out Stream_Element_Offset;
                        Value   : Value_Type
                     );
      with function "=" (Left, Right : Value_Type) return Boolean is <>;
   package Generic_Data_Test is
      procedure Test_Get
                (  Title    : String;
                   Sequence : Stream_Element_Array;
                   Expected : Value_Type
                );
      procedure Test_Value
                (  Title : String;
                   Value : Value_Type
                );
   end Generic_Data_Test;

   package body Generic_Data_Test is

      procedure Test_Get
                (  Title    : String;
                   Sequence : Stream_Element_Array;
                   Expected : Value_Type
                )  is
         Other       : Value_Type            := Expected;
         Pointer     : Stream_Element_Offset := Sequence'First;
         Tag         : ASN1.Tag_Type;
         Constructed : Boolean;
         Item        : Item_Type;
      begin
         ASN1.Get (Sequence, Pointer, Tag, Constructed);
         if Sequence (Pointer) > 127 then
            Pointer := Pointer + 2;
         else
            Pointer := Pointer + 1;
         end if;
         Get (Sequence, Pointer, Sequence'Last - Pointer + 1, Other);
         if Pointer /= Sequence'Last + 1 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Get fault [ "
               &  Image (Sequence)
               &  "] Pointer"
               &  Stream_Element_Offset'Image (Pointer)
               &  " /="
               &  Stream_Element_Offset'Image (Sequence'Last + 1)
               &  " (expected) "
               &  Ada.Tags.Expanded_Name (Item_Type'Tag)
            )  );
         elsif Other /= Expected then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Get fault ["
               &  Image (Sequence)
               &  "] "
               &  Image (Other)
               &  "/="
               &  Image (Expected)
               &  " (expected) "
               &  Ada.Tags.Expanded_Name (Item_Type'Tag)
            )  );
         end if;
         Feed (Title, Sequence, Item);
         if Take (Item) /= Expected then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Get Feed fault ["
               &  Image  (Sequence)
               &  "] "
               &  Image (Take (Item))
               &  "/="
               &  Image (Expected)
               &  " (expected) "
               &  Ada.Tags.Expanded_Name (Item_Type'Tag)
            )  );
         end if;
      exception
         when Status_Error =>
            raise;
         when Error : others =>
            Raise_Exception
            (  Data_Error'Identity,
               (  Title
               &  " exception in Test_Get ["
               &  Image (Sequence)
               &  "] "
               &  Ada.Tags.Expanded_Name (Item_Type'Tag)
               &  " "
               &  Exception_Information (Error)
            )  );
      end Test_Get;

      procedure Test_Value
                (  Title : String;
                   Value : Value_Type
                )  is
         Other       : Value_Type := Value;
         Item        : Item_Type;
         Buffer      : Stream_Element_Array (1..1024);
         Pointer_1   : Stream_Element_Offset := Buffer'First;
         Pointer_2   : Stream_Element_Offset := Buffer'First;
         Tag         : ASN1.Tag_Type;
         Constructed : Boolean;
      begin
         Set_Value (Item, Value);
         Encode (Item, Buffer, Pointer_1);
         ASN1.Get (Buffer, Pointer_2, Tag, Constructed);
         if Buffer (Pointer_2) > 127 then
            Pointer_2 := Pointer_2 + 2;
         else
            Pointer_2 := Pointer_2 + 1;
         end if;
         Get (Buffer, Pointer_2, Pointer_1 - Pointer_2, Other);
         if Other /= Value then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Value fault ["
               &  Image  (Buffer (1..Pointer_1 - 1))
               &  "] "
               &  Image (Other)
               &  "/="
               &  Image (Value)
               &  " (expected) "
               &  Ada.Tags.Expanded_Name (Item_Type'Tag)
            )  );
         end if;
         Feed (Title, Buffer (Buffer'First..Pointer_1 - 1), Item);
         if Take (Item) /= Value then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Value Feed fault ["
               &  Image  (Buffer (1..Pointer_1 - 1))
               &  "] "
               &  Image (Take (Item))
               &  "/="
               &  Image (Value)
               &  " (expected) "
               &  Ada.Tags.Expanded_Name (Item_Type'Tag)
            )  );
         end if;
      exception
         when Status_Error =>
            raise;
         when Error : others =>
            Raise_Exception
            (  Data_Error'Identity,
               (  Title
               &  " exception in Test_Value ["
               &  Image  (Buffer (1..Pointer_1 - 1))
               &  "] "
               &  Ada.Tags.Expanded_Name (Item_Type'Tag)
               &  " "
               &  Exception_Information (Error)
            )  );
      end Test_Value;

   end Generic_Data_Test;

   function Equal (Left, Right : Float) return Boolean is
      Diff : constant Float := abs (Left - Right);
   begin
      if Diff = 0.0 then
         return True;
      else
         return Float'Exponent (Left)  - Float'Exponent (Diff) > 4
             or Float'Exponent (Right) - Float'Exponent (Diff) > 4;
      end if;
   end Equal;

   function Image
            (  Item : ASN1.ASN1_Data_Item'Class
            )  return Stream_Element_Array is
      Buffer : Stream_Element_Array (1..1024);
      Index  : Stream_Element_Offset := 1;
   begin
      ASN1.Encode (Item, Buffer, Index);
      return Buffer (1..Index - 1);
   end Image;

   function Image
            (  Value : GNAT.Sockets.Connection_State_Machine.ASN1.
                       Bit_Strings.Boolean_Array
            )  return String is
      Result  : String (1..((Value'Length + 3) / 4) * 5);
      Pointer : Integer := 1;
      Count   : Integer := 4 - Result'Length mod 4;
   begin
      for Index in Value'Range loop
         if Count = 0 then
            if Index > Value'First then
               Result (Pointer) := '_';
               Pointer := Pointer + 1;
            end if;
            Count := 3;
         else
            Count := Count - 1;
         end if;
         if Value (Index) then
            Result (Pointer) := '1';
         else
            Result (Pointer) := '0';
         end if;
         Pointer := Pointer + 1;
      end loop;
      return Result (1..Pointer - 1);
   end Image;

   function Compare_Date (Left, Right : Ada.Calendar.Time)
      return Boolean is
      use Ada.Calendar;
   begin
      return abs (Left - Right) < 86400.0;
   end Compare_Date;

   function Compare_Generalized (Left, Right : Ada.Calendar.Time)
      return Boolean is
      use Ada.Calendar;
   begin
      return abs (Left - Right) < 1.0;
   end Compare_Generalized;

   function Compare_UTC (Left, Right : Ada.Calendar.Time)
      return Boolean is
      use Ada.Calendar;
   begin
      return abs (Left - Right) < 1.0;
   end Compare_UTC;

   function Take_Boolean
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Booleans.Boolean_Data_Item
            )  return Boolean is
   begin
      return Item.Value;
   end Take_Boolean;

   function Take_Character
            (  Item : Character_ASN1.Enumeration_Data_Item
            )  return Character is
   begin
      return Item.Value;
   end Take_Character;

   function Take_Generalized_Time
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Dates.Generalized_Time_Data_Item
            )  return Ada.Calendar.Time is
   begin
      return Item.Value;
   end Take_Generalized_Time;

   function Take_UTC
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Dates.UTC_Data_Item
            )  return Ada.Calendar.Time is
   begin
      return Item.Value;
   end Take_UTC;

   function Take_Float
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Floats.Real_Data_Item
            )  return Float is
   begin
      return Item.Value;
   end Take_Float;

   function Take_Integer
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Integers.Integer_Data_Item
            )  return Integer is
   begin
      return Item.Value;
   end Take_Integer;

   function Take_Integer_8
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Integers_8.Integer_Data_Item
            )  return Interfaces.Integer_8 is
   begin
      return Item.Value;
   end Take_Integer_8;

   function Take_Integer_16
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Integers_16.Integer_Data_Item
            )  return Interfaces.Integer_16 is
   begin
      return Item.Value;
   end Take_Integer_16;

   function Take_Integer_32
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Integers_32.Integer_Data_Item
            )  return Interfaces.Integer_32 is
   begin
      return Item.Value;
   end Take_Integer_32;

   function Take_Integer_64
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Integers_64.Integer_Data_Item
            )  return Interfaces.Integer_64 is
   begin
      return Item.Value;
   end Take_Integer_64;

   function Take
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Object_Identifiers.Implicit_OID_Data_Item'Class
            )  return Strings_Edit.Object_Identifiers.
                      Object_Identifier is
   begin
      return Item.Value (1..Item.Length);
   end Take;

   function Take_Unsigned_8
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Unsigneds_8.Unsigned_Data_Item
            )  return Interfaces.Unsigned_8 is
   begin
      return Item.Value;
   end Take_Unsigned_8;

   function Take_Unsigned_16
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Unsigneds_16.Unsigned_Data_Item
            )  return Interfaces.Unsigned_16 is
   begin
      return Item.Value;
   end Take_Unsigned_16;

   function Take_Unsigned_32
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Unsigneds_32.Unsigned_Data_Item
            )  return Interfaces.Unsigned_32 is
   begin
      return Item.Value;
   end Take_Unsigned_32;

   function Take_Unsigned_64
            (  Item : GNAT.Sockets.Connection_State_Machine.ASN1.
                      Unsigneds_64.Unsigned_Data_Item
            )  return Interfaces.Unsigned_64 is
   begin
      return Item.Value;
   end Take_Unsigned_64;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Count;
                Value   : out Boolean
             )  is
   begin
      ASN1.Booleans.Get (Data, Pointer, Value);
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Count;
                Value   : out Character
             )  is
   begin
      Character_ASN1.Get (Data, Pointer, Length, Value);
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Count;
                Value   : out Ada.Calendar.Time
             )  is
   begin
      ASN1.Dates.Get (Data, Pointer, Length, Value);
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Count;
                Value   : out Interfaces.Integer_8
             )  is
   begin
      ASN1.Integers_8.Get (Data, Pointer, Value);
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Count;
                Value   : out Interfaces.Unsigned_8
             )  is
   begin
      ASN1.Unsigneds_8.Get (Data, Pointer, Value);
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Count;
                Value   : out Float
             )  is
   begin
      ASN1.Floats.Get (Data, Pointer, Length, Value);
   end Get;

   procedure Get_UTC
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Count;
                Value   : out Ada.Calendar.Time
             )  is
   begin
      ASN1.Dates.Get_UTC (Data, Pointer, Length, Value);
   end Get_UTC;

   package Boolean_Test is
      new Generic_Data_Test
          (  Boolean,
             ASN1.Booleans.Boolean_Data_Item,
             Take_Boolean,
             ASN1.Booleans.Set_Value,
             Boolean'Image,
             Get,
             ASN1.Booleans.Put
          );
   package Character_Test is
      new Generic_Data_Test
          (  Character,
             Character_ASN1.Enumeration_Data_Item,
             Take_Character,
             Character_ASN1.Set_Value,
             Character'Image,
             Get,
             Character_ASN1.Put
          );
   package Date_Test is
      new Generic_Data_Test
          (  Ada.Calendar.Time,
             ASN1.Dates.Date_Data_Item,
             ASN1.Dates.Get_Time,
             ASN1.Dates.Set_Time,
             Strings_Edit.Time_Conversions.To_String,
             ASN1.Dates.Get_Date,
             ASN1.Dates.Put_Date,
             Compare_Date
          );
   package Date_Time_Test is
      new Generic_Data_Test
          (  Ada.Calendar.Time,
             ASN1.Dates.Date_Time_Data_Item,
             ASN1.Dates.Get_Time,
             ASN1.Dates.Set_Time,
             Strings_Edit.Time_Conversions.To_String,
             ASN1.Dates.Get_Date_Time,
             ASN1.Dates.Put_Date_Time,
             Compare_UTC
          );
   package Duration_Test is
      new Generic_Data_Test
          (  Duration,
             ASN1.Dates.Duration_Data_Item,
             ASN1.Dates.Get_Duration,
             ASN1.Dates.Set_Duration,
             Duration'Image,
             ASN1.Dates.Get_Duration,
             ASN1.Dates.Put_Duration
          );
   package Generalized_Time_Test is
      new Generic_Data_Test
          (  Ada.Calendar.Time,
             ASN1.Dates.Generalized_Time_Data_Item,
             Take_Generalized_Time,
             ASN1.Dates.Set_Time,
             Strings_Edit.Time_Conversions.To_String,
             Get,
             ASN1.Dates.Put,
             Compare_Generalized
          );
   package Float_Test is
      new Generic_Data_Test
          (  Float,
             ASN1.Floats.Real_Data_Item,
             Take_Float,
             ASN1.Floats.Set_Value,
             Float'Image,
             Get,
             ASN1.Floats.Put,
             Equal
          );
   package Integer_Test is
      new Generic_Data_Test
          (  Integer,
             ASN1.Integers.Integer_Data_Item,
             Take_Integer,
             ASN1.Integers.Set_Value,
             Integer'Image,
             ASN1.Integers.Get,
             ASN1.Integers.Put
          );
   package Integer_8_Test is
      new Generic_Data_Test
          (  Interfaces.Integer_8,
             ASN1.Integers_8.Integer_Data_Item,
             Take_Integer_8,
             ASN1.Integers_8.Set_Value,
             Interfaces.Integer_8'Image,
             Get,
             ASN1.Integers_8.Put,
             Interfaces."="
          );
   package Integer_16_Test is
      new Generic_Data_Test
          (  Interfaces.Integer_16,
             ASN1.Integers_16.Integer_Data_Item,
             Take_Integer_16,
             ASN1.Integers_16.Set_Value,
             Interfaces.Integer_16'Image,
             ASN1.Integers_16.Get,
             ASN1.Integers_16.Put,
             Interfaces."="
          );
   package Integer_32_Test is
      new Generic_Data_Test
          (  Interfaces.Integer_32,
             ASN1.Integers_32.Integer_Data_Item,
             Take_Integer_32,
             ASN1.Integers_32.Set_Value,
             Interfaces.Integer_32'Image,
             ASN1.Integers_32.Get,
             ASN1.Integers_32.Put,
             Interfaces."="
          );
   package Integer_64_Test is
      new Generic_Data_Test
          (  Interfaces.Integer_64,
             ASN1.Integers_64.Integer_Data_Item,
             Take_Integer_64,
             ASN1.Integers_64.Set_Value,
             Interfaces.Integer_64'Image,
             ASN1.Integers_64.Get,
             ASN1.Integers_64.Put,
             Interfaces."="
          );
   package Time_Test is
      new Generic_Data_Test
          (  Ada.Calendar.Time,
             ASN1.Dates.Time_Data_Item,
             ASN1.Dates.Get_Time,
             ASN1.Dates.Set_Time,
             Strings_Edit.Time_Conversions.To_String,
             ASN1.Dates.Get_Time,
             ASN1.Dates.Put_Time,
             Compare_UTC
          );
   package Time_Of_Day_Test is
      new Generic_Data_Test
          (  Duration,
             ASN1.Dates.Time_Of_Day_Data_Item,
             ASN1.Dates.Get_Duration,
             ASN1.Dates.Set_Duration,
             Duration'Image,
             ASN1.Dates.Get_Time_Of_Day,
             ASN1.Dates.Put_Time_Of_Day
          );
   package Unsigned_8_Test is
      new Generic_Data_Test
          (  Interfaces.Unsigned_8,
             ASN1.Unsigneds_8.Unsigned_Data_Item,
             Take_Unsigned_8,
             ASN1.Unsigneds_8.Set_Value,
             Interfaces.Unsigned_8'Image,
             Get,
             ASN1.Unsigneds_8.Put,
             Interfaces."="
          );
   package Unsigned_16_Test is
      new Generic_Data_Test
          (  Interfaces.Unsigned_16,
             ASN1.Unsigneds_16.Unsigned_Data_Item,
             Take_Unsigned_16,
             ASN1.Unsigneds_16.Set_Value,
             Interfaces.Unsigned_16'Image,
             ASN1.Unsigneds_16.Get,
             ASN1.Unsigneds_16.Put,
             Interfaces."="
          );
   package Unsigned_32_Test is
      new Generic_Data_Test
          (  Interfaces.Unsigned_32,
             ASN1.Unsigneds_32.Unsigned_Data_Item,
             Take_Unsigned_32,
             ASN1.Unsigneds_32.Set_Value,
             Interfaces.Unsigned_32'Image,
             ASN1.Unsigneds_32.Get,
             ASN1.Unsigneds_32.Put,
             Interfaces."="
          );
   package Unsigned_64_Test is
      new Generic_Data_Test
          (  Interfaces.Unsigned_64,
             ASN1.Unsigneds_64.Unsigned_Data_Item,
             Take_Unsigned_64,
             ASN1.Unsigneds_64.Set_Value,
             Interfaces.Unsigned_64'Image,
             ASN1.Unsigneds_64.Get,
             ASN1.Unsigneds_64.Put,
             Interfaces."="
          );
   package UTC_Test is
      new Generic_Data_Test
          (  Ada.Calendar.Time,
             ASN1.Dates.UTC_Data_Item,
             Take_UTC,
             ASN1.Dates.Set_Time,
             Strings_Edit.Time_Conversions.To_String,
             Get_UTC,
             ASN1.Dates.Put_UTC,
             Compare_UTC
          );

   function ASN1_Date (Value : String)
      return Stream_Element_Array is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      Buffer  : Stream_Element_Array (1..100);
      Pointer : Stream_Element_Offset := Buffer'First;
      Start   : Stream_Element_Offset;
   begin
      ASN1.Put
      (  Buffer,
         Pointer,
         (ASN1.Universal_Tag, ASN1.Date_Tag, False),
         False
      );
      Pointer := Pointer + 1;
      Start   := Pointer;
      Put (Buffer, Pointer, Value);
      Buffer (Start - 1) := Stream_Element (Pointer - Start);
      return Buffer (1..Pointer - 1);
   end ASN1_Date;

   function ASN1_Date_Time (Value : String)
      return Stream_Element_Array is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      Buffer  : Stream_Element_Array (1..100);
      Pointer : Stream_Element_Offset := Buffer'First;
      Start   : Stream_Element_Offset;
   begin
      ASN1.Put
      (  Buffer,
         Pointer,
         (ASN1.Universal_Tag, ASN1.Date_Time_Tag, False),
         False
      );
      Pointer := Pointer + 1;
      Start   := Pointer;
      Put (Buffer, Pointer, Value);
      Buffer (Start - 1) := Stream_Element (Pointer - Start);
      return Buffer (1..Pointer - 1);
   end ASN1_Date_Time;

   function ASN1_Duration (Value : String)
      return Stream_Element_Array is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      Buffer  : Stream_Element_Array (1..100);
      Pointer : Stream_Element_Offset := Buffer'First;
      Start   : Stream_Element_Offset;
   begin
      ASN1.Put
      (  Buffer,
         Pointer,
         (ASN1.Universal_Tag, ASN1.Duration_Tag, False),
         False
      );
      Pointer := Pointer + 1;
      Start   := Pointer;
      Put (Buffer, Pointer, Value);
      Buffer (Start - 1) := Stream_Element (Pointer - Start);
      return Buffer (1..Pointer - 1);
   end ASN1_Duration;

   function ASN1_Time (Value : String)
      return Stream_Element_Array is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      Buffer  : Stream_Element_Array (1..100);
      Pointer : Stream_Element_Offset := Buffer'First;
      Start   : Stream_Element_Offset;
   begin
      ASN1.Put
      (  Buffer,
         Pointer,
         (ASN1.Universal_Tag, ASN1.Time_Tag, False),
         False
      );
      Pointer := Pointer + 1;
      Start   := Pointer;
      Put (Buffer, Pointer, Value);
      Buffer (Start - 1) := Stream_Element (Pointer - Start);
      return Buffer (1..Pointer - 1);
   end ASN1_Time;

   function ASN1_Time_Of_Day (Value : String)
      return Stream_Element_Array is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      Buffer  : Stream_Element_Array (1..100);
      Pointer : Stream_Element_Offset := Buffer'First;
      Start   : Stream_Element_Offset;
   begin
      ASN1.Put
      (  Buffer,
         Pointer,
         (ASN1.Universal_Tag, ASN1.Time_Of_Day_Tag, False),
         False
      );
      Pointer := Pointer + 1;
      Start   := Pointer;
      Put (Buffer, Pointer, Value);
      Buffer (Start - 1) := Stream_Element (Pointer - Start);
      return Buffer (1..Pointer - 1);
   end ASN1_Time_Of_Day;

   function ASN1_Real (Value : String) return Stream_Element_Array is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Floats;
      Buffer  : Stream_Element_Array (1..100);
      Pointer : Stream_Element_Offset := Buffer'First + 2;
   begin
      Buffer (Buffer'First) := Stream_Element (Real_Tag);
      Put (Buffer, Pointer, Value);
      Buffer (Buffer'First + 1) := Stream_Element (Pointer - 3);
      return Buffer (1..Pointer - 1);
   end ASN1_Real;

   function ASN1_Generalized (Value : String)
      return Stream_Element_Array is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      Buffer  : Stream_Element_Array (1..100);
      Pointer : Stream_Element_Offset := Buffer'First + 2;
   begin
      Buffer (Buffer'First) := Stream_Element (Generalized_Time_Tag);
      Put (Buffer, Pointer, Value);
      Buffer (Buffer'First + 1) := Stream_Element (Pointer - 3);
      return Buffer (1..Pointer - 1);
   end ASN1_Generalized;

   function ASN1_UTC (Value : String)
      return Stream_Element_Array is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      Buffer  : Stream_Element_Array (1..100);
      Pointer : Stream_Element_Offset := Buffer'First + 2;
   begin
      Buffer (Buffer'First) := Stream_Element (UTC_Time_Tag);
      Put (Buffer, Pointer, Value);
      Buffer (Buffer'First + 1) := Stream_Element (Pointer - 3);
      return Buffer (1..Pointer - 1);
   end ASN1_UTC;

   procedure Encoding_Test_Lengths is
      use GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
   begin
      declare
         Data    : Stream_Element_Array (1..100);
         Pointer : Stream_Element_Offset := Data'First;
         Length  : Stream_Element_Count;
      begin
         Pointer := 1;
         Data (1) := 2#00100110#;
         Get (Data, Pointer, Length);
         if Pointer /= 2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error Get ASN.1 length pointer: got"
               &  Stream_Element_Offset'Image (Pointer)
               &  ", expected"
               &  Stream_Element_Offset'Image (2)
            )  );
         elsif Length /= 38 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error Get ASN.1 length: got"
               &  Stream_Element_Count'Image (Length)
               &  ", expected"
               &  Stream_Element_Count'Image (38)
            )  );
         end if;
         Pointer := 1;
         Put (Data, Pointer, 201);
         if Pointer /= 3 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error Put ASN.1 length pointer: got"
               &  Stream_Element_Offset'Image (Pointer)
               &  ", expected"
               &  Stream_Element_Offset'Image (3)
            )  );
         end if;
         Pointer := 1;
         Get (Data, Pointer, Length);
         if Pointer /= 3 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error Get ASN.1 length pointer: got"
               &  Stream_Element_Offset'Image (Pointer)
               &  ", expected"
               &  Stream_Element_Offset'Image (3)
            )  );
         elsif Length /= 201 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error Get ASN.1 length: got"
               &  Stream_Element_Count'Image (Length)
               &  ", expected"
               &  Stream_Element_Count'Image (201)
            )  );
         end if;
      end;
      declare
         use ASN1.Strings.Implicit;
         Text    : Implicit_String_Data_Item (1024);
         Data    : Stream_Element_Array (1..1024);
         Pointer : Stream_Element_Offset;
         Index   : Stream_Element_Offset;
         Length  : Stream_Element_Offset;
      begin
         declare
            Expected : constant Stream_Element_Array :=
                                +"05 31 32 33 34 35";
         begin
            Set_Value (Text, "12345");
            Pointer := Data'First;
            Definite_Encode (Text, Data, Pointer);
            if Data (1..Pointer - 1) /= Expected then
               Put_Line (Image (Data (1..Pointer - 1), True));
               Put_Line (Image (Expected, True) & " (expected)");
               Raise_Exception
               (  Data_Error'Identity,
                  "Definite length encoding error"
               );
            end if;

            Pointer := Data'First;
            Encode_String (Text, Data, Pointer);
            if (  Data (1..Pointer - 1)
               /= Stream_Element (Get_ASN1_Type (Text)) & Expected
               )  then
               Put_Line (Image (Data (1..Pointer - 1), True));
               Put_Line
               (  Image
                  (  Stream_Element (Get_ASN1_Type (Text)) & Expected,
                     True
                  )
               &  " (expected)"
               );
               Raise_Exception
               (  Data_Error'Identity,
                  "Definite length generic encoder error"
               );
            end if;

            Pointer := Data'First;
            Put_String
            (  Data,
               Pointer,
               Get_ASN1_Type (Text),
               Get_Value (Text)
            );
            if (  Data (1..Pointer - 1)
               /= Stream_Element (Get_ASN1_Type (Text)) & Expected
               )  then
               Put_Line (Image (Data (1..Pointer - 1), True));
               Put_Line
               (  Image
                  (  Stream_Element (Get_ASN1_Type (Text)) & Expected,
                     True
                  )
               &  " (expected)"
               );
               Raise_Exception
               (  Data_Error'Identity,
                  "Definite length generic Put error"
               );
            end if;
         end;
         Set_Value
         (  Text,
            "1234567890123456789a123456789b123456789c123456789d" &
            "123456789e123456789f123456789g123456789h123456789i" &
            "123456789j123456789k123456789l123456789m123456789n"
         );

         Pointer := Data'First;
         Definite_Encode (Text, Data, Pointer);
         Index := Data'First;
         Get (Data, Index, Length);
         if Length /= Stream_Element_Offset (Get_Length (Text)) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error definite length encode: got"
               &  Stream_Element_Count'Image (Length)
               &  ", expected "
               &  Image (Get_Length (Text))
            )  );
         elsif (  To_String (Data (Index..Pointer - 1))
               /= Get_Value (Text)
               )  then
            Put_Line (Image (Data (1..Pointer - 1), True));
            Put_Line
            (  Image (From_String (Get_Value (Text)), True)
            &  " (expected)"
            );
            Raise_Exception
            (  Data_Error'Identity,
               "Definite length encoding error"
            );
         end if;

         Pointer := Data'First;
         Encode_String (Text, Data, Pointer);
         Index := Data'First + 1;
         Get (Data, Index, Length);
         if Length /= Stream_Element_Offset (Get_Length (Text)) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error definite length generic encoder: got"
               &  Stream_Element_Count'Image (Length)
               &  ", expected "
               &  Image (Get_Length (Text))
            )  );
         elsif Data (1) /= Stream_Element (Get_ASN1_Type (Text)) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error definite length generic type encoder: got"
               &  Stream_Element'Image (Data (1))
               &  ", expected "
               &  Image (Integer (Get_ASN1_Type (Text)))
            )  );
         elsif (  To_String (Data (Index..Pointer - 1))
               /= Get_Value (Text)
               )  then
            Put_Line (Image (Data (1..Pointer - 1), True));
            Put_Line
            (  Image (From_String (Get_Value (Text)), True)
            &  " (expected body)"
            );
            Raise_Exception
            (  Data_Error'Identity,
               "Definite length generic encoder error"
            );
         end if;

         Pointer := Data'First;
         Put_String
         (  Data,
            Pointer,
            Get_ASN1_Type (Text),
            Get_Value (Text)
         );
         Index := Data'First + 1;
         Get (Data, Index, Length);
         if Length /= Stream_Element_Offset (Get_Length (Text)) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error definite length generic Put: got"
               &  Stream_Element_Count'Image (Length)
               &  ", expected "
               &  Image (Get_Length (Text))
            )  );
         elsif Data (1) /= Stream_Element (Get_ASN1_Type (Text)) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error definite length generic type Put: got"
               &  Stream_Element'Image (Data (1))
               &  ", expected "
               &  Image (Integer (Get_ASN1_Type (Text)))
            )  );
         elsif (  To_String (Data (Index..Pointer - 1))
               /= Get_Value (Text)
               )  then
            Put_Line (Image (Data (1..Pointer - 1), True));
            Put_Line
            (  Image (From_String (Get_Value (Text)), True)
            &  " (expected body)"
            );
            Raise_Exception
            (  Data_Error'Identity,
               "Definite length generic Put error"
            );
         end if;
      end;
   end Encoding_Test_Lengths;
   ---------------------------------------------------------------------
   procedure Test_Value
             (  Value     : GNAT.Sockets.Connection_State_Machine.ASN1.
                            Bit_Strings.Boolean_Array;
                Test_Feed : Boolean := True
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.
          Explicit;
      Title     : constant String := "ASN.1 bit string";
      Client    : Shared_Bit_Strings_Record_Machine;
      Other     : Boolean_Array (Value'Range);
      Item      : Bit_String_Data_Item (1024);
      Buffer    : Stream_Element_Array (1..1024);
      Pointer_1 : Stream_Element_Offset;
      Pointer_2 : Stream_Element_Offset;
   begin
      Set_Value (Item, Value);
      Pointer_1 := Buffer'First;
      Encode (Item, Buffer, Pointer_1);
      Pointer_2 := Buffer'First + 2;
      Get (Buffer, Pointer_2, Other);
      if Other /= Value then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value fault ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Image (Other)
            &  "/="
            &  Image (Value)
            &  " (expected)"
         )  );
      end if;
      if Test_Feed then
         Feed (Title, Buffer (Buffer'First..Pointer_1 - 1), Item);
         if Get_Value (Item) /= Value then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Value Feed fault ["
               &  Image  (Buffer (1..Pointer_1 - 1))
               &  "] "
               &  Image (Get_Value (Item))
               &  "/="
               &  Image (Value)
               &  " (expected)"
            )  );
         end if;
         Feed (Client, Title, Buffer (Buffer'First..Pointer_1 - 1));
         if Get_Value (Client.Data) /= Value then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Value external Feed fault ["
--               &  Image  (Buffer (1..Pointer_1 - 1))
--               &  "] "
               &  Image (Get_Value (Client.Data))
               &  "/="
               &  Image (Value)
               &  " (expected)"
            )  );
         end if;
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Value ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Value;

   procedure Test_Value
             (  Value_1   : GNAT.Sockets.Connection_State_Machine.ASN1.
                            Bit_Strings.Boolean_Array;
                Value_2   : GNAT.Sockets.Connection_State_Machine.ASN1.
                            Bit_Strings.Boolean_Array;
                Test_Feed : Boolean := True
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.
               Explicit;
      Title   : constant String := "ASN.1 bit string";
      Item    : Bit_String_Data_Item (1024);
      Buffer  : Stream_Element_Array (1..1024);
      Pointer : Stream_Element_Offset;
   begin
      Buffer (Buffer'First) :=
         Stream_Element (ASN1_Tag (Bit_String_Tag) or Constructed);
      Pointer := Buffer'First + 1;
      Buffer (Pointer) := 16#80#; Pointer := Pointer + 1;
      Set_Value (Item, Value_1);
      Encode (Item, Buffer, Pointer);
      Set_Value (Item, Value_2);
      Encode (Item, Buffer, Pointer);
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Feed (Title, Buffer (Buffer'First..Pointer - 1), Item);
      if Get_Value (Item) /= Value_1 & Value_2 then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value indefinite constructor Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] ("
            &  Image (Item.Length)
            &  " length) "
            &  Image (Get_Value (Item))
            &  "/="
            &  Image (Value_1 & Value_2)
            &  " (expected)"
         )  );
      end if;
      Pointer := Buffer'First + 2;
      Set_Value (Item, Value_1);
      Encode (Item, Buffer, Pointer);
      Set_Value (Item, Value_2);
      Encode (Item, Buffer, Pointer);
      Buffer (Buffer'First + 1) :=
         Stream_Element (Pointer - Buffer'First - 2);
      Feed (Title, Buffer (Buffer'First..Pointer - 1), Item);
      if Get_Value (Item) /= Value_1 & Value_2 then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value definite constructor Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] ("
            &  Image (Item.Length)
            &  " length) "
            &  Image (Get_Value (Item))
            &  "/="
            &  Image (Value_1 & Value_2)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Value ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Value;

   procedure Test_Bit_Value_Decode
             (  Encoded : Stream_Element_Array
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.
          Explicit;
      Title   : constant String := "ASN.1 bit shared string";
      Client  : Shared_Bit_Strings_Record_Machine;
      Buffer  : Stream_Element_Array (1..1024 * 5);
      Pointer : Stream_Element_Offset := 1;
   begin
      Feed (Client, Title, Encoded);
      Encode (Client.Data, Buffer, Pointer);
      if Buffer (1..Pointer - 1) /= Encoded then
         Put_Line (Image (Buffer (1..Pointer - 1), True));
         Put_Line (Image (Encoded, True) & " (expected)");
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " feed / encode fault"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Bit_Value_Decode "
            &  Exception_Information (Error)
         )  );
   end Test_Bit_Value_Decode;
   ---------------------------------------------------------------------
   procedure Test_String_Value (Value : String)  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit;
      Title     : constant String := "ASN.1 string";
      Other     : String (Value'Range);
      Item      : String_Data_Item (1024);
      Buffer    : Stream_Element_Array (1..1024);
      Pointer_1 : Stream_Element_Offset := Buffer'First;
      Pointer_2 : Stream_Element_Offset;
   begin
      Set_Value (Item, Value);
      Encode (Item, Buffer, Pointer_1);
      Pointer_2 := Buffer'First + 1;
      if Buffer (Pointer_2) > 127 then
         Pointer_2 := Pointer_2 + 2;
      else
         Pointer_2 := Pointer_2 + 1;
      end if;
      Get (Buffer, Pointer_2, Other);
      if Other /= Value then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value fault ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Other
            &  "/="
            &  Value
            &  " (expected)"
         )  );
      end if;
      Feed (Title, Buffer (Buffer'First..Pointer_1 - 1), Item);
      if Item.Value (1..Item.Length) /= Value then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value Feed fault ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Item.Value (1..Item.Length)
            &  "/="
            &  Value
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Value ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_String_Value;

   procedure Test_String_Value
             (  Value_1   : String;
                Value_2   : String;
                Test_Feed : Boolean := True
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit;
      Title   : constant String := "ASN.1 string";
      Item    : String_Data_Item (1024);
      Buffer  : Stream_Element_Array (1..1024);
      Pointer : Stream_Element_Offset;
   begin
      Buffer (Buffer'First) :=
         Stream_Element (ASN1_Tag (UTF8_String_Tag) or Constructed);
      Pointer := Buffer'First + 1;
      Buffer (Pointer) := 16#80#;         Pointer := Pointer + 1;
      Buffer (Pointer) := 16#0C#;         Pointer := Pointer + 1;
      Buffer (Pointer) := Value_1'Length; Pointer := Pointer + 1;
      Put (Buffer, Pointer, Value_1);
      Buffer (Pointer) := 16#0C#;         Pointer := Pointer + 1;
      Buffer (Pointer) := Value_2'Length; Pointer := Pointer + 1;
      Put (Buffer, Pointer, Value_2);
      Buffer (Pointer) := 16#00#;         Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#;         Pointer := Pointer + 1;
      Feed (Title, Buffer (Buffer'First..Pointer - 1), Item);
      if Get_Value (Item) /= Value_1 & Value_2 then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value constructor Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] ("
            &  Image (Item.Length)
            &  " length) "
            &  Get_Value (Item)
            &  "/="
            &  Value_1 & Value_2
            &  " (expected)"
         )  );
      end if;
      Pointer := Buffer'First + 2;
      Buffer (Pointer) := 16#0C#;         Pointer := Pointer + 1;
      Buffer (Pointer) := Value_1'Length; Pointer := Pointer + 1;
      Put (Buffer, Pointer, Value_1);
      Buffer (Pointer) := 16#0C#;         Pointer := Pointer + 1;
      Buffer (Pointer) := Value_2'Length; Pointer := Pointer + 1;
      Put (Buffer, Pointer, Value_2);
      Buffer (Buffer'First + 1) :=
         Stream_Element (Pointer - Buffer'First - 2);
      Feed (Title, Buffer (Buffer'First..Pointer - 1), Item);
      if Get_Value (Item) /= Value_1 & Value_2 then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value constructor Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] ("
            &  Image (Item.Length)
            &  " length) "
            &  Get_Value (Item)
            &  "/="
            &  Value_1 & Value_2
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Value ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_String_Value;

   procedure Test_Wide_String_Value
             (  Value_1   : Wide_String;
                Value_2   : Wide_String;
                Test_Feed : Boolean := True
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit;
      use Strings_Edit.UTF8;
      use Strings_Edit.UTF8.Handling;
      Title   : constant String := "ASN.1 string";
      Item    : String_Data_Item (1024);
      Buffer  : Stream_Element_Array (1..1024);
      Pointer : Stream_Element_Offset;
      Start   : Stream_Element_Offset;
   begin
      Buffer (Buffer'First) :=
         Stream_Element (ASN1_Tag (BMP_String_Tag) or Constructed);
      Pointer := Buffer'First + 1;
      Buffer (Pointer) := 16#80#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#1E#; Pointer := Pointer + 2;
                                  Start   := Pointer;
      Put_BMP (Buffer, Pointer, To_UTF8 (Value_1));
      Buffer (Start - 1) := Stream_Element (Pointer - Start);
      Buffer (Pointer) := 16#1E#; Pointer := Pointer + 2;
                                  Start   := Pointer;
      Put_BMP (Buffer, Pointer, To_UTF8 (Value_2));
      Buffer (Start - 1) := Stream_Element (Pointer - Start);
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Feed (Title, Buffer (Buffer'First..Pointer - 1), Item);
      if Get_Value (Item) /= To_UTF8 (Value_1) & To_UTF8 (Value_2) then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value constructor Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] ("
            &  Image (Item.Length)
            &  " length) "
            &  Get_Value (Item)
            &  "/="
            &  To_UTF8 (Value_1) & To_UTF8 (Value_2)
            &  " (expected)"
         )  );
      end if;
      Pointer := Buffer'First + 2;
      Buffer (Pointer) := 16#1E#; Pointer := Pointer + 2;
                                  Start   := Pointer;
      Put_BMP (Buffer, Pointer, To_UTF8 (Value_1));
      Buffer (Start - 1) := Stream_Element (Pointer - Start);
      Buffer (Pointer) := 16#1E#; Pointer := Pointer + 2;
                                  Start   := Pointer;
      Put_BMP (Buffer, Pointer, To_UTF8 (Value_2));
      Buffer (Start - 1) := Stream_Element (Pointer - Start);
      Buffer (Buffer'First + 1) :=
         Stream_Element (Pointer - Buffer'First - 2);
      Feed (Title, Buffer (Buffer'First..Pointer - 1), Item);
      if (  Item.Value (1..Item.Length)
         /= To_UTF8 (Value_1) & To_UTF8 (Value_2)
         )  then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value constructor Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] ("
            &  Image (Item.Length)
            &  " length) "
            &  Get_Value (Item)
            &  "/="
            &  To_UTF8 (Value_1) & To_UTF8 (Value_2)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Value ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Wide_String_Value;
   ---------------------------------------------------------------------
   procedure Test_Value
             (  Name  : String;
                Check : Boolean;
                Date  : Ada.Calendar.Time
             )  is
      use Ada.Calendar;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Booleans;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Dates;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Sequences;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit;
      Title   : constant String := "ASN.1 sequence";
      Client  : Name_And_Check_Machine;
      Buffer  : Stream_Element_Array (1..1024);
      Pointer : Stream_Element_Offset;
   begin
      Pointer := Buffer'First;
      Buffer (Pointer) := 16#30#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#80#; Pointer := Pointer + 1;
      Set_Value (Client.Data.Name,  Name);
      Set_Value (Client.Data.Check, Check);
      Set_Time  (Client.Data.Date,  Date);
      Encode (Client.Data.Name,  Buffer, Pointer);
      Encode (Client.Data.Check, Buffer, Pointer);
      Encode (Client.Data.Date,  Buffer, Pointer);
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Feed
      (  Client,
         (  Title
         &  " (indefinite {"
         &  Name
         &  ","
         &  Boolean'Image (Check)
         &  ","
         &  Strings_Edit.Time_Conversions.To_String (Date)
         &  "})"
         ),
         Buffer (Buffer'First..Pointer - 1)
      );
      if (  Client.Data.Name.Value (1..Client.Data.Name.Length)
         /= Name
         )  then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value sequence Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Client.Data.Name.Value (1..Client.Data.Name.Length)
            &  "/="
            &  Name
            &  " (expected)"
         )  );
      elsif Client.Data.Check.Value /= Check then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value sequence Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Boolean'Image (Client.Data.Check.Value)
            &  "/="
            &  Boolean'Image (Check)
            &  " (expected)"
         )  );
      end if;
      Pointer := Buffer'First + 2;
      Set_Value (Client.Data.Name,  Name);
      Set_Value (Client.Data.Check, Check);
      Set_Time  (Client.Data.Date,  Date);
      Encode (Client.Data.Name,  Buffer, Pointer);
      Encode (Client.Data.Check, Buffer, Pointer);
      Encode (Client.Data.Date,  Buffer, Pointer);
      Buffer (Buffer'First + 1) := Stream_Element (Pointer - 3);
      Feed
      (  Client,
         (  Title
         &  " (definite {"
         &  Name
         &  ","
         &  Boolean'Image (Check)
         &  ","
         &  Strings_Edit.Time_Conversions.To_String (Date)
         &  "})"
         ),
         Buffer (Buffer'First..Pointer - 1)
      );
      if (  Client.Data.Name.Value (1..Client.Data.Name.Length)
         /= Name
         )  then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value sequence Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Client.Data.Name.Value (1..Client.Data.Name.Length)
            &  "/="
            &  Name
            &  " (expected)"
         )  );
      elsif Client.Data.Check.Value /= Check then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value sequence Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Boolean'Image (Client.Data.Check.Value)
            &  "/="
            &  Boolean'Image (Check)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in sequence Test_Value ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Value;
   ---------------------------------------------------------------------
   procedure Test_OID_Get
             (  Title    : String;
                Sequence : Stream_Element_Array;
                Expected : Strings_Edit.Object_Identifiers.
                           Object_Identifier
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;
      use Strings_Edit.Object_Identifiers;
      Item    : OID_Data_Item (127);
      Other   : Object_Identifier     := Expected;
      Pointer : Stream_Element_Offset := Sequence'First + 1;
      Last    : Integer;
   begin
      Get (Sequence, Pointer, Sequence'Length, Other, Last);
      if Pointer /= Sequence'Last + 1 then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Get fault [ "
            &  Image (Sequence)
            &  "] Pointer"
            &  Stream_Element_Offset'Image (Pointer)
            &  " /="
            &  Stream_Element_Offset'Image (Sequence'Last + 1)
            &  " (expected)"
         )  );
      elsif Other (1..Last) /= Expected then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Get fault ["
            &  Image (Sequence)
            &  "] "
            &  Image (Other (1..Last))
            &  "/="
            &  Image (Expected)
            &  " (expected)"
         )  );
      end if;
      Feed (Title, Sequence, Item);
      if Take (Item) /= Expected then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Get Feed fault ["
            &  Image  (Sequence)
            &  "] "
            &  Image (Take (Item))
            &  "/="
            &  Image (Expected)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Get ["
            &  Image (Sequence)
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_OID_Get;

   procedure Test_OID_Value
             (  Title : String;
                Value : Strings_Edit.Object_Identifiers.
                        Object_Identifier
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;
      use Strings_Edit.Object_Identifiers;
      Client    : External_OID_Machine;
      Other     : Object_Identifier := Value;
      Item      : OID_Data_Item (127);
      Buffer    : Stream_Element_Array (1..1024);
      Pointer_1 : Stream_Element_Offset := 1;
      Pointer_2 : Stream_Element_Offset;
      Last      : Integer;
   begin
      Set_Value (Item, Value);
      Encode (Item, Buffer, Pointer_1);
      Pointer_2 := Buffer'First + 2;
      Get (Buffer, Pointer_2, Pointer_1 - 3, Other, Last);
      if Other (1..Last) /= Value then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value fault ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Image (Other (1..Last))
            &  "/="
            &  Image (Value)
            &  " (expected)"
         )  );
      end if;
      Feed (Title, Buffer (Buffer'First..Pointer_1 - 1), Item);
      if Take (Item) /= Value then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value Feed fault ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Image (Take (Item))
            &  "/="
            &  Image (Value)
            &  " (expected)"
         )  );
      end if;
      Feed (Client, Title, Buffer (Buffer'First..Pointer_1 - 1));
      if Get_Value (Client.OID) /= Value then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value external Feed fault ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Image (Get_Value (Client.OID))
            &  "/="
            &  Image (Value)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Value ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_OID_Value;
   ---------------------------------------------------------------------
   procedure Test_Relative_OID_Get
             (  Title    : String;
                Sequence : Stream_Element_Array;
                Expected : Strings_Edit.Object_Identifiers.
                           Object_Identifier
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;
      use Strings_Edit.Object_Identifiers;
      Item    : Relative_OID_Data_Item (127);
      Other   : Object_Identifier     := Expected;
      Pointer : Stream_Element_Offset := Sequence'First + 1;
      Last    : Integer;
   begin
      Get_Relative (Sequence, Pointer, Sequence'Length, Other, Last);
      if Pointer /= Sequence'Last + 1 then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Get fault [ "
            &  Image (Sequence)
            &  "] Pointer"
            &  Stream_Element_Offset'Image (Pointer)
            &  " /="
            &  Stream_Element_Offset'Image (Sequence'Last + 1)
            &  " (expected)"
         )  );
      elsif Other (1..Last) /= Expected then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Get fault ["
            &  Image (Sequence)
            &  "] "
            &  Image (Other (1..Last))
            &  "/="
            &  Image (Expected)
            &  " (expected)"
         )  );
      end if;
      Feed (Title, Sequence, Item);
      if Take (Item) /= Expected then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Get Feed fault ["
            &  Image  (Sequence)
            &  "] "
            &  Image (Take (Item))
            &  "/="
            &  Image (Expected)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Get ["
            &  Image (Sequence)
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Relative_OID_Get;

   procedure Test_Relative_OID_Value
             (  Title : String;
                Value : Strings_Edit.Object_Identifiers.
                        Object_Identifier
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;
      use Strings_Edit.Object_Identifiers;
      Other     : Object_Identifier := Value;
      Item      : Relative_OID_Data_Item (127);
      Buffer    : Stream_Element_Array (1..1024);
      Pointer_1 : Stream_Element_Offset := 1;
      Pointer_2 : Stream_Element_Offset;
      Last      : Integer;
   begin
      Set_Value (Item, Value);
      Encode (Item, Buffer, Pointer_1);
      Pointer_2 := Buffer'First + 2;
      Get_Relative (Buffer, Pointer_2, Pointer_1 - 3, Other, Last);
      if Other (1..Last) /= Value then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value fault ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Image (Other (1..Last))
            &  "/="
            &  Image (Value)
            &  " (expected)"
         )  );
      end if;
      Feed (Title, Buffer (Buffer'First..Pointer_1 - 1), Item);
      if Take (Item) /= Value then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value Feed fault ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Image (Take (Item))
            &  "/="
            &  Image (Value)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Value ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Relative_OID_Value;
   ---------------------------------------------------------------------
   procedure Test_Sequence_Of_Values (Sequence : Integer_32_Array) is
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Sequences;
      Title   : constant String := "ASN.1 sequence of integer 32";
      Client  : Sequence_Of_Integers_Machine;
      Buffer  : Stream_Element_Array (1..1024*8);
      Pointer : Stream_Element_Offset;
      Start   : Stream_Element_Offset;
   begin
      Pointer := Buffer'First;
      Buffer (Pointer) := 16#30#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#80#; Pointer := Pointer + 1;
      for Index in Sequence'Range loop
         Buffer (Pointer) := 16#02#;
         Pointer := Pointer + 2;
         Start   := Pointer;
         Put (Buffer, Pointer, Sequence (Index));
         Buffer (Start - 1) := Stream_Element (Pointer - Start);
      end loop;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Feed
      (  Client,
         Title & " (indefinite {" & Image (Sequence) & "})",
         Buffer (Buffer'First..Pointer - 1)
      );
      if Get (Client.Data) /= Sequence then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value sequence of integer 32 Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Image (Get (Client.Data))
            &  "/="
            &  Image (Sequence)
            &  " (expected)"
         )  );
      end if;
      declare
         Length : Stream_Element_Offset;
      begin
         Pointer := Buffer'First;
         for Index in Sequence'Range loop
            Buffer (Pointer) := 16#02#;
            Pointer := Pointer + 2;
            Start   := Pointer;
            Put (Buffer, Pointer, Sequence (Index));
            Buffer (Start - 1) := Stream_Element (Pointer - Start);
         end loop;
         Length := Pointer - Buffer'First;
         Pointer := Buffer'First;
         Buffer (Pointer) := 16#30#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, Length);
         for Index in Sequence'Range loop
            Buffer (Pointer) := 16#02#;
            Pointer := Pointer + 2;
            Start   := Pointer;
            Put (Buffer, Pointer, Sequence (Index));
            Buffer (Start - 1) := Stream_Element (Pointer - Start);
         end loop;
      end;
      Feed
      (  Client,
         Title & " (definite {" & Image (Sequence) & "})",
         Buffer (Buffer'First..Pointer - 1)
      );
      if Get (Client.Data) /= Sequence then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value sequence of integer 32 Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Image (Get (Client.Data))
            &  "/="
            &  Image (Sequence)
            &  " (expected)"
         )  );
      end if;
      Pointer := Buffer'First;
      Encode (Client.Data, Buffer, Pointer);
      Feed
      (  Client,
         Title & " (definite encoded {" & Image (Sequence) & "})",
         Buffer (Buffer'First..Pointer - 1)
      );
      if Get (Client.Data) /= Sequence then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value sequence of integer 32 Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Image (Get (Client.Data))
            &  "/="
            &  Image (Sequence)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in sequence of integer 32 Test_Value ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Sequence_Of_Values;

   procedure Test_Tag_Value (Title : String; Value : ASN1.ASN1_Type) is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      Other       : Tag_Type;
      Tag         : constant Tag_Type := (Application_Tag, Value, True);
      Constructed : Boolean;
      Buffer      : Stream_Element_Array (1..1024);
      Pointer_1   : Stream_Element_Offset;
      Pointer_2   : Stream_Element_Offset;
   begin
      Pointer_1 := Buffer'First;
      Put (Buffer, Pointer_1, Tag, True);
      Pointer_2 := Buffer'First;
      Get (Buffer, Pointer_2, Other, Constructed);
      if Other /= Tag then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Tag_Value fault ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Image (Other)
            &  "/="
            &  Image (Value)
            &  " (expected)"
         )  );
      elsif not Constructed then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Tag_Value fault ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Boolean'Image (Constructed)
            &  "/="
            &  Boolean'Image (True)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Value ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Tag_Value;
   ---------------------------------------------------------------------
   procedure Test_Tagged_Name (No : ASN1.ASN1_Type; Value : String) is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
      Title   : constant String   := "ASN.1 tagged string";
      Tag     : constant Tag_Type := (Application_Tag, No, False);
      Client  : Tagged_Name_Machine;
      Buffer  : Stream_Element_Array (1..1024);
      Pointer : Stream_Element_Offset;
   begin
      Pointer := Buffer'First;
      Put (Buffer, Pointer, Tag, False);
      Buffer (Pointer) := Value'Length;
      Pointer := Pointer + 1;
      Put (Buffer, Pointer, Value);
      Set_Implicit_Tag
      (  Client.Data.Value,
         (Application_Tag, Octet_String_Tag, False),
         Value'Length
      );
      Feed
      (  Client,
         Title & " (" & "[" & Image (Tag) & "] " & Value,
         Buffer (Buffer'First..Pointer - 1)
      );
      if Get_Value (Client.Data.Value) /= Value then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Tagged_Name value fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Quote (Get_Value (Client.Data.Value))
            &  "/="
            &  Quote (Value)
            &  " (expected)"
         )  );
      elsif Client.Data.Tag /= Tag then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Tagged_Name tag fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Quote (Get_Value (Client.Data.Value))
            &  "/="
            &  Quote (Value)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Tagged_Name ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Tagged_Name;
   ---------------------------------------------------------------------
   procedure Test_Tagged_Sequence is
      use Ada.Calendar;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Sequences;
      Title   : constant String := "ASN.1 tagged sequence";
      Client  : Four_Integers_Machine;
      Buffer  : Stream_Element_Array (1..1024);
      Pointer : Stream_Element_Offset;

      procedure Check_1234 is
      begin
         if not Is_Set (Client.Data, 1) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] A is not set"
            )  );
         elsif not Is_Set (Client.Data, 2) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] B is not set"
            )  );
         elsif not Is_Set (Client.Data, 3) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] C is not set"
            )  );
         elsif not Is_Set (Client.Data, 4) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] D is not set"
            )  );
         elsif Client.Data.A.Value /= 1 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] A "
               &  Image (Client.Data.A.Value)
               &  "/= 1 (expected)"
            )  );
         elsif Client.Data.B.Value /= 2 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] B "
               &  Image (Client.Data.B.Value)
               &  "/= 2 (expected)"
            )  );
         elsif Client.Data.C.Value /= 3 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] C "
               &  Image (Client.Data.C.Value)
               &  "/= 3 (expected)"
            )  );
         elsif Client.Data.D.Value /= 4 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] D "
               &  Image (Client.Data.B.Value)
               &  "/= 4 (expected)"
            )  );
         end if;
      end Check_1234;

      procedure Check_1_34 is
      begin
         if not Is_Set (Client.Data, 1) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] A is not set"
            )  );
         elsif Is_Set (Client.Data, 2) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] B is set"
            )  );
         elsif not Is_Set (Client.Data, 3) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] C is not set"
            )  );
         elsif not Is_Set (Client.Data, 4) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] D is not set"
            )  );
         elsif Client.Data.A.Value /= 1 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] A "
               &  Image (Client.Data.A.Value)
               &  "/= 1 (expected)"
            )  );
         elsif Client.Data.B.Value /= 99 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] B "
               &  Image (Client.Data.B.Value)
               &  "/= 2 (expected)"
            )  );
         elsif Client.Data.C.Value /= 3 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] C "
               &  Image (Client.Data.C.Value)
               &  "/= 3 (expected)"
            )  );
         elsif Client.Data.D.Value /= 4 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Tagged_Sequence Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] D "
               &  Image (Client.Data.B.Value)
               &  "/= 4 (expected)"
            )  );
         end if;
      end Check_1_34;

      procedure Put_1234 is
         use GNAT.Sockets.Connection_State_Machine.ASN1;
      begin
         Put (Buffer, Pointer, (Context_Specific_Tag, 0, False), False);
         Buffer (Pointer) := 16#03#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#02#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 1);
         Put (Buffer, Pointer, (Context_Specific_Tag, 1, False), False);
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 2);
         Put (Buffer, Pointer, (Context_Specific_Tag, 2, False), False);
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 3);
         Put (Buffer, Pointer, (Context_Specific_Tag, 3, False), False);
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 4);
      end Put_1234;

      procedure Put_1_34 is
         use GNAT.Sockets.Connection_State_Machine.ASN1;
      begin
         Put (Buffer, Pointer, (Context_Specific_Tag, 0, False), False);
         Buffer (Pointer) := 16#03#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#02#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 1);
         Client.Data.B.Value := 99;
         Set_Optional (Client.Data, 2, True);
         Put (Buffer, Pointer, (Context_Specific_Tag, 2, False), False);
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 3);
         Put (Buffer, Pointer, (Context_Specific_Tag, 3, False), False);
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 4);
      end Put_1_34;

      procedure Put_Universal_1234 is
      begin
         Set_Tag (Client.Data, 1, (ASN1.Universal_Tag, 0, False));
         Set_Tag (Client.Data, 2, (ASN1.Universal_Tag, 0, False));
         Set_Tag (Client.Data, 3, (ASN1.Universal_Tag, 0, False));
         Set_Tag (Client.Data, 4, (ASN1.Universal_Tag, 0, False));
         Buffer (Pointer) := 16#02#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#03#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#02#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 1);
         Buffer (Pointer) := 16#02#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 2);
         Buffer (Pointer) := 16#02#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 3);
         Buffer (Pointer) := 16#02#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#01#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, 4);
      end Put_Universal_1234;

   begin
      Pointer := Buffer'First;
      Buffer (Pointer) := 16#30#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#80#; Pointer := Pointer + 1;
      Put_1234;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Feed
      (  Client,
         Title & " (indefinite {1,2,3,4})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_1234;

      Pointer := Buffer'First + 2;
      Put_1234;
      Buffer (Buffer'First + 1) :=
         Stream_Element (Pointer - Buffer'First - 2);
      Feed
      (  Client,
         Title & " (definite {1,2,3,4})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_1234;

      Pointer := Buffer'First + 2;
      Put_1_34;
      Buffer (Buffer'First + 1) :=
         Stream_Element (Pointer - Buffer'First - 2);
      Feed
      (  Client,
         Title & " (definite {1,3,4})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_1_34;

      Pointer := Buffer'First + 2;
      Put_Universal_1234;
      Buffer (Buffer'First + 1) :=
         Stream_Element (Pointer - Buffer'First - 2);
      Feed
      (  Client,
         Title & " (definite universal {1,2,3,4})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_1234;

   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in sequence Test_Value ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Tagged_Sequence;
   ---------------------------------------------------------------------
   procedure Test_Set is
      use Ada.Calendar;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Sets;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
      Title   : constant String := "ASN.1 sets";
      Client  : Assorted_Set_Machine;
      Buffer  : Stream_Element_Array (1..1024);
      Pointer : Stream_Element_Offset;

      procedure Check_Smith_2 is
      begin
         if not Is_Set (Client.Data, 1) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] Name is not set"
            )  );
         elsif Is_Set (Client.Data, 2) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] A is set"
            )  );
         elsif not Is_Set (Client.Data, 3) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] B is not set"
            )  );
         elsif Get_Value (Client.Data.Name) /= "Smith" then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] Name "
               &  Quote (Get_Value (Client.Data.Name))
               &  "/= ""Smith"" (expected)"
            )  );
         elsif Client.Data.A.Value /= 99 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] A "
               &  Image (Client.Data.A.Value)
               &  "/= 99 (expected)"
            )  );
         elsif Client.Data.B.Value /= 2 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] B "
               &  Image (Client.Data.B.Value)
               &  "/= 2 (expected)"
            )  );
         end if;
      end Check_Smith_2;

      procedure Check_Smith_1_2 is
      begin
         if not Is_Set (Client.Data, 1) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] Name is not set"
            )  );
         elsif not Is_Set (Client.Data, 2) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] A is not set"
            )  );
         elsif not Is_Set (Client.Data, 3) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] B is not set"
            )  );
         elsif Get_Value (Client.Data.Name) /= "Smith" then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] Name "
               &  Quote (Get_Value (Client.Data.Name))
               &  "/= ""Smith"" (expected)"
            )  );
         elsif Client.Data.A.Value /= 1 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] A "
               &  Image (Client.Data.A.Value)
               &  "/= 1 (expected)"
            )  );
         elsif Client.Data.B.Value /= 2 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] B "
               &  Image (Client.Data.B.Value)
               &  "/= 2 (expected)"
            )  );
         end if;
      end Check_Smith_1_2;

      procedure Check_None is
      begin
         if Is_Set (Client.Data, 1) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set none Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] Name is set"
            )  );
         elsif Is_Set (Client.Data, 2) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set none Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] A is set"
            )  );
         elsif Is_Set (Client.Data, 3) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set none Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] B is set"
            )  );
         elsif Get_Value (Client.Data.Name) /= "yyy" then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set none Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] Name "
               &  Quote (Get_Value (Client.Data.Name))
               &  "/= ""yyy"" (expected)"
            )  );
         elsif Client.Data.A.Value /= 888 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set none Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] A "
               &  Image (Client.Data.A.Value)
               &  "/= 888 (expected)"
            )  );
         elsif Client.Data.B.Value /= 999 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Set none Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] B "
               &  Image (Client.Data.B.Value)
               &  "/= 999 (expected)"
            )  );
         end if;
      end Check_None;

      procedure Put_Smith_1_2 is
         use GNAT.Sockets.Connection_State_Machine.ASN1;
      begin
         Put (Buffer, Pointer, (Context_Specific_Tag, 2, False), False);
         Put (Buffer, Pointer, 1);
         Put (Buffer, Pointer, 2);
         Put (Buffer, Pointer, (Context_Specific_Tag, 0, False), False);
         Put (Buffer, Pointer, 5);
         Put (Buffer, Pointer, String'("Smith"));
         Put (Buffer, Pointer, (Context_Specific_Tag, 1, False), False);
         Put (Buffer, Pointer, 1);
         Put (Buffer, Pointer, 1);
      end Put_Smith_1_2;

      procedure Put_Smith_2 is
         use GNAT.Sockets.Connection_State_Machine.ASN1;
      begin
         Put (Buffer, Pointer, (Context_Specific_Tag, 2, False), False);
         Put (Buffer, Pointer, 1);
         Put (Buffer, Pointer, 2);
         Put (Buffer, Pointer, (Context_Specific_Tag, 0, False), False);
         Put (Buffer, Pointer, 5);
         Put (Buffer, Pointer, String'("Smith"));
      end Put_Smith_2;

   begin
      Set_Optional (Client.Data, 1, True);
      Set_Optional (Client.Data, 2, True);
      Set_Optional (Client.Data, 3, True);
      Client.Data.Name.Length := 3;
      Client.Data.Name.Value (1..3) := "yyy";
      Client.Data.A.Value := 888;
      Client.Data.B.Value := 999;

      Pointer := Buffer'First;
      Buffer (Pointer) := 16#31#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#80#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Feed
      (  Client,
         Title & " (indefinite empty)",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_None;

      Pointer := Buffer'First + 2;
      Buffer (Buffer'First + 1) :=
         Stream_Element (Pointer - Buffer'First - 2);
      Feed
      (  Client,
         Title & " (definite empty)",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_None;
      ------------------------------------------------------------------
      Pointer := Buffer'First;
      Buffer (Pointer) := 16#31#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#80#; Pointer := Pointer + 1;
      Put_Smith_1_2;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Feed
      (  Client,
         Title & " (indefinite {Smith,1,2})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_Smith_1_2;

      Pointer := Buffer'First + 2;
      Put_Smith_1_2;
      Buffer (Buffer'First + 1) :=
         Stream_Element (Pointer - Buffer'First - 2);
      Feed
      (  Client,
         Title & " (definite {Smith,1,2})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_Smith_1_2;
      ------------------------------------------------------------------
      Set_Optional (Client.Data, 1, True);
      Set_Optional (Client.Data, 2, True);
      Set_Optional (Client.Data, 3, True);
      Client.Data.Name.Length := 3;
      Client.Data.Name.Value (1..3) := "xxx";
      Client.Data.A.Value := 99;
      Client.Data.B.Value := 999;

      Pointer := Buffer'First;
      Buffer (Pointer) := 16#31#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#80#; Pointer := Pointer + 1;
      Put_Smith_2;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Feed
      (  Client,
         Title & " (indefinite {Smith,2})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_Smith_2;

      Pointer := Buffer'First + 2;
      Put_Smith_2;
      Buffer (Buffer'First + 1) :=
         Stream_Element (Pointer - Buffer'First - 2);
      Feed
      (  Client,
         Title & " (definite {Smith,2})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_Smith_2;

   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in set Test_Set ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Set;
   ---------------------------------------------------------------------
   procedure Test_Choice is
      use Ada.Calendar;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
      Title   : constant String := "ASN.1 choice";
      Client  : Alternatives_Record_Machine;
      Buffer  : Stream_Element_Array (1..1024);
      Pointer : Stream_Element_Offset;

      procedure Check_Smith is
      begin
         if Get_Selected (Client.Data) /= 1 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Choice Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] Name is not selected (selected "
               &  Image (Get_Selected (Client.Data))
               &  ")"
            )  );
         elsif Get_Value (Client.Data.Name) /= "Smith" then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Choice Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] Name "
               &  Quote (Get_Value (Client.Data.Name))
               &  "/= ""Smith"" (expected)"
            )  );
         end if;
      end Check_Smith;

      procedure Check_123 is
      begin
         if Get_Selected (Client.Data) /= 2 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Choice Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] ID is not selected (selected "
               &  Image (Get_Selected (Client.Data))
               &  ")"
            )  );
         elsif Client.Data.ID.Value /= 123 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Choice Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] ID "
               &  Image (Client.Data.ID.Value)
               &  "/= 123 (expected)"
            )  );
         end if;
      end Check_123;

      procedure Check_Unexpected is
      begin
         if Get_Selected (Client.Data) /= 0 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Choice Feed fault ["
               &  Image  (Buffer (1..Pointer - 1))
               &  "] ID is selected "
               &  Image (Get_Selected (Client.Data))
            )  );
         end if;
      end Check_Unexpected;

      procedure Put_Smith is
         use GNAT.Sockets.Connection_State_Machine.ASN1;
      begin
         Put (Buffer, Pointer, (Context_Specific_Tag, 0, False), False);
         Put (Buffer, Pointer, 5);
         Put (Buffer, Pointer, String'("Smith"));
         Client.Data.Name.Length := 0;
         Client.Data.ID.Value    := 99;
      end Put_Smith;

      procedure Put_123 is
         use GNAT.Sockets.Connection_State_Machine.ASN1;
      begin
         Put (Buffer, Pointer, (Context_Specific_Tag, 1, False), False);
         Put (Buffer, Pointer, 1);
         Put (Buffer, Pointer, 123);
         Client.Data.Name.Length := 0;
         Client.Data.ID.Value    := 99;
      end Put_123;

      procedure Put_Unexpected_Implicit_Definite is
         use GNAT.Sockets.Connection_State_Machine.ASN1;
      begin
         Put (Buffer, Pointer, (Application_Tag, 0, False), False);
         Put (Buffer, Pointer, 5);
         Put (Buffer, Pointer, String'("Smith"));
         Client.Data.Name.Length := 0;
         Client.Data.ID.Value    := 99;
      end Put_Unexpected_Implicit_Definite;

      procedure Put_Unexpected_Implicit_Indefinite is
         use GNAT.Sockets.Connection_State_Machine.ASN1;
      begin
         Put (Buffer, Pointer, (Application_Tag, 0, False), False);
         Buffer (Pointer) := 16#80#; Pointer := Pointer + 1;
         Put (Buffer, Pointer, String'("Smith"));
         Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
         Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      end Put_Unexpected_Implicit_Indefinite;

   begin
      Pointer := Buffer'First;
      Put_Smith;
      Feed
      (  Client,
         Title & " ([0]{Smith})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_Smith;
      ------------------------------------------------------------------
      Pointer := Buffer'First;
      Put_123;
      Feed
      (  Client,
         Title & " ([1]{123})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_123;
      ------------------------------------------------------------------
      Enable_Unsolicited (Client.Data, True);
      Pointer := Buffer'First;
      Put_Unexpected_Implicit_Definite;
      Feed
      (  Client,
         Title & " ([APPLICATION 0]{Smith})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_Unexpected;
      ------------------------------------------------------------------
      Enable_Unsolicited (Client.Data, True);
      Pointer := Buffer'First;
      Put_Unexpected_Implicit_Indefinite;
      Feed
      (  Client,
         Title & " ([APPLICATION 0]{Smith})",
         Buffer (Buffer'First..Pointer - 1)
      );
      Check_Unexpected;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in set Test_Set ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Choice;
   ---------------------------------------------------------------------
   procedure Test_Shared_Strings is
      use GNAT.Sockets.Connection_State_Machine.ASN1.Sequences;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;
      Title   : constant String := "ASN.1 shared string";
      Client  : Shared_Strings_Record_Machine;
      Buffer  : Stream_Element_Array (1..1024);
      Pointer : Stream_Element_Offset := 1;
      Start   : Stream_Element_Offset;
   begin
      Start := Pointer;
      Put (Buffer, Pointer, String'("ABCD"));
      Set_Implicit_Tag
      (  Client.Data.A,
         (ASN1.Application_Tag, ASN1.Octet_String_Tag, False),
         Pointer - Start
      );
      Start := Pointer;
      Put (Buffer, Pointer, String'("123456789"));
      Set_Implicit_Tag
      (  Client.Data.B,
         (ASN1.Application_Tag, ASN1.Octet_String_Tag, False),
         Pointer - Start
      );
      Start := Pointer;
      Put (Buffer, Pointer, String'("xyz"));
      Set_Implicit_Tag
      (  Client.Data.C,
         (ASN1.Application_Tag, ASN1.Octet_String_Tag, False),
         Pointer - Start
      );
      Set_Implicit_Tag
      (  Client.Data,
         (ASN1.Application_Tag, ASN1.Octet_String_Tag, False),
         Pointer - 1
      );
      Feed
      (  Client,
         Title & " ({""ABCD"",""123456789"",""xyz""})",
         Buffer (Buffer'First..Pointer - 1)
      );
      if Get_Value (Client.Data.A) /= "ABCD" then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Shared_Strings Feed fault A="
            &  Quote (Get_Value (Client.Data.A))
            &  " /= "
            &  Quote ("ABCD")
            &  " (expected)"
         )  );
      elsif Get_Value (Client.Data.B) /= "123456789" then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Shared_Strings Feed fault B="
            &  Quote (Get_Value (Client.Data.B))
            &  " /= "
            &  Quote ("123456789")
            &  " (expected)"
         )  );
      elsif Get_Value (Client.Data.C) /= "xyz" then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Shared_Strings Feed fault C="
            &  Quote (Get_Value (Client.Data.C))
            &  " /= "
            &  Quote ("xyz")
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in set Test_Shared_Strings ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Shared_Strings;
   ---------------------------------------------------------------------
   procedure Test_External_Set_Of_Values
             (  Sequence : Integer_32_Array
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Sets;
      Title   : constant String := "ASN.1 external set of integer 32";
      Client  : Set_Of_Machine;
      Buffer  : Stream_Element_Array (1..1024*8);
      Pointer : Stream_Element_Offset;
      Start   : Stream_Element_Offset;
   begin
      Pointer := Buffer'First;
      Buffer (Pointer) := Stream_Element (ASN1.Set_Tag);
      Pointer := Pointer + 1;
      Buffer (Pointer) := 16#80#; Pointer := Pointer + 1;
      for Index in Sequence'Range loop
         Buffer (Pointer) := 16#02#;
         Pointer := Pointer + 2;
         Start   := Pointer;
         Put (Buffer, Pointer, Sequence (Index));
         Buffer (Start - 1) := Stream_Element (Pointer - Start);
      end loop;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Buffer (Pointer) := 16#00#; Pointer := Pointer + 1;
      Feed
      (  Client,
         Title & " (indefinite {" & Image (Sequence) & "})",
         Buffer (Buffer'First..Pointer - 1)
      );
      if Get (Client.Data) /= Sequence then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value set of integer 32 Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Image (Get (Client.Data))
            &  "/="
            &  Image (Sequence)
            &  " (expected)"
         )  );
      end if;
      declare
         Length : Stream_Element_Offset;
      begin
         Pointer := Buffer'First;
         for Index in Sequence'Range loop
            Buffer (Pointer) := 16#02#;
            Pointer := Pointer + 2;
            Start   := Pointer;
            Put (Buffer, Pointer, Sequence (Index));
            Buffer (Start - 1) := Stream_Element (Pointer - Start);
         end loop;
         Length := Pointer - Buffer'First;
         Pointer := Buffer'First;
         Buffer (Pointer) := Stream_Element (ASN1.Set_Tag);
         Pointer := Pointer + 1;
         Put (Buffer, Pointer, Length);
         for Index in Sequence'Range loop
            Buffer (Pointer) := 16#02#;
            Pointer := Pointer + 2;
            Start   := Pointer;
            Put (Buffer, Pointer, Sequence (Index));
            Buffer (Start - 1) := Stream_Element (Pointer - Start);
         end loop;
      end;
      Feed
      (  Client,
         Title & " (definite {" & Image (Sequence) & "})",
         Buffer (Buffer'First..Pointer - 1)
      );
      if Get (Client.Data) /= Sequence then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value set of integer 32 Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Image (Get (Client.Data))
            &  "/="
            &  Image (Sequence)
            &  " (expected)"
         )  );
      end if;
      Pointer := Buffer'First;
      Encode (Client.Data, Buffer, Pointer);
      Feed
      (  Client,
         Title & " (definite encoded {" & Image (Sequence) & "})",
         Buffer (Buffer'First..Pointer - 1)
      );
      if Get (Client.Data) /= Sequence then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Value sequence of integer 32 Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Image (Get (Client.Data))
            &  "/="
            &  Image (Sequence)
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in sequence of integer 32 Test_Value ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_External_Set_Of_Values;
   ---------------------------------------------------------------------
   procedure Test_Reference (Value : Interfaces.Integer_32) is
      use type Interfaces.Integer_32;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
      use GNAT.Sockets.Connection_State_Machine.ASN1.Sets;
      Title   : constant String := "ASN.1 reference to integer 32";
      Client  : Reference_Machine;
      Buffer  : Stream_Element_Array (1..1024*8);
      Pointer : Stream_Element_Offset;
      Start   : Stream_Element_Offset;
   begin
      Pointer := Buffer'First;
      Buffer (Pointer) := Stream_Element (ASN1.Integer_Tag);
      Pointer := Pointer + 2;
      Start   := Pointer;
      Put (Buffer, Pointer, Value);
      Buffer (Start - 1) := Stream_Element (Pointer - Start);
      Feed
      (  Client,
         Title & " reference to" & Interfaces.Integer_32'Image (Value),
         Buffer (Buffer'First..Pointer - 1)
      );
      if Get (Client.Data) /= Value then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Test_Reference integer 32 Feed fault ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "]"
            &  Interfaces.Integer_32'Image (Get (Client.Data))
            &  "/="
            &  Image (Integer (Value))
            &  " (expected)"
         )  );
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in reference to integer 32 Test_Reference ["
            &  Image  (Buffer (1..Pointer - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Reference;
   ---------------------------------------------------------------------
   function Equal (Left, Right : Stream_Element_Array)
      return Boolean is
      function Do_Compare (Short, Long : Stream_Element_Array)
         return Boolean is
      begin
         if (  Long (Long'Last + 1 - Short'Length..Long'Last)
            /= Short
            )  then
            return False;
         end if;
         for Index in Long'First..Long'Last - Short'Length loop
            if Long (Index) /= 0 then
               return False;
            end if;
         end loop;
         return True;
      end Do_Compare;
   begin
      if Left'Length >= Right'Length then
         return Do_Compare (Right, Left);
      else
         return Do_Compare (Left, Right);
      end if;
   end Equal;

   procedure Test_Indefinite_Unsigned
             (  Value     : Stream_Element_Array;
                Test_Feed : Boolean := True
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      use GNAT.Sockets.Connection_State_Machine.ASN1.
          Indefinite_Unsigneds;
      Title     : constant String := "ASN.1 indefinite unsigned";
      Client    : Shared_Indefinite_Unsigned_Record_Machine;
      Buffer    : Stream_Element_Array (1..1024);
      Pointer_1 : Stream_Element_Offset := 1;
      Pointer_2 : Stream_Element_Offset;
   begin
      Buffer (Buffer'First) := Stream_Element (Integer_Tag);
      Pointer_1 := Buffer'First + 2;
      Put (Buffer, Pointer_1, Value);
      Buffer (2) := Stream_Element (Pointer_1 - 3);
      declare
         Other : Stream_Element_Array
                 (  1
                 .. Stream_Element_Offset (Buffer (2))
                 );
      begin
         Pointer_2 := Buffer'First + 2;
         Get (Buffer, Pointer_2, Other);
         if not Equal (Other, Value) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Indefinite_Unsigned fault ["
               &  Image  (Buffer (1..Pointer_1 - 1))
               &  "] "
               &  Image (Other)
               &  "/="
               &  Image (Value)
               &  " (expected)"
            )  );
         end if;
      end;
      if Test_Feed then
         Feed
         (  Title,
            Buffer (Buffer'First..Pointer_1 - 1),
            Client.Data
         );
         if not Equal (Get_Value (Client.Data), Value) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Indefinite_Unsigned Feed fault ["
               &  Image  (Buffer (1..Pointer_1 - 1))
               &  "] "
               &  Image (Get_Value (Client.Data))
               &  "/="
               &  Image (Value)
               &  " (expected)"
            )  );
         end if;
         Feed (Client, Title, Buffer (Buffer'First..Pointer_1 - 1));
         if not Equal (Get_Value (Client.Data), Value) then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Test_Indefinite_Unsigned Feed fault ["
--               &  Image  (Buffer (1..Pointer_1 - 1))
--               &  "] "
               &  Image (Get_Value (Client.Data))
               &  "/="
               &  Image (Value)
               &  " (expected)"
            )  );
         end if;
      end if;
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Indefinite_Unsigned ["
            &  Image  (Buffer (1..Pointer_1 - 1))
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Indefinite_Unsigned;

   procedure Test_X509 (Data : Stream_Element_Array) is
      use GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates;
      Title  : constant String := "X.509 certificate";
      Client : X509_Certificate_Machine;
   begin
      Feed_Flat (Client, Title, Data);
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_X509 ["
            &  Image  (Data)
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_X509;

   procedure Test_Any (Data : Stream_Element_Array) is
      use GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates;
      Title  : constant String := "Any";
      Client : Any_Machine;
   begin
      Feed_Flat (Client, Title, Data);
      Put (Client.Any, "");
   exception
      when Status_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  Title
            &  " exception in Test_Any ["
            &  Image  (Data)
            &  "] "
            &  Exception_Information (Error)
         )  );
   end Test_Any;

   use Interfaces;

begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
-- if false then
--  null;
-- end if;
   Encoding_Test_Lengths;
   Boolean_Test.Test_Get   ("ASN.1 Boolean", (1, 1, 12), True);
   Boolean_Test.Test_Get   ("ASN.1 Boolean", (1, 16#81#, 1, 0), False);
   Boolean_Test.Test_Value ("ASN.1 Boolean", True);
   Boolean_Test.Test_Value ("ASN.1 Boolean", False);
   Integer_8_Test.Test_Get ("ASN.1 Integer_8", (2, 1, 12), 12);
   Integer_8_Test.Test_Get ("ASN.1 Integer_8", (2, 1, 255), -1);
   Integer_8_Test.Test_Get ("ASN.1 Integer_8", (2, 16#81#, 1, 12), 12);
   Integer_8_Test.Test_Get ("ASN.1 Integer_8", (2, 16#81#, 1, 255), -1);
   Integer_8_Test.Test_Value ("ASN.1 Integer_8",  127);
   Integer_8_Test.Test_Value ("ASN.1 Integer_8",   12);
   Integer_8_Test.Test_Value ("ASN.1 Integer_8",    1);
   Integer_8_Test.Test_Value ("ASN.1 Integer_8",    0);
   Integer_8_Test.Test_Value ("ASN.1 Integer_8",   -1);
   Integer_8_Test.Test_Value ("ASN.1 Integer_8",  -33);
   Integer_8_Test.Test_Value ("ASN.1 Integer_8", -128);
   Integer_Test.Test_Get ("ASN.1 Integer", (2, 1, 12), 12);
   Integer_Test.Test_Get ("ASN.1 Integer", (2, 1, 255), -1);
   Integer_Test.Test_Get ("ASN.1 Integer", (2, 16#81#, 1, 12), 12);
   Integer_Test.Test_Get ("ASN.1 Integer", (2, 16#81#, 1, 255), -1);
   Integer_Test.Test_Value ("ASN.1 Integer", Integer'Last);
   Integer_Test.Test_Value ("ASN.1 Integer", 12345);
   Integer_Test.Test_Value ("ASN.1 Integer", 0);
   Integer_Test.Test_Value ("ASN.1 Integer", -1);
   Integer_Test.Test_Value ("ASN.1 Integer", -127);
   Integer_Test.Test_Value ("ASN.1 Integer", -128);
   Integer_Test.Test_Value ("ASN.1 Integer", -255);
   Integer_Test.Test_Value ("ASN.1 Integer", -256);
   Integer_Test.Test_Value ("ASN.1 Integer", -257);
   Integer_Test.Test_Value ("ASN.1 Integer", -12346);
   Integer_Test.Test_Value ("ASN.1 Integer", Integer'First);
   Integer_Test.Test_Value ("ASN.1 Integer", Integer'First + 256);
   Integer_Test.Test_Value ("ASN.1 Integer", Integer'First + 255);
   Integer_Test.Test_Value ("ASN.1 Integer", Integer'First + 254);
   Integer_Test.Test_Value ("ASN.1 Integer", Integer'First + 1);
   Integer_32_Test.Test_Value ("ASN.1 Integer_32", Integer_32'Last);
   Integer_32_Test.Test_Value ("ASN.1 Integer_32", Integer_32'Last - 1);
   Integer_32_Test.Test_Value ("ASN.1 Integer_32", Integer_32'Last - 2);
   Integer_32_Test.Test_Value ("ASN.1 Integer_32", Integer_32'Last - 3);
   Integer_32_Test.Test_Value ("ASN.1 Integer_32", 1);
   Integer_32_Test.Test_Value ("ASN.1 Integer_32", 0);
   Integer_32_Test.Test_Value ("ASN.1 Integer_32", -1);
   Integer_32_Test.Test_Value ("ASN.1 Integer_32", Integer_32'First +2);
   Integer_32_Test.Test_Value ("ASN.1 Integer_32", Integer_32'First +1);
   Integer_32_Test.Test_Value ("ASN.1 Integer_32", Integer_32'First);
   Integer_64_Test.Test_Value ("ASN.1 Integer_64", Integer_64'Last);
   Integer_64_Test.Test_Value ("ASN.1 Integer_64", Integer_64'Last - 1);
   Integer_64_Test.Test_Value ("ASN.1 Integer_64", Integer_64'Last - 2);
   Integer_64_Test.Test_Value ("ASN.1 Integer_64", Integer_64'Last - 3);
   Integer_64_Test.Test_Value ("ASN.1 Integer_64", 1);
   Integer_64_Test.Test_Value ("ASN.1 Integer_64", 0);
   Integer_64_Test.Test_Value ("ASN.1 Integer_64", -1);
   Integer_64_Test.Test_Value ("ASN.1 Integer_64", Integer_64'First +2);
   Integer_64_Test.Test_Value ("ASN.1 Integer_64", Integer_64'First +1);
   Integer_64_Test.Test_Value ("ASN.1 Integer_64", Integer_64'First);
   Unsigned_8_Test.Test_Get ("ASN.1 Unsigned_8", (2, 1, 12), 12);
   Unsigned_8_Test.Test_Get ("ASN.1 Unsigned_8", (2, 1, 255), 255);
   Unsigned_8_Test.Test_Get ("ASN.1 Unsigned_8", (2, 16#81#,1,12), 12);
   Unsigned_8_Test.Test_Get ("ASN.1 Unsigned_8", (2, 16#81#,1,255),255);
   Unsigned_8_Test.Test_Value ("ASN.1 Unsigned_8", 255);
   Unsigned_8_Test.Test_Value ("ASN.1 Unsigned_8",  12);
   Unsigned_8_Test.Test_Value ("ASN.1 Unsigned_8",   1);
   Unsigned_8_Test.Test_Value ("ASN.1 Unsigned_8",   0);
   Unsigned_16_Test.Test_Get ("ASN.1 Unsigned_16", (2, 1,12), 12);
   Unsigned_16_Test.Test_Get ("ASN.1 Unsigned_16", (2, 1,255), 255);
   Unsigned_16_Test.Test_Get ("ASN.1 Unsigned_16", (2, 16#81#,1,12),12);
   Unsigned_16_Test.Test_Value ("ASN.1 Unsigned_16", 255);
   Unsigned_16_Test.Test_Value ("ASN.1 Unsigned_16",  12);
   Unsigned_16_Test.Test_Value ("ASN.1 Unsigned_16",   0);
   Unsigned_16_Test.Test_Value ("ASN.1 Unsigned_16",   1);
   Unsigned_16_Test.Test_Value ("ASN.1 Unsigned_16", Unsigned_16'Last);
   Unsigned_64_Test.Test_Get ("ASN.1 Unsigned_64", (2, 1, 12), 12);
   Unsigned_64_Test.Test_Get ("ASN.1 Unsigned_64", (2, 1, 255), 255);
   Unsigned_64_Test.Test_Get ("ASN.1 Unsigned_64", (2, 16#81#,1,12),12);
   Unsigned_64_Test.Test_Value ("ASN.1 Unsigned_64", 255);
   Unsigned_64_Test.Test_Value ("ASN.1 Unsigned_64",  12);
   Unsigned_64_Test.Test_Value ("ASN.1 Unsigned_64",   0);
   Unsigned_64_Test.Test_Value ("ASN.1 Unsigned_64",   1);
   Unsigned_64_Test.Test_Value ("ASN.1 Unsigned_64", Unsigned_64'Last);
   Character_Test.Test_Get ("ASN.1 Character", (10, 1, 35), '#');
   Character_Test.Test_Get ("ASN.1 Character", (10, 16#81#,1,35), '#');
   Character_Test.Test_Value ("ASN.1 Character", Character'Last);
   Character_Test.Test_Value ("ASN.1 Character", 'A');
   Character_Test.Test_Value ("ASN.1 Character", Character'First);
   Float_Test.Test_Value ("ASN.1 Real", 0.0);
   Float_Test.Test_Value ("ASN.1 Real", 1.0);
   Float_Test.Test_Value ("ASN.1 Real", 123.0);
   Float_Test.Test_Value ("ASN.1 Real", -123.0);
   Float_Test.Test_Value ("ASN.1 Real", 23.0E-10);
   Float_Test.Test_Value ("ASN.1 Real", 0.123);
   Float_Test.Test_Value ("ASN.1 Real", Float'First);
   Float_Test.Test_Value ("ASN.1 Real", Float'Last);
   Float_Test.Test_Get ("ASN.1 Real", ASN1_Real ("1"), 1.0);
   Float_Test.Test_Get ("ASN.1 Real", ASN1_Real ("-123"), -123.0);
   Float_Test.Test_Get ("ASN.1 Real", ASN1_Real ("-124.3"), -124.3);
   Float_Test.Test_Get ("ASN.1 Real", ASN1_Real ("1.3E-10"), 1.3E-10);
   Float_Test.Test_Get ("ASN.1 Real", ASN1_Real (".13E-10"), 1.3E-11);
   Generalized_Time_Test.Test_Get
   (  "ASN.1 Generalized time",
      ASN1_Generalized ("1998052814Z"),
      Ada.Calendar.Formatting.Time_Of (1998, 05, 28, 14, 0, 0)
   );
   Generalized_Time_Test.Test_Get
   (  "ASN.1 Generalized time",
      ASN1_Generalized ("20120102123001.25+0100"),
      Ada.Calendar.Formatting.Time_Of
      (  Year       => 2012,
         Month      => 01,
         Day        => 02,
         Hour       => 12,
         Minute     => 30,
         Second     => 1,
         Sub_Second => 0.25,
         Time_Zone  => 60
   )  );
   Generalized_Time_Test.Test_Value
   (  "ASN.1 Generalized time",
      Ada.Calendar.Clock
   );
   UTC_Test.Test_Get
   (  "ASN.1 UTC",
      ASN1_UTC ("98052814Z"),
      Ada.Calendar.Formatting.Time_Of (1998, 05, 28, 14, 0, 0)
   );
   UTC_Test.Test_Value
   (  "ASN.1 UTC",
      Ada.Calendar.Clock
   );
   Date_Test.Test_Get
   (  "ASN.1 Date",
      ASN1_Date ("1998-05-28"),
      Ada.Calendar.Time_Of (1998, 05, 28, 0.0)
   );
   Date_Test.Test_Value
   (  "ASN.1 Date",
      Ada.Calendar.Clock
   );
   Date_Time_Test.Test_Get
   (  "ASN.1 Date time",
      ASN1_Date_Time ("1998-05-28T10:30:00"),
      Ada.Calendar.Time_Of (1998, 05, 28, (10.0 * 60.0 + 30.0) * 60.0)
   );
   Date_Time_Test.Test_Value
   (  "ASN.1 Date time",
      Ada.Calendar.Clock
   );
   Time_Of_Day_Test.Test_Get
   (  "ASN.1 Time of day",
      ASN1_Time_Of_Day ("10:10:01"),
      10.0 * 3_600.0 + 10.0 * 60.0 + 1.0
   );
   Time_Of_Day_Test.Test_Value
   (  "ASN.1 Time of day",
      10.0 * 3_600.0 + 10.0 * 60.0 + 1.0
   );
   Duration_Test.Test_Get
   (  "ASN.1 Duration",
      ASN1_Duration ("P1W"),
      7.0 * 24.0 * 60.0 * 60.0
   );
   Duration_Test.Test_Get
   (  "ASN.1 Duration",
      ASN1_Duration ("PT0S"),
      0.0
   );
   Duration_Test.Test_Get
   (  "ASN.1 Duration",
      ASN1_Duration ("P1.5D"),
      1.5 * 24.0 * 60.0 * 60.0
   );
   Duration_Test.Test_Value
   (  "ASN.1 Duration",
      10.0 * 3_600.0 + 10.0 * 60.0 + 1.0
   );
   Duration_Test.Test_Value
   (  "ASN.1 Duration",
      13456789.0
   );
   Time_Test.Test_Get
   (  "ASN.1 Time",
      ASN1_Time ("1998-05-28"),
      Ada.Calendar.Time_Of (1998, 05, 28, 0.0)
   );
   Time_Test.Test_Value
   (  "ASN.1 Time",
      Ada.Calendar.Clock
   );
   Time_Test.Test_Get
   (  "ASN.1 Time",
      ASN1_Time ("2009-W01-1"),
      Ada.Calendar.Time_Of (2008, 12, 29, 0.0)
   );
   Test_Value ((1..0 => True));
   Test_Value ((1 => True));
   Test_Value ((True, False));
   Test_Value ((True,  False,  True,  True, True, False));
   Test_Value ((False,  True, False,  True, True, True, False));
   Test_Value ((False, False,  True, False, True, True, True, False));
   Test_Value ((1..8 => False));
   Test_Value ((1..9 => False));
   Test_Value ((1..200 => True));
   Test_Value (  Bits
                 ( +"01 02 03 04 05 06 07 08"
                   +"09 10 11 12 13 14 15 16"
              )  );
   Test_Bit_Value_Decode
   (                                        +"03 82 01"
     +"0f 00 30 82 01 0a 02 82 01 01 00 c7 ff be a1 64"
     +"06 47 e1 c4 95 0a 65 59 6b 91 c4 a8 a0 a3 08 aa"
     +"dc 3f 00 44 0a cf 41 4b ed e2 6f 03 78 c7 d3 67"
     +"14 d3 fe b4 27 85 1a 02 57 97 b5 86 16 87 97 82"
     +"0d e8 2b ed 69 16 c3 96 a6 c4 47 b8 5d c1 76 a9"
     +"02 9d 28 52 c9 d9 93 f1 b2 da 18 7c 0a 78 bd 1c"
     +"3f 12 4f e5 3f ee 5b b7 b3 3c a5 b6 55 65 c6 33"
     +"09 b0 45 a4 49 f5 d8 f6 54 25 2e db 4b c3 65 64"
     +"86 16 bf 73 8e 78 e8 25 cc c1 b4 7d d1 b1 55 20"
     +"a4 ae de b8 41 ee 43 9a 48 33 d4 12 39 a0 a8 fb"
     +"91 4f 47 bb 3b d6 75 3b 62 a6 c5 5a d7 7e 36 10"
     +"bc 3a 0d 46 e3 de c1 df 50 d0 94 77 22 17 a0 51"
     +"9b af dc 34 60 26 1d a0 04 ac a5 06 04 57 64 1a"
     +"d1 4f 1c 19 f8 be 4b 68 12 0f 5f c3 12 40 64 bb"
     +"c6 a6 84 d9 5a b0 78 1d 1b 11 c1 54 e6 15 d1 af"
     +"2b e2 57 2b 6d b9 c4 5d b7 01 74 57 a7 fb 27 85"
     +"2b 92 15 e2 e3 13 97 29 c8 92 7b 02 03 01 00 01"
   );
   Test_Value ((1..0 => True), (1..0 => True));
   Test_Value ((1..1 => True), (1..1 => False));
   Test_Value
   (  (1..0 => True),
      (False, False,  True, False, True, True, True, False)
   );
   Test_Value
   (  (False, False,  True, False, True, True, True, False),
      (1..0 => True)
   );
   Test_String_Value ("");
   Test_String_Value ("A");
   Test_String_Value ("ABC");
   Test_String_Value ("", "");
   Test_String_Value ("", "A");
   Test_String_Value ("A", "");
   Test_String_Value ("A", "A");
   Test_Wide_String_Value ("", "");
   Test_Wide_String_Value ("", "A");
   Test_Wide_String_Value ("A", "");
   Test_Wide_String_Value ("A", "A");
   Test_Shared_Strings;
   Test_OID_Value ("ASN.1 OID", (1,2));
   Test_OID_Value
   (  "ASN.1 OID",
      Strings_Edit.Object_Identifiers.Value ("1.2")
   );
   Test_OID_Value ("ASN.1 OID", (1,3,6,1,4,1,311,21,20));
   Test_OID_Value
   (  "ASN.1 OID",
      Strings_Edit.Object_Identifiers.Value ("1.3.6.1.4.1.311.21.20")
   );
   Test_Relative_OID_Value ("ASN.1 Relative OID", (1,2));
   Test_Relative_OID_Value
   (  "ASN.1 Relative OID",
      Strings_Edit.Object_Identifiers.Value ("1.2")
   );
   Test_Relative_OID_Value
   (  "ASN.1 Relative OID",
      (1,3,6,1,4,1,311,21,20)
   );
   Test_Relative_OID_Value
   (  "ASN.1 Relative OID",
      Strings_Edit.Object_Identifiers.Value ("1.3.6.1.4.1.311.21.20")
   );
   Test_Indefinite_Unsigned ((1..0 => 0));
   Test_Indefinite_Unsigned ((1 => 1));
   Test_Indefinite_Unsigned ((1 => 127));
   Test_Indefinite_Unsigned ((0, 255));
   Test_Indefinite_Unsigned ((1, 2));
   Test_Indefinite_Unsigned ((127, 2));
   Test_Indefinite_Unsigned ((0, 255, 0, 1, 2));
   Test_Value ("Smith", True, Ada.Calendar.Clock);
   Test_Sequence_Of_Values ((1..0 => 0));
   Test_Sequence_Of_Values ((1 => 9));
   Test_Sequence_Of_Values ((1,2,3,4,5,6,7,8,9));
   Test_Sequence_Of_Values ((1..1 => 123456));
   Test_Sequence_Of_Values ((1..5 => 123456));
   Test_Sequence_Of_Values ((1..10 => 123456));
   Test_Sequence_Of_Values ((1..26 => 123456));
   Test_Sequence_Of_Values ((1..512 => 123456));
   Test_Tag_Value ("ASN.1 Tag", 0);
   Test_Tag_Value ("ASN.1 Tag", 1);
   Test_Tag_Value ("ASN.1 Tag", 30);
   Test_Tag_Value ("ASN.1 Tag", 31);
   Test_Tag_Value ("ASN.1 Tag", 32);
   Test_Tag_Value ("ASN.1 Tag", 123);
   Test_Tag_Value ("ASN.1 Tag", 99_9999);
   Test_Tagged_Name (20, "Smith");
   Test_Tagged_Sequence;
   Test_Set;
   Test_Choice;
   Test_External_Set_Of_Values ((1..0 => 0));
   Test_External_Set_Of_Values ((1 => 9));
   Test_External_Set_Of_Values ((1,2,3,4,5,6,7,8,9));
   Test_External_Set_Of_Values ((1..1 => 123456));
   Test_External_Set_Of_Values ((1..5 => 123456));
   Test_External_Set_Of_Values ((1..10 => 123456));
   Test_External_Set_Of_Values ((1..26 => 123456));
   Test_External_Set_Of_Values ((1..512 => 123456));
   Test_Reference (0);
   Test_Reference (1);
   Test_Reference (1234);
   Test_X509
   ( +"30 82 05 E0"
     +"   30 82 04 C8"
     +"      A0 03 02 01 02"
     +"      02 10 0C 00 93 10 D2 06 DB E3 37 55 35 80 11 8D DC 87"
     +"      30 0D"
     +"         06 09 2A 86 48 86 F7 0D 01 01 0B"
     +"         05 00"
     +"      30 75"
     +"         31 0B"
     +"            30 09"
     +"               06 03 55 04 06"
     +"               13 02 55 53"
     +"         31 15"
     +"            30 13"
     +"               06 03 55 04 0A"
     +"               13 0C 44 69 67 69 43 65 72 74 20 49 6E 63"
     +"         31 19"
     +"            30 17"
     +"               06 03 55 04 0B"
     +"               13 10 77 77 77 2E 64  69 67 69 63 65"
     +"                     72 74 2E 63 6F  6D"
     +"         31 34"
     +"            30 32"
     +"               06 03 55 04 03"
     +"               13 2B 44 69 67 69 43  65 72 74 20 53"
     +"                     48 41 32 20 45  78 74 65 6E 64"
     +"                     65 64 20 56 61  6C 69 64 61 74"
     +"                     69 6F 6E 20 53  65 72 76 65 72"
     +"                     20 43 41"
     +"      30 1E"
     +"         17 0D 31 34 30 34 30 38 30 30 30 30 30 30 5A"
     +"         17 0D 31 36 30 34 31 32 31 32 30 30 30 30 5A"
     +"      30 81 F0"
     +"         31 1D"
     +"            30 1B"
     +"               06 03 55 04 0F"
     +"               0C 14 50 72 69 76 61  74 65 20 4F 72"
     +"                     67 61 6E 69 7A  61 74 69 6F 6E"
     +"         31 13"
     +"            30 11"
     +"               06 0B 2B 06 01 04 01 82 37 3C 02 01 03"
     +"               13 02 55 53"
     +"         31 19"
     +"            30 17"
     +"               06 0B 2B 06 01 04 01 82 37 3C 02 01 02"
     +"               13 08 44 65 6C 61 77 61 72 65"
     +"         31 10"
     +"            30 0E"
     +"               06 03 55 04 05"
     +"               13 07 35 31 35 37 35 35 30"
     +"         31 17"
     +"            30 15"
     +"               06 03 55 04 09"
     +"               13 0E 35 34 38 20 34 74 68 20 53 74 72 65 65 74"
     +"         31 0E"
     +"            30 0C"
     +"               06 03 55 04 11"
     +"               13 05 39 34 31 30 37"
     +"         31 0B"
     +"            30 09"
     +"               06 03 55 04 06"
     +"               13 02 55 53"
     +"         31 13"
     +"            30 11"
     +"               06 03 55 04 08"
     +"               13 0A 43 61 6C 69 66 6F 72 6E 69 61"
     +"         31 16"
     +"            30 14"
     +"               06 03 55 04 07"
     +"               13 0D 53 61 6E 20 46 72 61 6E 63 69 73 63 6F"
     +"         31 15"
     +"            30 13"
     +"               06 03 55 04 0A"
     +"               13 0C 47 69 74 48 75 62 2C 20 49 6E 63 2E"
     +"         31 13"
     +"            30 11"
     +"               06 03 55 04 03"
     +"               13 0A 67 69 74 68 75 62 2E 63 6F 6D"
     +"      30 82 01 22"
     +"         30 0D"
     +"            06 09 2A 86 48 86 F7 0D 01 01 01"
     +"            05 00"
     +"         03 82 01 0F 00 30 82 01 0A 02  82 01 01 00 B1"
     +"                        D4 DC 3C AF FD  F3 4E ED C1 67"
     +"                        AD E6 CB 22 E8  B7 E2 AB 28 F2"
     +"                        F7 DC 62 70 08  D1 0C AF D6 16"
     +"                        6A 21 B0 36 4B  17 0D 36 63 04"
     +"                        AE BF EA 20 51  95 65 66 F2 BF"
     +"                        B9 4D A4 0C 29  EB F5 15 B1 E8"
     +"                        35 B3 70 10 94  D5 1B 59 B4 26"
     +"                        0F D6 83 57 59  9D E1 7C 09 DD"
     +"                        E0 13 CA 4D 6F  43 9B CD CF 87"
     +"                        3A 15 A7 85 DD  66 83 ED 93 0C"
     +"                        FE 2B 6D 38 1C  79 88 90 CF AD"
     +"                        58 18 2D 51 D1  C2 A3 F2 47 8C"
     +"                        6F 38 09 B9 B8  EF 4C 93 0B CB"
     +"                        83 94 87 EA E0  A3 B5 D9 7B 9B"
     +"                        6B 0F 43 F9 CA  EE 80 0D 28 A7"
     +"                        76 F1 25 F4 C1  35 3C F6 74 AD"
     +"                        DE 6A 33 82 7B  DC FD 4B 76 A7"
     +"                        C2 EE F2 6A BF  A9 24 A6 5F E7"
     +"                        2E 7C 0E DB C3  74 73 FA 7E C6"
     +"                        D8 CF 60 EB 36  56 21 B6 C1 8A"
     +"                        B8 24 82 4D 78  24 BA E9 1D A1"
     +"                        8A A7 87 BE 66  25 69 BF BE 3B"
     +"                        72 6E 4F E0 E4  85 25 08 B1 91"
     +"                        89 B8 D6 74 65  76 9B 2C 4F 62"
     +"                        1F A1 FA 3A BE  9C 24 BF 9F CA"
     +"                        B0 C5 C0 67 8D  02 03 01 00 01"
     +"      A3 82 01 EE"
     +"         30 82 01 EA"
     +"            30 1F"
     +"               06 03 55 1D 23"
     +"               04 18 30 16 80 14 3D  D3 50 A5 D6 A0"
     +"                     AD EE F3 4A 60  0A 65 D3 21 D4"
     +"                     F8 F8 D6 0F"
     +"            30 1D"
     +"               06 03 55 1D 0E"
     +"               04 16 04 14 6A 43 90  7D 3B 98 14 72"
     +"                     52 95 3A AA 28  0A 43 F8 51 7E"
     +"                     D3 A6"
     +"            30 25"
     +"               06 03 55 1D 11"
     +"               04 1E 30 1C 82 0A 67  69 74 68 75 62"
     +"                     2E 63 6F 6D 82  0E 77 77 77 2E"
     +"                     67 69 74 68 75  62 2E 63 6F 6D"
     +"            30 0E"
     +"               06 03 55 1D 0F"
     +"               01 01 FF"
     +"               04 04 03 02 05 A0"
     +"            30 1D"
     +"               06 03 55 1D 25"
     +"               04 16 30 14 06 08 2B  06 01 05 05 07"
     +"                     03 01 06 08 2B  06 01 05 05 07"
     +"                     03 02"
     +"            30 75"
     +"               06 03 55 1D 1F"
     +"               04 6E 30 6C 30 34 A0  32 A0 30 86 2E"
     +"                     68 74 74 70 3A  2F 2F 63 72 6C"
     +"                     33 2E 64 69 67  69 63 65 72 74"
     +"                     2E 63 6F 6D 2F  73 68 61 32 2D"
     +"                     65 76 2D 73 65  72 76 65 72 2D"
     +"                     67 31 2E 63 72  6C 30 34 A0 32"
     +"                     A0 30 86 2E 68  74 74 70 3A 2F"
     +"                     2F 63 72 6C 34  2E 64 69 67 69"
     +"                     63 65 72 74 2E  63 6F 6D 2F 73"
     +"                     68 61 32 2D 65  76 2D 73 65 72"
     +"                     76 65 72 2D 67  31 2E 63 72 6C"
     +"            30 42"
     +"               06 03 55 1D 20"
     +"               04 3B 30 39 30 37 06  09 60 86 48 01"
     +"                     86 FD 6C 02 01  30 2A 30 28 06"
     +"                     08 2B 06 01 05  05 07 02 01 16"
     +"                     1C 68 74 74 70  73 3A 2F 2F 77"
     +"                     77 77 2E 64 69  67 69 63 65 72"
     +"                     74 2E 63 6F 6D  2F 43 50 53"
     +"            30 81 88"
     +"               06 08 2B 06 01 05 05 07 01 01"
     +"               04 7C 30 7A 30 24 06 08  2B 06 01 05 05"
     +"                        07 30 01 86 18  68 74 74 70 3A"
     +"                        2F 2F 6F 63 73  70 2E 64 69 67"
     +"                        69 63 65 72 74  2E 63 6F 6D 30"
     +"                        52 06 08 2B 06  01 05 05 07 30"
     +"                        02 86 46 68 74  74 70 3A 2F 2F"
     +"                        63 61 63 65 72  74 73 2E 64 69"
     +"                        67 69 63 65 72  74 2E 63 6F 6D"
     +"                        2F 44 69 67 69  43 65 72 74 53"
     +"                        48 41 32 45 78  74 65 6E 64 65"
     +"                        64 56 61 6C 69  64 61 74 69 6F"
     +"                        6E 53 65 72 76  65 72 43 41 2E"
     +"                        63 72 74"
     +"            30 0C"
     +"               06 03 55 1D 13"
     +"               01 01 FF"
     +"               04 02 30 00"
     +"      30 0D"
     +"         06 09 2A 86 48 86 F7 0D 01 01 0B"
     +"         05 00"
     +"         03 82 01 01 00 6F E7 6D CB  82 F3 EF 90 87"
     +"                     09 D7 0F 15 22  2C 8C FE D3 AB"
     +"                     1C 8A 96 DB 5D  12 5D D1 78 C0"
     +"                     31 B0 FF 45 C8  89 F7 08 98 52"
     +"                     17 1F 4C 4B 20  64 6A 6D DB 50"
     +"                     D7 10 BE 7E AB  FE 2F 80 D8 A9"
     +"                     4A 58 41 69 81  72 19 08 83 9B"
     +"                     92 10 4E 62 2D  7B 46 70 43 6E"
     +"                     A3 53 13 1F E2  93 A6 23 5B F7"
     +"                     92 3E 37 14 75  3B B9 4B 24 41"
     +"                     2E A5 3D 48 0D  0F 99 EA 1E 42"
     +"                     97 C6 FE 95 DA  AB 47 9A CB 2B"
     +"                     03 D6 0D 40 C1  0A F7 78 1A DA"
     +"                     B5 83 A4 AD B5  99 49 20 2E F8"
     +"                     93 3C 1E 6C 3D  D1 3B 23 3A 6B"
     +"                     38 2A 7E 62 7A  5F DD 17 05 75"
     +"                     D0 24 5D BE 8D  A8 9A 10 44 FA"
     +"                     D2 B4 CA EF D7  D0 B5 76 A5 26"
     +"                     25 1C 08 41 D8  64 92 A7 AF 7D"
     +"                     FE 88 40 39 61  0B C0 48 30 A9"
     +"                     82 34 AD F7 70  46 03 7C 35 91"
     +"                     3A D5 BB 24 D8  01 BC 14 F0 C3"
     +"                     0F 23 3B 58 32  BA 0F 12 6C 66"
     +"                     7A 6D 9D E4 F0  E5 7C 5D 7E 02"
     +"                     D8 D7 AC 89 97  0B 61 B7 36 9F"
     +"                     B0 7D 3B EE B7  33 69"
   );
   Test_Any
   ( +"30 81 87"
     +"   02 01 00"
     +"   30 13"
     +"      06 07   2A 86 48 CE 3D 02 01"
     +"      06 08   2A 86 48 CE 3D 03 01 07"
     +"   04 6D"
     +"      30 6B"
     +"         02 01 01"
     +"         04 20 89 8C 9D A3 6E DA 34 63"
     +"               BD 0C 15 16 78 61 0F 0F"
     +"               CA 0D 8B 52 D9 0E A7 F1"
     +"               3E 64 25 89 E2 C1 8D 54"
     +"         A1 44"
     +"            03 42 00"
     +"                  04 21 3D 52 58 BC 6C 69 C3"
     +"                     E2 13 96 75 EA 39 28 A4"
     +"                     09 FC FF EF 39 AC C8 E0"
     +"                     C8 2A 24 AE 78 C3 7E DE"
     +"                     98 FD 89 C0 E0 0E 74 C9"
     +"                     97 BB 0A 71 6C A9 E0 DC"
     +"                     A6 73 DB B9 B3 FA 72 96"
     +"                     22 55 C9 DE BC D2 18 CA"
   );
   Test_Any
   ( +"30 82 05 E0"
     +"   30 82 04 C8"
     +"      A0 03 02 01 02"
     +"      02 10 0C 00 93 10 D2 06 DB E3 37 55 35 80 11 8D DC 87"
     +"      30 0D"
     +"         06 09 2A 86 48 86 F7 0D 01 01 0B"
     +"         05 00"
     +"      30 75"
     +"         31 0B"
     +"            30 09"
     +"               06 03 55 04 06"
     +"               13 02 55 53"
     +"         31 15"
     +"            30 13"
     +"               06 03 55 04 0A"
     +"               13 0C 44 69 67 69 43 65 72 74 20 49 6E 63"
     +"         31 19"
     +"            30 17"
     +"               06 03 55 04 0B"
     +"               13 10 77 77 77 2E 64  69 67 69 63 65"
     +"                     72 74 2E 63 6F  6D"
     +"         31 34"
     +"            30 32"
     +"               06 03 55 04 03"
     +"               13 2B 44 69 67 69 43  65 72 74 20 53"
     +"                     48 41 32 20 45  78 74 65 6E 64"
     +"                     65 64 20 56 61  6C 69 64 61 74"
     +"                     69 6F 6E 20 53  65 72 76 65 72"
     +"                     20 43 41"
     +"      30 1E"
     +"         17 0D 31 34 30 34 30 38 30 30 30 30 30 30 5A"
     +"         17 0D 31 36 30 34 31 32 31 32 30 30 30 30 5A"
     +"      30 81 F0"
     +"         31 1D"
     +"            30 1B"
     +"               06 03 55 04 0F"
     +"               0C 14 50 72 69 76 61  74 65 20 4F 72"
     +"                     67 61 6E 69 7A  61 74 69 6F 6E"
     +"         31 13"
     +"            30 11"
     +"               06 0B 2B 06 01 04 01 82 37 3C 02 01 03"
     +"               13 02 55 53"
     +"         31 19"
     +"            30 17"
     +"               06 0B 2B 06 01 04 01 82 37 3C 02 01 02"
     +"               13 08 44 65 6C 61 77 61 72 65"
     +"         31 10"
     +"            30 0E"
     +"               06 03 55 04 05"
     +"               13 07 35 31 35 37 35 35 30"
     +"         31 17"
     +"            30 15"
     +"               06 03 55 04 09"
     +"               13 0E 35 34 38 20 34 74 68 20 53 74 72 65 65 74"
     +"         31 0E"
     +"            30 0C"
     +"               06 03 55 04 11"
     +"               13 05 39 34 31 30 37"
     +"         31 0B"
     +"            30 09"
     +"               06 03 55 04 06"
     +"               13 02 55 53"
     +"         31 13"
     +"            30 11"
     +"               06 03 55 04 08"
     +"               13 0A 43 61 6C 69 66 6F 72 6E 69 61"
     +"         31 16"
     +"            30 14"
     +"               06 03 55 04 07"
     +"               13 0D 53 61 6E 20 46 72 61 6E 63 69 73 63 6F"
     +"         31 15"
     +"            30 13"
     +"               06 03 55 04 0A"
     +"               13 0C 47 69 74 48 75 62 2C 20 49 6E 63 2E"
     +"         31 13"
     +"            30 11"
     +"               06 03 55 04 03"
     +"               13 0A 67 69 74 68 75 62 2E 63 6F 6D"
     +"      30 82 01 22"
     +"         30 0D"
     +"            06 09 2A 86 48 86 F7 0D 01 01 01"
     +"            05 00"
     +"         03 82 01 0F 00 30 82 01 0A 02  82 01 01 00 B1"
     +"                        D4 DC 3C AF FD  F3 4E ED C1 67"
     +"                        AD E6 CB 22 E8  B7 E2 AB 28 F2"
     +"                        F7 DC 62 70 08  D1 0C AF D6 16"
     +"                        6A 21 B0 36 4B  17 0D 36 63 04"
     +"                        AE BF EA 20 51  95 65 66 F2 BF"
     +"                        B9 4D A4 0C 29  EB F5 15 B1 E8"
     +"                        35 B3 70 10 94  D5 1B 59 B4 26"
     +"                        0F D6 83 57 59  9D E1 7C 09 DD"
     +"                        E0 13 CA 4D 6F  43 9B CD CF 87"
     +"                        3A 15 A7 85 DD  66 83 ED 93 0C"
     +"                        FE 2B 6D 38 1C  79 88 90 CF AD"
     +"                        58 18 2D 51 D1  C2 A3 F2 47 8C"
     +"                        6F 38 09 B9 B8  EF 4C 93 0B CB"
     +"                        83 94 87 EA E0  A3 B5 D9 7B 9B"
     +"                        6B 0F 43 F9 CA  EE 80 0D 28 A7"
     +"                        76 F1 25 F4 C1  35 3C F6 74 AD"
     +"                        DE 6A 33 82 7B  DC FD 4B 76 A7"
     +"                        C2 EE F2 6A BF  A9 24 A6 5F E7"
     +"                        2E 7C 0E DB C3  74 73 FA 7E C6"
     +"                        D8 CF 60 EB 36  56 21 B6 C1 8A"
     +"                        B8 24 82 4D 78  24 BA E9 1D A1"
     +"                        8A A7 87 BE 66  25 69 BF BE 3B"
     +"                        72 6E 4F E0 E4  85 25 08 B1 91"
     +"                        89 B8 D6 74 65  76 9B 2C 4F 62"
     +"                        1F A1 FA 3A BE  9C 24 BF 9F CA"
     +"                        B0 C5 C0 67 8D  02 03 01 00 01"
     +"      A3 82 01 EE"
     +"         30 82 01 EA"
     +"            30 1F"
     +"               06 03 55 1D 23"
     +"               04 18 30 16 80 14 3D  D3 50 A5 D6 A0"
     +"                     AD EE F3 4A 60  0A 65 D3 21 D4"
     +"                     F8 F8 D6 0F"
     +"            30 1D"
     +"               06 03 55 1D 0E"
     +"               04 16 04 14 6A 43 90  7D 3B 98 14 72"
     +"                     52 95 3A AA 28  0A 43 F8 51 7E"
     +"                     D3 A6"
     +"            30 25"
     +"               06 03 55 1D 11"
     +"               04 1E 30 1C 82 0A 67  69 74 68 75 62"
     +"                     2E 63 6F 6D 82  0E 77 77 77 2E"
     +"                     67 69 74 68 75  62 2E 63 6F 6D"
     +"            30 0E"
     +"               06 03 55 1D 0F"
     +"               01 01 FF"
     +"               04 04 03 02 05 A0"
     +"            30 1D"
     +"               06 03 55 1D 25"
     +"               04 16 30 14 06 08 2B  06 01 05 05 07"
     +"                     03 01 06 08 2B  06 01 05 05 07"
     +"                     03 02"
     +"            30 75"
     +"               06 03 55 1D 1F"
     +"               04 6E 30 6C 30 34 A0  32 A0 30 86 2E"
     +"                     68 74 74 70 3A  2F 2F 63 72 6C"
     +"                     33 2E 64 69 67  69 63 65 72 74"
     +"                     2E 63 6F 6D 2F  73 68 61 32 2D"
     +"                     65 76 2D 73 65  72 76 65 72 2D"
     +"                     67 31 2E 63 72  6C 30 34 A0 32"
     +"                     A0 30 86 2E 68  74 74 70 3A 2F"
     +"                     2F 63 72 6C 34  2E 64 69 67 69"
     +"                     63 65 72 74 2E  63 6F 6D 2F 73"
     +"                     68 61 32 2D 65  76 2D 73 65 72"
     +"                     76 65 72 2D 67  31 2E 63 72 6C"
     +"            30 42"
     +"               06 03 55 1D 20"
     +"               04 3B 30 39 30 37 06  09 60 86 48 01"
     +"                     86 FD 6C 02 01  30 2A 30 28 06"
     +"                     08 2B 06 01 05  05 07 02 01 16"
     +"                     1C 68 74 74 70  73 3A 2F 2F 77"
     +"                     77 77 2E 64 69  67 69 63 65 72"
     +"                     74 2E 63 6F 6D  2F 43 50 53"
     +"            30 81 88"
     +"               06 08 2B 06 01 05 05 07 01 01"
     +"               04 7C 30 7A 30 24 06 08  2B 06 01 05 05"
     +"                        07 30 01 86 18  68 74 74 70 3A"
     +"                        2F 2F 6F 63 73  70 2E 64 69 67"
     +"                        69 63 65 72 74  2E 63 6F 6D 30"
     +"                        52 06 08 2B 06  01 05 05 07 30"
     +"                        02 86 46 68 74  74 70 3A 2F 2F"
     +"                        63 61 63 65 72  74 73 2E 64 69"
     +"                        67 69 63 65 72  74 2E 63 6F 6D"
     +"                        2F 44 69 67 69  43 65 72 74 53"
     +"                        48 41 32 45 78  74 65 6E 64 65"
     +"                        64 56 61 6C 69  64 61 74 69 6F"
     +"                        6E 53 65 72 76  65 72 43 41 2E"
     +"                        63 72 74"
     +"            30 0C"
     +"               06 03 55 1D 13"
     +"               01 01 FF"
     +"               04 02 30 00"
     +"      30 0D"
     +"         06 09 2A 86 48 86 F7 0D 01 01 0B"
     +"         05 00"
     +"         03 82 01 01 00 6F E7 6D CB  82 F3 EF 90 87"
     +"                     09 D7 0F 15 22  2C 8C FE D3 AB"
     +"                     1C 8A 96 DB 5D  12 5D D1 78 C0"
     +"                     31 B0 FF 45 C8  89 F7 08 98 52"
     +"                     17 1F 4C 4B 20  64 6A 6D DB 50"
     +"                     D7 10 BE 7E AB  FE 2F 80 D8 A9"
     +"                     4A 58 41 69 81  72 19 08 83 9B"
     +"                     92 10 4E 62 2D  7B 46 70 43 6E"
     +"                     A3 53 13 1F E2  93 A6 23 5B F7"
     +"                     92 3E 37 14 75  3B B9 4B 24 41"
     +"                     2E A5 3D 48 0D  0F 99 EA 1E 42"
     +"                     97 C6 FE 95 DA  AB 47 9A CB 2B"
     +"                     03 D6 0D 40 C1  0A F7 78 1A DA"
     +"                     B5 83 A4 AD B5  99 49 20 2E F8"
     +"                     93 3C 1E 6C 3D  D1 3B 23 3A 6B"
     +"                     38 2A 7E 62 7A  5F DD 17 05 75"
     +"                     D0 24 5D BE 8D  A8 9A 10 44 FA"
     +"                     D2 B4 CA EF D7  D0 B5 76 A5 26"
     +"                     25 1C 08 41 D8  64 92 A7 AF 7D"
     +"                     FE 88 40 39 61  0B C0 48 30 A9"
     +"                     82 34 AD F7 70  46 03 7C 35 91"
     +"                     3A D5 BB 24 D8  01 BC 14 F0 C3"
     +"                     0F 23 3B 58 32  BA 0F 12 6C 66"
     +"                     7A 6D 9D E4 F0  E5 7C 5D 7E 02"
     +"                     D8 D7 AC 89 97  0B 61 B7 36 9F"
     +"                     B0 7D 3B EE B7  33 69"
   );
   Test_X509 -- Test_Any
   ( +"30 82 04 f7 30 82 03 df a0 03 02 01 02 02 09 00 82 ed bf 41 c8"
     +"80 91 9d 30 0d 06 09 2a 86 48 86 f7 0d 01 01 05 05 00 30 4d 31"
     +"0b 30 09 06 03 55 04 06 13 02 58 59 31 26 30 24 06 03 55 04 0a"
     +"0c 1d 50 79 74 68 6f 6e 20 53 6f 66 74 77 61 72 65 20 46 6f 75"
     +"6e 64 61 74 69 6f 6e 20 43 41 31 16 30 14 06 03 55 04 03 0c 0d"
     +"6f 75 72 2d 63 61 2d 73 65 72 76 65 72 30 1e 17 0d 31 38 30 31"
     +"31 39 31 39 30 39 30 36 5a 17 0d 32 37 31 31 32 38 31 39 30 39"
     +"30 36 5a 30 62 31 0b 30 09 06 03 55 04 06 13 02 58 59 31 17 30"
     +"15 06 03 55 04 07 0c 0e 43 61 73 74 6c 65 20 41 6e 74 68 72 61"
     +"78 31 23 30 21 06 03 55 04 0a 0c 1a 50 79 74 68 6f 6e 20 53 6f"
     +"66 74 77 61 72 65 20 46 6f 75 6e 64 61 74 69 6f 6e 31 15 30 13"
     +"06 03 55 04 03 0c 0c 66 61 6b 65 68 6f 73 74 6e 61 6d 65 30 82"
     +"01 22 30 0d 06 09 2a 86 48 86 f7 0d 01 01 01 05 00 03 82 01 0f"
     +"00 30 82 01 0a 02 82 01 01 00 c7 ff be a1 64 06 47 e1 c4 95 0a"
     +"65 59 6b 91 c4 a8 a0 a3 08 aa dc 3f 00 44 0a cf 41 4b ed e2 6f"
     +"03 78 c7 d3 67 14 d3 fe b4 27 85 1a 02 57 97 b5 86 16 87 97 82"
     +"0d e8 2b ed 69 16 c3 96 a6 c4 47 b8 5d c1 76 a9 02 9d 28 52 c9"
     +"d9 93 f1 b2 da 18 7c 0a 78 bd 1c 3f 12 4f e5 3f ee 5b b7 b3 3c"
     +"a5 b6 55 65 c6 33 09 b0 45 a4 49 f5 d8 f6 54 25 2e db 4b c3 65"
     +"64 86 16 bf 73 8e 78 e8 25 cc c1 b4 7d d1 b1 55 20 a4 ae de b8"
     +"41 ee 43 9a 48 33 d4 12 39 a0 a8 fb 91 4f 47 bb 3b d6 75 3b 62"
     +"a6 c5 5a d7 7e 36 10 bc 3a 0d 46 e3 de c1 df 50 d0 94 77 22 17"
     +"a0 51 9b af dc 34 60 26 1d a0 04 ac a5 06 04 57 64 1a d1 4f 1c"
     +"19 f8 be 4b 68 12 0f 5f c3 12 40 64 bb c6 a6 84 d9 5a b0 78 1d"
     +"1b 11 c1 54 e6 15 d1 af 2b e2 57 2b 6d b9 c4 5d b7 01 74 57 a7"
     +"fb 27 85 2b 92 15 e2 e3 13 97 29 c8 92 7b 02 03 01 00 01 a3 82"
     +"01 c3 30 82 01 bf 30 17 06 03 55 1d 11 04 10 30 0e 82 0c 66 61"
     +"6b 65 68 6f 73 74 6e 61 6d 65 30 0e 06 03 55 1d 0f 01 01 ff 04"
     +"04 03 02 05 a0 30 1d 06 03 55 1d 25 04 16 30 14 06 08 2b 06 01"
     +"05 05 07 03 01 06 08 2b 06 01 05 05 07 03 02 30 0c 06 03 55 1d"
     +"13 01 01 ff 04 02 30 00 30 1d 06 03 55 1d 0e 04 16 04 14 f8 76"
     +"79 cb 11 85 f0 46 e5 95 e6 7e 69 cb 12 5e 4e aa ec 4d 30 7d 06"
     +"03 55 1d 23 04 76 30 74 80 14 9a cf cf 6e eb 71 3d db 3c f1 ae"
     +"88 6b 56 72 03 cb 08 a7 48 a1 51 a4 4f 30 4d 31 0b 30 09 06 03"
     +"55 04 06 13 02 58 59 31 26 30 24 06 03 55 04 0a 0c 1d 50 79 74"
     +"68 6f 6e 20 53 6f 66 74 77 61 72 65 20 46 6f 75 6e 64 61 74 69"
     +"6f 6e 20 43 41 31 16 30 14 06 03 55 04 03 0c 0d 6f 75 72 2d 63"
     +"61 2d 73 65 72 76 65 72 82 09 00 82 ed bf 41 c8 80 91 9b 30 81"
     +"83 06 08 2b 06 01 05 05 07 01 01 04 77 30 75 30 3c 06 08 2b 06"
     +"01 05 05 07 30 02 86 30 68 74 74 70 3a 2f 2f 74 65 73 74 63 61"
     +"2e 70 79 74 68 6f 6e 74 65 73 74 2e 6e 65 74 2f 74 65 73 74 63"
     +"61 2f 70 79 63 61 63 65 72 74 2e 63 65 72 30 35 06 08 2b 06 01"
     +"05 05 07 30 01 86 29 68 74 74 70 3a 2f 2f 74 65 73 74 63 61 2e"
     +"70 79 74 68 6f 6e 74 65 73 74 2e 6e 65 74 2f 74 65 73 74 63 61"
     +"2f 6f 63 73 70 2f 30 43 06 03 55 1d 1f 04 3c 30 3a 30 38 a0 36"
     +"a0 34 86 32 68 74 74 70 3a 2f 2f 74 65 73 74 63 61 2e 70 79 74"
     +"68 6f 6e 74 65 73 74 2e 6e 65 74 2f 74 65 73 74 63 61 2f 72 65"
     +"76 6f 63 61 74 69 6f 6e 2e 63 72 6c 30 0d 06 09 2a 86 48 86 f7"
     +"0d 01 01 05 05 00 03 82 01 01 00 6d 50 8d fb ee 4e 93 8b eb 47"
     +"56 ba 38 cc 80 e1 9d c7 e1 9e 1f 9c 22 0c d2 08 9b ed bf 31 d9"
     +"00 ee af 8c 56 78 92 d1 7c ba 4e 81 7f 82 1f f4 68 99 86 91 c6"
     +"cb 57 d3 b9 41 12 fa 75 53 fd 22 32 21 50 af 6b 4c b1 34 36 d1"
     +"a8 25 0a d0 f0 f8 81 7d 69 58 6e af e3 d2 c4 32 87 79 d7 cd ad"
     +"0c 56 f3 15 27 10 0c f9 57 59 53 00 ed af 5d 4d 07 86 7a e5 f3"
     +"97 88 bc 86 b4 f1 17 46 33 55 28 66 7b 70 d3 a5 12 b9 4f c7 ed"
     +"e6 13 20 2d f0 9e ec 17 64 cf fd 13 14 1b 76 ba 64 ac c5 51 b6"
     +"cd 13 0a 93 b1 fd 43 09 a0 0b 44 6c 77 45 43 0b e5 ed 70 b2 76"
     +"dc 08 4a 5b 73 5f c1 fc 7f 63 70 f8 b9 ca 3c 98 06 5f fd 98 d1"
     +"e4 e6 61 5f 09 8f 6c 18 86 98 9c cb 3f 73 7b 3f 38 f5 a7 09 20"
     +"ee a5 63 1c ff 8b a6 d1 8c e8 f4 84 3d 99 38 0f cc e0 52 03 f9"
     +"18 05 23 76 39 de 52 ce 8e fb a6 6e f5 4f c3"
   );
   declare
      use Ada.Streams.Stream_IO;
      use GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates.
          Stream_IO;
      File : Ada.Streams.Stream_IO.File_Type;
      Data : X509_Certificate_Data (1024 * 10);
   begin
      Open
      (  File,
         In_File,
         "D:\mingw64\lib\python3.7\test\keycert4.pem"
      );
      Read (Stream (File).all, Data);
      Close (File);
      Put_Line ("Read certificate from file:");
      Put (Data.Certificate, "");
      Create
      (  File,
         Out_File,
         "C:\temp\keycert4.pem"
      );
      Write (Stream (File).all, Data);
      Close (File);
   end;
   declare
      Factory : aliased Data_Factory;
      Server  : Connections_Server (Factory'Access, Port);
   begin
      Put_Line ("Data server started ================================");
      Trace_On (Factory, Trace_Any, Trace_Any);
      delay 60.0 * Minutes; -- Service
      Put_Line ("Data server stopping");
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Data_Server;
