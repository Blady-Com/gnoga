--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Generic_Real                           Spring, 2019       --
--  Implementation                                                    --
--                                Last revision :  10:13 29 Nov 2020  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

with GNAT.Sockets.Connection_State_Machine.ASN1.Integers;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Integers;

with Ada.Unchecked_Conversion;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Generic_Real is
   use Edit;

   type Feed_Mode is
        (  Feed_Decimal,
           Feed_Header,
           Feed_Exponent_Length,
           Feed_Exponent_First,
           Feed_Exponent,
           Feed_Mantissa
        );
   type ASN1_Radix is (Binary, Octal, Hexadecimal);
   type Number_Part is
        (  Mantissa_Start,
           Before_Point,
           After_Point,
           Exponent_Start,
           Exponent
        );

   type Feed_State_Type (Mode : Feed_Mode := Feed_Header) is record
      case Mode is
         when Feed_Header =>
            null;
         when Feed_Exponent_Length..Feed_Mantissa =>
            Sign   : Boolean;
            Radix  : ASN1_Radix;
            Factor : Stream_Element range 0..3;
            case Mode is
               when Feed_Exponent_Length =>
                  null;
               when Feed_Exponent_First..Feed_Mantissa =>
                  Exponent : Integer_16;
                  case Mode is
                     when Feed_Exponent_First..Feed_Exponent =>
                        Exponent_Length : Stream_Element;
                     when Feed_Mantissa =>
                        Mantissa_Length : Stream_Element;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when Feed_Decimal =>
            State         : Number_Part;
            Mantissa_Sign : Boolean;
            Exponent_Sign : Boolean; -- Or non-empty mantissa
            Total_Length  : Stream_Element;
            Decimal_Point : Unsigned_8;
      end case;
   end record;
   for Feed_State_Type use record
      Mode            at 0 range  0..2;
      Sign            at 0 range  3..3;
      Radix           at 0 range  4..5;
      Factor          at 0 range  6..7;
      Exponent_Length at 0 range  8..15;
      Mantissa_Length at 0 range  8..15;
      Exponent        at 0 range 16..31;
      Mantissa_Sign   at 0 range  3..3;
      Exponent_Sign   at 0 range  4..4;
      State           at 0 range  5..7;
      Total_Length    at 0 range  8..15;
      Decimal_Point   at 0 range 16..31;
   end record;
   for Feed_State_Type'Size use Unsigned_32'Size;

   function From_State
            (  State : Stream_Element_Offset
            )  return Feed_State_Type is
      function Convert is
         new Ada.Unchecked_Conversion (Unsigned_32, Feed_State_Type);
   begin
      return Convert (Unsigned_32 (State));
   end From_State;

   function To_State
            (  Status : Feed_State_Type
            )  return Stream_Element_Offset is
      function Convert is
         new Ada.Unchecked_Conversion (Feed_State_Type, Unsigned_32);
   begin
      return Stream_Element_Offset (Convert (Status));
   end To_State;

   procedure Encode
             (  Item    : Implicit_Real_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put (Data, Pointer, Item.Value);
   end Encode;

   procedure Explicit_Encode is
      new Generic_Explicit_Encode (Implicit_Real_Data_Item);

   procedure Encode
             (  Item    : Real_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Encode (Implicit_Real_Data_Item (Item), Data, Pointer);
   end Encode;

   procedure Feed
             (  Item    : in out Real_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check (Data (Pointer), (1 => Real_Tag), True);
         Pointer := Pointer + 1;
         State   := Start_Length;
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      if State < 0 then
         while not Is_Length_Ready (State) loop
            if Pointer > Data'Last then
               return;
            end if;
            Embedded_Feed (Data, Pointer, State);
         end loop;
         if Is_Indefinite (State) then
            Raise_Exception (Data_Error'Identity, Wrong_Length);
         end if;
         Item.Value := Number (Get_Length (State));
         if Item.Value = 0.0 then
            State := 0;
            return;
         end if;
         State := To_State ((Mode => Feed_Header));
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      Feed
      (  Item    => Implicit_Real_Data_Item (Item),
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Real_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      Status : Feed_State_Type;
   begin
      if State = 0 then
         if Item.Value = 0.0 then
            State := 0;
            return;
         end if;
         Status := (Mode => Feed_Header);
         State  := To_State (Status);
      end if;
      Status := From_State (State);
      loop
         case Status.Mode is
            when Feed_Header =>
               declare
                  Length : constant Stream_Element :=
                                    Stream_Element (Item.Value) - 1;
                  Head   : constant Stream_Element :=
                                    Data (Pointer);
                  function Radix return ASN1_Radix is
                  begin
                     case Head and 2#0011_0000# is
                        when 2#0000_0000# =>
                           return Binary;
                        when 2#0001_0000# =>
                           return Octal;
                        when 2#0010_0000# =>
                           return Hexadecimal;
                        when others =>
                           Raise_Exception
                           (  Data_Error'Identity,
                              Real_Format_Reserved
                           );
                     end case;
                  end Radix;
               begin
                  Pointer := Pointer + 1;
                  if Head > 127 then -- Binary format
                     case Head and 2#11# is -- Exponent length
                        when 0 =>
                           if Length < 2 then
                              Raise_Exception
                              (  End_Error'Identity,
                                 Non_Terminated
                              );
                           end if;
                           Status :=
                              (  Mode   => Feed_Exponent_First,
                                 Sign   => 0 /= (Head and 2#0100_0000#),
                                 Radix  => Radix,
                                 Factor => (Head and 2#0000_1100#) / 4,
                                 Exponent => 0,
                                 Exponent_Length => 1
                              );
                        when 1 =>
                           if Length < 3 then
                              Raise_Exception
                              (  End_Error'Identity,
                                 Non_Terminated
                              );
                           end if;
                           Status :=
                              (  Mode   => Feed_Exponent_First,
                                 Sign   => 0 /= (Head and 2#0100_0000#),
                                 Radix  => Radix,
                                 Factor => (Head and 2#0000_1100#) / 4,
                                 Exponent => 0,
                                 Exponent_Length => 2
                              );
                        when 2 =>
                           if Length < 4 then
                              Raise_Exception
                              (  End_Error'Identity,
                                 Non_Terminated
                              );
                           end if;
                           Status :=
                              (  Mode   => Feed_Exponent_First,
                                 Sign   => 0 /= (Head and 2#0100_0000#),
                                 Radix  => Radix,
                                 Factor => (Head and 2#0000_1100#) / 4,
                                 Exponent => 0,
                                 Exponent_Length => 3
                              );
                        when others =>
                           if Length < 1 then
                              Raise_Exception
                              (  End_Error'Identity,
                                 Non_Terminated
                              );
                           end if;
                           Status :=
                              (  Mode   => Feed_Exponent_Length,
                                 Sign   => 0 /= (Head and 2#0100_0000#),
                                 Radix  => Radix,
                                 Factor => (Head and 2#0000_1100#) / 4
                              );
                     end case;
                     Item.Value := Number (Length);
                  else
                     if 0 /= (Head and 2#0100_0000#) then
                        Raise_Exception
                        (  Data_Error'Identity,
                           Real_Not_A_Number
                        );
                     end if;
                     Status :=
                        (  Mode          => Feed_Decimal,
                           State         => Mantissa_Start,
                           Mantissa_Sign => False,
                           Exponent_Sign => False,
                           Total_Length  => Length,
                           Decimal_Point => 0
                        );
                      Item.Value := 0.0;
                  end if;
               end;
            when Feed_Exponent_Length =>
               declare
                  Total  : Stream_Element :=
                           Stream_Element (Item.Value);
                  Length : constant Stream_Element := Data (Pointer);
               begin
                  Pointer := Pointer + 1;
                  if Length > Total - 2 then
                     Raise_Exception
                     (  End_Error'Identity,
                        Non_Terminated
                     );
                  end if;
                  Total := Total - Length - 1;
                  if Length > 0 then
                     Status :=
                        (  Mode     => Feed_Exponent_First,
                           Sign     => Status.Sign,
                           Radix    => Status.Radix,
                           Factor   => Status.Factor,
                           Exponent => 0,
                           Exponent_Length => Length
                        );
                     Item.Value := Number (Total);
                  else
                     Item.Value := 0.0;
                     Status :=
                        (  Mode     => Feed_Mantissa,
                           Sign     => Status.Sign,
                           Radix    => Status.Radix,
                           Factor   => Status.Factor,
                           Exponent => 0,
                           Mantissa_Length => Total
                        );
                  end if;
               end;
            when Feed_Exponent_First =>
               declare
                  Exponent : Integer_16;
               begin
                  if Data (Pointer) > 127 then -- Negative exponent
                     Exponent := -Integer_16 (not Data (Pointer)) - 1;
                  else
                     Exponent := Integer_16 (Data (Pointer));
                  end if;
                  Pointer := Pointer + 1;
                  if Status.Exponent_Length = 1 then
                     Status :=
                        (  Mode     => Feed_Mantissa,
                           Sign     => Status.Sign,
                           Radix    => Status.Radix,
                           Factor   => Status.Factor,
                           Exponent => Exponent,
                           Mantissa_Length =>
                              Stream_Element (Item.Value) - 1
                        );
                     Item.Value := 0.0;
                  else
                     Status :=
                        (  Mode     => Feed_Exponent,
                           Sign     => Status.Sign,
                           Radix    => Status.Radix,
                           Factor   => Status.Factor,
                           Exponent => Exponent,
                           Exponent_Length => Status.Exponent_Length - 1
                        );
                     Item.Value :=
                        Item.Value - Number (Status.Exponent_Length);
                  end if;
               end;
            when Feed_Exponent =>
               declare
                  Exponent : Integer_16 := Status.Exponent;
               begin
                  Exponent :=
                     Exponent * 256 + Integer_16 (Data (Pointer));
                  Pointer := Pointer + 1;
                  if Status.Exponent_Length = 1 then
                     Status :=
                        (  Mode     => Feed_Mantissa,
                           Sign     => Status.Sign,
                           Radix    => Status.Radix,
                           Factor   => Status.Factor,
                           Exponent => Exponent,
                           Mantissa_Length =>
                              Stream_Element (Item.Value)
                        );
                     Item.Value := 0.0;
                  else
                     Status.Exponent := Exponent;
                     Status.Exponent_Length :=
                        Status.Exponent_Length - 1;
                  end if;
               end;
            when Feed_Mantissa =>
               Item.Value :=
                  Item.Value * 256.0 + Number (Data (Pointer));
               Pointer := Pointer + 1;
               if Status.Mantissa_Length = 1 then
                  declare
                     Exponent : Integer := Integer (Status.Exponent);
                  begin
                     case Status.Factor is
                        when 1 => Exponent := Exponent + 1;
                        when 2 => Exponent := Exponent + 2;
                        when 3 => Exponent := Exponent + 3;
                        when others => null;
                     end case;
                     case Status.Radix is
                        when Binary =>
                           Item.Value := Item.Value * 2.0 ** Exponent;
                        when Octal =>
                           Item.Value := Item.Value * 8.0 ** Exponent;
                        when Hexadecimal =>
                           Item.Value := Item.Value * 16.0 ** Exponent;
                     end case;
                     if Status.Sign then
                        Item.Value := -Item.Value;
                     end if;
                     State := 0;
                     return;
                  end;
               else
                  Status.Mantissa_Length := Status.Mantissa_Length - 1;
               end if;
            when Feed_Decimal =>
               Status.Total_Length := Status.Total_Length - 1;
               case Data (Pointer) is
                  when Character'Pos ('0')..Character'Pos ('9') =>
                     case Status.State is
                        when Mantissa_Start =>
                           Status.State := Before_Point;
                           Status.Exponent_Sign := True;
                           Item.Value := Number
                                         (  Data (Pointer)
                                         -  Character'Pos ('0')
                                         );
                        when Before_Point =>
                           Status.Exponent_Sign := True;
                           Item.Value := (  Item.Value * 10.0
                                         +  Number
                                            (  Data (Pointer)
                                            -  Character'Pos ('0')
                                         )  );
                        when After_Point =>
                           Status.Exponent_Sign := True;
                           Status.Decimal_Point :=
                              Status.Decimal_Point + 1;
                           Item.Value :=
                              (  Item.Value
                              +  Number
                                 (  Data (Pointer)
                                 -  Character'Pos ('0')
                                 )
                              /  10.0 ** Natural (Status.Decimal_Point)
                              );
                        when Exponent_Start =>
                           Status.State := Exponent;
                           Status.Decimal_Point :=
                              Unsigned_8
                              (  Data (Pointer)
                              -  Character'Pos ('0')
                              );
                           if Status.Total_Length = 0 then
                              if Status.Exponent_Sign then
                                 Item.Value :=
                                    (  Item.Value
                                    /  10.0
                                    ** Natural (Status.Decimal_Point)
                                    );
                              else
                                 Status.Exponent_Sign := True;
                                 Item.Value :=
                                    (  Item.Value
                                    *  10.0
                                    ** Natural (Status.Decimal_Point)
                                    );
                              end if;
                           end if;
                        when Exponent =>
                           Status.Decimal_Point :=
                              (  Status.Decimal_Point * 10
                              +  Unsigned_8
                                 (  Data (Pointer)
                                 -  Character'Pos ('0')
                              )  );
                           if Status.Total_Length = 0 then
                              if Status.Exponent_Sign then
                                 Item.Value :=
                                    (  Item.Value
                                    /  10.0
                                    ** Natural (Status.Decimal_Point)
                                    );
                              else
                                 Status.Exponent_Sign := True;
                                 Item.Value :=
                                    (  Item.Value
                                    *  10.0
                                    ** Natural (Status.Decimal_Point)
                                    );
                              end if;
                           end if;
                     end case;
                     if Status.Total_Length = 0 then
                        if not Status.Exponent_Sign then
                           Raise_Exception
                           (  Data_Error'Identity,
                              Real_Invalid_Decimal
                           );
                        end if;
                        if Status.Mantissa_Sign then
                           Item.Value := -Item.Value;
                        end if;
                        State   := 0;
                        Pointer := Pointer + 1;
                        return;
                     end if;
                  when Character'Pos ('.') =>
                     case Status.State is
                        when Mantissa_Start | Before_Point =>
                           Status.State := After_Point;
                        when After_Point | Exponent_Start | Exponent =>
                           Raise_Exception
                           (  Data_Error'Identity,
                              Real_Invalid_Decimal
                           );
                     end case;
                  when Character'Pos ('e') | Character'Pos ('E') =>
                     if (  Status.Total_Length = 0
                        or else
                           not Status.Exponent_Sign
                        )  then
                        Raise_Exception
                        (  Data_Error'Identity,
                           Real_Invalid_Decimal
                        );
                     end if;
                     Status.Exponent_Sign := False;
                     case Status.State is
                        when Before_Point | After_Point =>
                           Status.State := Exponent_Start;
                        when Mantissa_Start |
                             Exponent_Start |
                             Exponent       =>
                           Raise_Exception
                           (  Data_Error'Identity,
                              Real_Invalid_Decimal
                           );
                     end case;
                  when Character'Pos ('+') =>
                     if Status.Total_Length = 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           Real_Invalid_Decimal
                        );
                     end if;
                     case Status.State is
                        when Mantissa_Start =>
                           Status.State := Before_Point;
                        when Exponent_Start =>
                           Status.State := Exponent;
                           Status.Decimal_Point := 0;
                        when Before_Point | After_Point | Exponent =>
                           Raise_Exception
                           (  Data_Error'Identity,
                              Real_Invalid_Decimal
                           );
                     end case;
                  when Character'Pos ('-') =>
                     if Status.Total_Length = 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           Real_Invalid_Decimal
                        );
                     end if;
                     case Status.State is
                        when Mantissa_Start =>
                           Status.State := Before_Point;
                           Status.Mantissa_Sign := True;
                        when Exponent_Start =>
                           Status.State := Exponent;
                           Status.Exponent_Sign := True;
                           Status.Decimal_Point := 0;
                        when Before_Point | After_Point | Exponent =>
                           Raise_Exception
                           (  Data_Error'Identity,
                              Real_Invalid_Decimal
                           );
                     end case;
                  when others =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        Real_Invalid_Decimal
                     );
               end case;
               Pointer := Pointer + 1;
         end case;
         if Pointer > Data'Last then
            State := To_State (Status);
            exit;
         end if;
      end loop;
   end Feed;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Number
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )
      then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      if Length = 0 then
         Value := 0.0;
         return;
      end if;
      declare
         Head  : Stream_Element;
         Index : Stream_Element_Offset := Pointer;
         Size  : Stream_Element_Offset := Length;
      begin
         if Data'Last - Index < Size - 1 then
            Raise_Exception (End_Error'Identity, Non_Terminated);
         end if;
         Head  := Data (Index);
         Index := Index + 1;
         Size  := Size  - 1;
         if Head > 127 then -- Binary format
            declare
               Radix           : Number;
               Result          : Number := 0.0;
               Exponent_Length : Stream_Element_Count;
               Exponent        : Integer := 0;
            begin
               case Head and 2#0011_0000# is
                  when 2#0000_0000# => Radix :=  2.0;
                  when 2#0001_0000# => Radix :=  8.0;
                  when 2#0010_0000# => Radix := 16.0;
                  when others =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        Real_Format_Reserved
                     );
               end case;
               case Head and 2#11# is -- Exponent length
                  when 0 =>
                     Exponent_Length := 1;
                  when 1 =>
                     Exponent_Length := 2;
                  when 2 =>
                     Exponent_Length := 3;
                  when others =>
                     if Size < 1 then
                        Raise_Exception
                        (  End_Error'Identity,
                           Non_Terminated
                        );
                     end if;
                     Exponent_Length :=
                        Stream_Element_Count (Data (Index));
                     Index := Index  + 1;
                     Size  := Size - 1;
               end case;
               if Exponent_Length > Size then
                  Raise_Exception (End_Error'Identity, Non_Terminated);
               end if;
               if Exponent_Length > 0 then
                  if Data (Index) > 127 then -- Negative exponent
                     Exponent := - Integer (not Data (Index)) - 1;
                  else
                     Exponent := Integer (Data (Index));
                  end if;
                  Index := Index + 1;
                  for Octet in 2..Exponent_Length loop
                     Exponent :=
                        Exponent * 256 + Integer (Data (Index));
                     Index := Index + 1;
                  end loop;
               end if;
               Size := Size - Exponent_Length;
               while Size > 0 loop
                  Result := Result * 256.0 + Number (Data (Index));
                  Index  := Index  + 1;
                  Size   := Size - 1;
               end loop;
               case (Head and 2#0000_1100#) / 4 is
                  when 1 => Exponent := Exponent + 1;
                  when 2 => Exponent := Exponent + 2;
                  when 3 => Exponent := Exponent + 3;
                  when others => null;
               end case;
               Result := Result * Radix ** Exponent;
               if 0 /= (Head and 2#0100_0000#) then
                  Result := -Result;
               end if;
               Value := Result;
            end;
         else
            if 0 /= (Head and 2#0100_0000#) then -- Special case
               Raise_Exception
               (  Constraint_Error'Identity,
                  Real_Not_A_Number
               );
            end if;
            declare
               subtype Slice is String (1..Integer (Size));
               Text : Slice;
               pragma Import (Ada, Text);
               for Text'Address use Data (Index)'Address;
            begin
               Value := Edit.Value (Text);
            end;
         end if;
         Pointer := Index + Size;
      end;
   end Get;

   function Get_ASN1_Type
            (  Item : Implicit_Real_Data_Item
            )  return ASN1_Type is
   begin
       return Real_Tag;
   end Get_ASN1_Type;

   function Get_Value
            (  Item : Implicit_Real_Data_Item
            )  return Number is
   begin
      return Item.Value;
   end Get_Value;

   function Is_Implicit (Item : Implicit_Real_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Real_Data_Item) return Boolean is
   begin
      return False;
   end Is_Implicit;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Number
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      if Value = 0.0 then
         return;
      end if;
      declare
         Fraction_Buffer : Stream_Element_Array (1..256);
         Exponent_Buffer : Stream_Element_Array (1..16);
         First_Octet     : Stream_Element;
         Fraction        : Number;
         Exponent        : Integer;
         Total_Length    : Stream_Element_Offset;
         Exponent_Length : Stream_Element_Offset := 1;
         Fraction_First  : Stream_Element_Offset :=
                              Fraction_Buffer'Last + 1;
      begin
         Fraction :=
            Number'Scaling
            (  Number'Fraction (abs Value),
               Number'Machine_Mantissa
            );
         Exponent :=
            Number'Exponent (Value) - Number'Machine_Mantissa;
         loop
            declare
               Head : constant Number :=
                               Number'Floor (Fraction / 256.0);
            begin
               Fraction_First := Fraction_First - 1;
               Fraction_Buffer (Fraction_First) :=
                  Stream_Element (Fraction - Head * 256.0);
               Fraction := Head;
            end;
            exit when Fraction = 0.0;
         end loop;
         Total_Length := Fraction_Buffer'Last + 2 - Fraction_First;
         Put (Exponent_Buffer, Exponent_Length, Exponent);
         Exponent_Length := Exponent_Length - 1;
         Total_Length := Total_Length + Exponent_Length;
         case Number'Machine_Radix is
            when 2 =>      First_Octet := 2#1000_0000#;
            when 8 =>      First_Octet := 2#1001_0000#;
            when others => First_Octet := 2#1010_0000#;
         end case;
         if Value < 0.0 then
            First_Octet := First_Octet or 2#0100_0000#;
         end if;
         if Exponent_Length > 3 then
            Total_Length := Total_Length + 1;
            First_Octet  := First_Octet or 2#0000_0011#;
         else
            First_Octet := First_Octet
                        or Stream_Element (Exponent_Length - 1);
         end if;
         if Data'Last - Pointer < Total_Length - 1 then
            Raise_Exception (End_Error'Identity, No_Room);
         end if;
         Data (Pointer) := First_Octet;
         Pointer := Pointer + 1;
         if Exponent_Length > 3 then
            Data (Pointer) := Stream_Element (Exponent_Length);
            Pointer := Pointer + 1;
         end if;
         Data (Pointer..Pointer + Exponent_Length - 1) :=
            Exponent_Buffer (1..Exponent_Length);
         Pointer := Pointer + Exponent_Length;
         Data
         (  Pointer
         .. Pointer + Fraction_Buffer'Last - Fraction_First
         )  := Fraction_Buffer
               (  Fraction_First
               .. Fraction_Buffer'Last
               );
         Pointer := Pointer + Fraction_Buffer'Last - Fraction_First + 1;
      end;
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             )  is
      N2 : Boolean := False;
      N3 : Boolean := False;
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      elsif Data'Last - Pointer < Value'Length then
         Raise_Exception (End_Error'Identity, No_Room);
      elsif Value'Length = 0 then
         Raise_Exception
         (  Data_Error'Identity,
            Real_Empty_Decimal
         );
      end if;
      for Index in Value'Range loop
         case Value (Index) is
            when '.' =>
               N2 := True;
            when 'e' | 'E' =>
               N3 := True;
            when '0'..'9' | '+' | '-' =>
               null;
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  Real_Invalid_Decimal
               );
         end case;
      end loop;
      if N3 then
         Data (Pointer) := 2#0000_0011#;
      elsif N2 then
         Data (Pointer) := 2#0000_0010#;
      else
         Data (Pointer) := 2#0000_0001#;
      end if;
      Pointer := Pointer + 1;
      for Index in Value'Range loop
         Data (Pointer) := Character'Pos (Value (Index));
         Pointer := Pointer + 1;
      end loop;
   end;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Real_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length < 1 then
         Set_Implicit_Tag (ASN1_Data_Item (Item), Tag, Length);
      else
         Item.Value := Number (Length); -- Zero length = 0.0 value
      end if;
   end Set_Implicit_Tag;

   procedure Set_Value
             (  Item  : in out Implicit_Real_Data_Item;
                Value : Number
             )  is
   begin
      Item.Value := Value;
   end Set_Value;

end GNAT.Sockets.Connection_State_Machine.ASN1.Generic_Real;
