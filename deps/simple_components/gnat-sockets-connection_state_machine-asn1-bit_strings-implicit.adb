--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Bit_Strings.Implicit                   Spring, 2019       --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Tags;           use Ada.Tags;

with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.
             Implicit is

   type Embedded_Length is mod 2**24;
   type Unused_Bits     is mod 8;
   type Embedding_Mode is
        (  Bit_String_Body,
           Unused_Bits_Count,
           Bit_String_Type,
           Length_Lookahead,
           Null_Contents,
           End_Of_Content
        );
   type Status_Type is record
      Length : Embedded_Length;
      Unused : Unused_Bits;
      Mode   : Embedding_Mode;
   end record;
   for Status_Type use record
      Length at 0 range  0..23;
      Unused at 0 range 24..26;
      Mode   at 0 range 27..29;
   end record;
   for Status_Type'Size use Unsigned_32'Size;

   procedure Check_Length
             (  Item   : Implicit_Bit_String_Data_Item;
                Length : Natural
             )  is
   begin
      if Item.Size < Length then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
   end Check_Length;

   procedure Check_Length
             (  Item   : Implicit_External_Bit_String_Data_Item;
                Length : Natural
             )  is
   begin
      if (  Item.Buffer = null
         or else
            Item.Buffer.Size - Item.Buffer.Length < (Length + 7) / 8
         )  then
         Raise_Exception (Storage_Error'Identity, Too_Large);
      end if;
   end Check_Length;

   procedure Store
             (  Item : in out Implicit_Bit_String_Data_Item;
                Bit  : Boolean
             )  is
   begin
      Item.Length := Item.Length + 1;
      Item.Value (Item.Length) := Bit;
   end Store;

   procedure Store
             (  Item : in out Implicit_Bit_String_Data_Item;
                Byte : Stream_Element
             )  is
   begin
      Item.Value (Item.Length + 1..Item.Length + 8) :=
         (  0 /= (Byte and 2#1000_0000#),
            0 /= (Byte and 2#0100_0000#),
            0 /= (Byte and 2#0010_0000#),
            0 /= (Byte and 2#0001_0000#),
            0 /= (Byte and 2#0000_1000#),
            0 /= (Byte and 2#0000_0100#),
            0 /= (Byte and 2#0000_0010#),
            0 /= (Byte and 2#0000_0001#)
         );
      Item.Length := Item.Length + 8;
   end Store;

   procedure Store
             (  Item : in out Implicit_External_Bit_String_Data_Item;
                Bit  : Boolean
             )  is
   begin
      if Item.Length mod 8 = 0 then
         Item.Buffer.Length := Item.Buffer.Length + 1;
         Item.Buffer.Buffer (Item.Buffer.Length) := Character'Val (0);
      end if;
      if Bit then
         declare
            Octet : Character renames
                    Item.Buffer.Buffer (Item.Buffer.Length);
         begin
            Octet :=
               Character'Val
               (  Character'Pos (Octet) + 2**(7 - Item.Length mod 8)
               );
         end;
      end if;
      Item.Length := Item.Length + 1;
   end Store;

   procedure Store
             (  Item : in out Implicit_External_Bit_String_Data_Item;
                Byte : Stream_Element
             )  is
   begin
      Item.Buffer.Length := Item.Buffer.Length + 1;
      Item.Buffer.Buffer (Item.Buffer.Length) := Character'Val (Byte);
      Item.Length := Item.Length + 8;
   end Store;

   function From_State
            (  State : Stream_Element_Offset
            )  return Status_Type is
      function Convert is
         new Ada.Unchecked_Conversion (Unsigned_32, Status_Type);
   begin
      return Convert (Unsigned_32 (State));
   end From_State;

   function To_State
            (  Status : Status_Type
            )  return Stream_Element_Offset is
      function Convert is
         new Ada.Unchecked_Conversion (Status_Type, Unsigned_32);
   begin
      return Stream_Element_Offset (Convert (Status));
   end To_State;

   procedure Uninitialized (Item : ASN1_Data_Item'Class) is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         (  "ASN.1 bit string "
         &  Expanded_Name (Item'Tag)
         &  " was not properly initialized"
      )  );
   end Uninitialized;

   procedure Explicit_Put is new Generic_Put (Boolean_Array);

   procedure Encode
             (  Item    : Implicit_Bit_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put (Data, Pointer, Item.Value (1..Item.Length));
   end Encode;

   procedure Encode
             (  Item    : Implicit_External_Bit_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put (Data, Pointer, Get_Value (Item));
   end Encode;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_External_Bit_String_Data_Item
             )  is
      procedure Find
                (  Object : in out
                      Implicit_External_Bit_String_Data_Item'Class;
                   Shared : Shared_Data_Item_Ptr
                )  is
         This : Shared_Data_Item_Ptr := Shared;
      begin
         loop
            if This = null then
               Raise_Exception
               (  Use_Error'Identity,
                  (  "Missing buffer for externally buffered "
                  &  "ASN.1 bit string"
               )  );
            end if;
            exit when This.all in External_String_Buffer'Class;
            This := This.Previous;
         end loop;
         Object.Buffer :=
            External_String_Buffer'Class (This.all)'Unchecked_Access;
      end Find;
   begin
      Check_Initialization_Stream (Item, Stream.all);
      declare
         This : Initialization_Stream'Class renames
                Initialization_Stream'Class (Stream.all);
      begin
         if Item.Buffer = null then
            if This.Parent = null then
               Find (Self (Item).all, This.Shared);
            else
               Find (Self (Item).all, This.Parent.all);
            end if;
         end if;
         Enumerate (Stream, ASN1_Data_Item (Item));
      end;
   end Enumerate;

   procedure Generic_Feed
             (  Parent  : in out Data_Item;
                Item    : in out Bit_String_Data;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      Status : Status_Type;
   begin
      if State = 0 then
         if Item.Definite then
            declare
               Length : Stream_Element_Offset := Item.Last;
            begin
               if Item.Primitive then -- Primitive bit string
                  if Length = 0 then
                     Raise_Exception
                     (  Data_Error'Identity,
                        Invalid_Length
                     );
                  end if;
                  Length := Length - 1;
                  if Length = 0 then
                     Status.Mode   := End_Of_Content;
                  else
                     Status.Mode   := Unused_Bits_Count;
                     Status.Unused := 0;
                     Status.Length := Embedded_Length (Length);
                  end if;
               else -- The total length is here
                  Item.Last   := Pointer + Length - 1;
                  Status.Mode := Bit_String_Type;
               end if;
            end;
         else
            if Item.Primitive then
               Raise_Exception (Data_Error'Identity, Wrong_Length);
            end if;
            Item.Nested   := True;
            Status.Length := 0;
            Status.Unused := 0;
            Status.Mode   := Bit_String_Type;
         end if;
         Item.Nested := True;
         State       := To_State (Status);
      end if;
      while Pointer <= Data'Last loop
         if State < 0 then
            while not Is_Length_Ready (State) loop
               if Pointer > Data'Last then
                  return;
               end if;
               Embedded_Feed (Data, Pointer, State);
            end loop;
            if Is_Indefinite (State) then
               if Item.Primitive then
                  Raise_Exception (Data_Error'Identity, Wrong_Length);
               elsif Item.Nested then
                  Raise_Exception (Data_Error'Identity, Nested);
               end if;
               Item.Definite := False;
               Item.Nested   := True;
               Status.Length := 0;
               Status.Unused := 0;
               Status.Mode   := Bit_String_Type;
            else
               declare
                  Length : Stream_Element_Offset := Get_Length (State);
               begin
                  if Item.Primitive then -- Primitive bit string
                     if Length = 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           Invalid_Length
                        );
                     end if;
                     Length := Length - 1;
                     if Length = 0 then
                        Status.Mode   := End_Of_Content;
                     else
                        Status.Mode   := Unused_Bits_Count;
                        Status.Unused := 0;
                        Status.Length := Embedded_Length (Length);
                     end if;
                  elsif Item.Nested then -- Constructor bit string
                     if Item.Definite and then Pointer > Item.Last then
                        Raise_Exception
                        (  Data_Error'Identity,
                           "ASN.1 bit string data overrun"
                        );
                     elsif Length = 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           Invalid_Length
                        );
                     end if;
                     Length := Length - 1;
                     if Length = 0 then
                        Status.Mode := Null_Contents;
                     else
                        Status.Mode   := Unused_Bits_Count;
                        Status.Unused := 0;
                        Status.Length := Embedded_Length (Length);
                     end if;
                  else -- Not nested, the total length is here
                     Item.Last     := Pointer + Length - 1;
                     Item.Definite := True;
                     Item.Nested   := True;
                     Status.Mode   := Bit_String_Type;
                  end if;
               end;
            end if;
            State := To_State (Status);
         else
            Status := From_State (State);
            declare
               Mask  : Stream_Element;
               Octet : Stream_Element;
            begin
               loop
                  case Status.Mode is
                     when Unused_Bits_Count =>
                        if Data (Pointer) > 7 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              Wrong_Unused_Count
                           );
                        end if;
                        Status.Mode   := Bit_String_Body;
                        Status.Unused := Unused_Bits (Data (Pointer));
                        Check_Length
                        (  Parent,
                           (  Integer (Status.Length) * 8
                           -  Integer (Data (Pointer))
                        )  );
                        Pointer := Pointer + 1;
                     when Bit_String_Body =>
                        loop
                           Mask    := 2**7;
                           Octet   := Data (Pointer);
                           Pointer := Pointer + 1;
                           if Status.Length = 1 then
                              for Bit in 1
                                      .. 8 - Integer (Status.Unused)
                              loop
                                 Store (Parent, 0 /= (Octet and Mask));
                                 Mask := Mask / 2;
                              end loop;
                              if (  Item.Primitive
                                 or else
                                    (  Item.Definite
                                    and then
                                       Pointer > Item.Last
                                 )  )  then
                                 State := 0;
                                 return;
                              end if;
                              Status.Mode := Bit_String_Type;
                              exit;
                           end if;
                           Store (Parent, Octet);
                           Status.Length := Status.Length - 1;
                           if Pointer > Data'Last then
                              State := To_State (Status);
                              return;
                           end if;
                        end loop;
                     when Bit_String_Type =>
                        if (  Item.Definite
                           and then
                              Pointer > Item.Last
                           )  then
                           State := 0;
                           return;
                        end if;
                        case Data (Pointer) is
                           when ASN1_Type'Pos (Bit_String_Tag) =>
                              Status.Mode := Length_Lookahead;
                           when 0 =>
                              Status.Mode := End_Of_Content;
                           when others =>
                              Raise_Exception
                              (  Data_Error'Identity,
                                 Bit_String_Invalid_EOC
                              );
                        end case;
                        Pointer := Pointer + 1;
                     when Length_Lookahead => -- Checking zero string
                        case Data (Pointer) is
                           when 0 =>      -- Empty bit string
                              Status.Mode := Null_Contents;
                              Pointer     := Pointer + 1;
                           when 1..127 => -- Non-empty bit string
                              State := Start_Length;
                              exit;
                           when 128..255 =>
                              Raise_Exception
                              (  Data_Error'Identity,
                                 Nested
                              );
                        end case;
                     when Null_Contents => -- Checking zero string
                        if Data (Pointer) /= 0 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              Bit_String_Invalid_EOC
                           );
                        end if;
                        Status.Mode := Bit_String_Type;
                        Pointer     := Pointer + 1;
                        if (  Item.Definite
                           and then
                              Pointer > Item.Last
                           )  then
                           State := 0;
                           return;
                        end if;
                     when End_Of_Content =>
                        if Data (Pointer) /= 0 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              Bit_String_Invalid_EOC
                           );
                        end if;
                        Pointer := Pointer + 1;
                        State   := 0;
                        return;
                  end case;
                  if Pointer > Data'Last then
                     State := To_State (Status);
                     return;
                  end if;
               end loop;
            end;
         end if;
      end loop;
   end Generic_Feed;

   procedure Internal_Feed is
      new Generic_Feed (Implicit_Bit_String_Data_Item);

   procedure External_Feed is
      new Generic_Feed (Implicit_External_Bit_String_Data_Item);

   procedure Feed
             (  Item    : in out Implicit_Bit_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Item.Length := 0;
      end if;
      Internal_Feed (Item, Item.Data, Data, Pointer, Client, State);
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_External_Bit_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         if Item.Buffer = null then
            Uninitialized (Item);
         end if;
         Item.Length := 0;
         Item.Start  := Item.Buffer.Length + 1;
      end if;
      External_Feed (Item, Item.Data, Data, Pointer, Client, State);
   end Feed;

   function Get_ASN1_Type
            (  Item : Implicit_External_Bit_String_Data_Item
            )  return ASN1_Type is
   begin
      return Bit_String_Tag;
   end Get_ASN1_Type;

   function Get_Buffer
            (  Item : Implicit_External_Bit_String_Data_Item
            )  return External_String_Buffer_Ptr is
   begin
      return Item.Buffer;
   end Get_Buffer;

   function Get_Length
            (  Item : Implicit_External_Bit_String_Data_Item
            )  return Natural is
   begin
      return Item.Length;
   end Get_Length;

   function Get_Value
            (  Item : Implicit_External_Bit_String_Data_Item
            )  return Boolean_Array is
      Result  : Boolean_Array (1..Item.Length);
      Current : Integer := Item.Start;
      Octet   : Stream_Element;
      Mask    : Stream_Element := 0;
   begin
      if Item.Buffer = null then
         return (1..0 => False);
      end if;
      for Index in Result'Range loop
         if Mask = 0 then
            Mask    := 2**7;
            Octet   := Character'Pos (Item.Buffer.Buffer (Current));
            Current := Current + 1;
         end if;
         Result (Index) := 0 /= (Octet and Mask);
         Mask := Mask / 2;
      end loop;
      return Result;
   end Get_Value;

   function Is_Implicit (Item : Implicit_Bit_String_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Implicit_External_Bit_String_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   procedure Reset_Feed
             (  Item    : in out Implicit_Bit_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Item.Data.Primitive := 0 = (Data (Pointer) and 16#20#);
      Item.Data.Nested    := False;
   end Reset_Feed;

   procedure Reset_Feed
             (  Item    : in out Implicit_External_Bit_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      if Item.Buffer = null then
         Uninitialized (Item);
      end if;
      Item.Data.Primitive := 0 = (Data (Pointer) and 16#20#);
      Item.Data.Nested    := False;
      Item.Length         := 0;
      Item.Start          := Item.Buffer.Length + 1;
   end Reset_Feed;

   function Self
            (  Item : Implicit_External_Bit_String_Data_Item'Class
            )  return Implicit_External_Bit_String_Data_Item_Ptr is
      package From_Address is
         new System.Address_To_Access_Conversions
             (  Implicit_External_Bit_String_Data_Item'Class
             );
      use From_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Bit_String_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length > 0 then
         Item.Data.Definite := True;
         Item.Data.Last     := Length;
      elsif Length = 0 then
         Raise_Exception (Data_Error'Identity, Invalid_Length);
      else
         Item.Data.Definite := False;
      end if;
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_External_Bit_String_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length > 0 then
         Item.Data.Definite := True;
         Item.Data.Last     := Length;
      elsif Length = 0 then
         Raise_Exception (Data_Error'Identity, Invalid_Length);
      else
         Item.Data.Definite := False;
      end if;
   end Set_Implicit_Tag;

   procedure Set_Value
             (  Item  : in out Implicit_External_Bit_String_Data_Item;
                Value : Boolean_Array
             )  is
      Size : constant Natural := (Value'Length + 7) / 8;
      Mask : Stream_Element := 0;
   begin
      if Item.Buffer = null then
         Uninitialized (Item);
      elsif Item.Buffer.Size - Item.Buffer.Length < Size then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      declare
         Buffer : String  renames Item.Buffer.Buffer;
         Length : Integer renames Item.Buffer.Length;
      begin
         Item.Start := Length + 1;
         for Index in Value'Range loop
            if Mask = 0 then
               Mask   := 2**7;
               Length := Length + 1;
               Buffer (Length) := Character'Val (0);
            end if;
            if Value (Index) then
               Buffer (Length) :=
                  Character'Val
                  (  Character'Pos (Buffer (Length)) or Mask
                  );
            end if;
            Mask := Mask / 2;
         end loop;
      end;
   end Set_Value;

end GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.Implicit;
