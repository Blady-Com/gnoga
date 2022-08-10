--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Generic_Unsigned                       Spring, 2019       --
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

package body GNAT.Sockets.Connection_State_Machine.
             ASN1.Generic_Unsigned is

   procedure Explicit_Put is new Generic_Put (Number);

   procedure Encode
             (  Item    : Implicit_Unsigned_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put (Data, Pointer, Item.Value);
   end Encode;

   procedure Encode
             (  Item    : Unsigned_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Put (Data, Pointer, Integer_Tag, Item.Value);
   end Encode;

   procedure Feed
             (  Item    : in out Unsigned_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check (Data (Pointer), (1 => Integer_Tag), True);
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
         if Item.Value = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         State := 0;
      end if;
      Feed
      (  Item    => Implicit_Unsigned_Data_Item (Item),
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Unsigned_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         State := Stream_Element_Offset (Item.Value);
         Item.Value := 0;
      end if;
      while Pointer <= Data'Last loop
         begin
            Item.Value := Item.Value * 256 + Number (Data (Pointer));
         exception
            when Constraint_Error =>
               Raise_Exception (Data_Error'Identity, Too_Large);
         end;
         State   := State - 1;
         Pointer := Pointer + 1;
         exit when State = 0;
      end loop;
   end Feed;

   function Get_ASN1_Type
            (  Item : Implicit_Unsigned_Data_Item
            )  return ASN1_Type is
   begin
       return Integer_Tag;
   end Get_ASN1_Type;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Count;
                Value   : out Number
             )  is
      Result : Number := 0;
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
      declare
         Index  : Stream_Element_Offset := Pointer;
      begin
         if Data'Last - Index < Length - 1 then
            Raise_Exception (End_Error'Identity, Non_Terminated);
         end if;
         for Octet in 1..Length loop
            begin
               Result := Result * 256 + Number (Data (Index));
            exception
               when Constraint_Error =>
                 Raise_Exception (Data_Error'Identity, Too_Large);
            end;
            Index  := Index + 1;
         end loop;
         Value   := Result;
         Pointer := Index;
      end;
   end Get;

   function Get_Value
            (  Item : Implicit_Unsigned_Data_Item
            )  return Number is
   begin
      return Item.Value;
   end Get_Value;

   function Is_Implicit (Item : Implicit_Unsigned_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Unsigned_Data_Item) return Boolean is
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
      declare
         Result : Stream_Element_Array (1..(Number'Size + 7) / 8);
         First  : Stream_Element_Offset := Result'Last + 1;
         Tail   : Number := Value;
      begin
         loop
            First := First - 1;
            Result (First) := Stream_Element (Tail mod 256);
            Tail  := Tail / 256;
            exit when Tail = 0;
         end loop;
         declare
            Length : constant Stream_Element_Offset :=
                              Result'Last - First + 1;
         begin
            if Data'Last - Pointer < Length - 1 then
               Raise_Exception (End_Error'Identity, No_Room);
            end if;
            Data (Pointer..Pointer + Length - 1) :=
               Result (First..Result'Last);
            Pointer := Pointer + Length;
         end;
      end;
   end Put;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Unsigned_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length < 1 then
         if Length = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         else
            Set_Implicit_Tag (ASN1_Data_Item (Item), Tag, Length);
         end if;
      else
         Item.Value := Number (Length);
      end if;
   end Set_Implicit_Tag;

   procedure Set_Value
             (  Item  : in out Implicit_Unsigned_Data_Item;
                Value : Number
             )  is
   begin
      Item.Value := Value;
   end Set_Value;

end GNAT.Sockets.Connection_State_Machine.ASN1.Generic_Unsigned;
