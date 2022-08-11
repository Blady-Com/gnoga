--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Indefinite_Unsigned                    Spring, 2019       --
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
with Ada.Tags;           use Ada.Tags;

with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine.
             ASN1.Indefinite_Unsigneds is
   pragma Assert (Character'Size = Stream_Element'Size);

   procedure Uninitialized (Item : ASN1_Data_Item'Class) is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         (  "ASN.1 indefinite unsigned "
         &  Expanded_Name (Item'Tag)
         &  " was not properly initialized"
      )  );
   end Uninitialized;

   procedure Encode
             (  Item    : Implicit_Indefinite_Unsigned_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
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
         Content : Stream_Element_Array
                   (  1
                   .. Stream_Element_Count (Item.Length)
                   );
         pragma Import (Ada, Content);
         for Content'Address use
             Item.Buffer.Buffer (Item.Start)'Address;
      begin
         Put (Data, Pointer, Content);
      end;
   end Encode;

   procedure Explicit_Encode is
      new Generic_Explicit_Encode
          (  Implicit_Indefinite_Unsigned_Data_Item
          );

   procedure Encode
             (  Item    : Indefinite_Unsigned_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Encode
      (  Implicit_Indefinite_Unsigned_Data_Item (Item),
         Data,
         Pointer
      );
   end Encode;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Indefinite_Unsigned_Data_Item
             )  is
      procedure Find
                (  Object : in out
                      Implicit_Indefinite_Unsigned_Data_Item'Class;
                   Shared : Shared_Data_Item_Ptr
                )  is
         This : Shared_Data_Item_Ptr := Shared;
      begin
         loop
            if This = null then
               Raise_Exception
               (  Use_Error'Identity,
                  (  "Missing buffer for externally buffered "
                  &  "ASN.1 indefinite unsigned"
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

   procedure Feed
             (  Item    : in out Indefinite_Unsigned_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check (Data (Pointer), (1 => Integer_Tag), True);
         Pointer     := Pointer + 1;
         Item.Start  := Item.Buffer.Length + 1;
         Item.Length := 0;
         State       := Start_Length;
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
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         State := Get_Length (State);
         if State = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         Item.Length := 0;
         Item.Start  := Item.Buffer.Length + 1;
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      Feed
      (  Item    => Implicit_Indefinite_Unsigned_Data_Item (Item),
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Indefinite_Unsigned_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         if Item.Length = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         State       := Stream_Element_Offset (Item.Length);
         Item.Start  := Item.Buffer.Length + 1;
         Item.Length := 0;
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      while Pointer <= Data'Last loop
         if Item.Buffer.Length >= Item.Buffer.Size then
            Raise_Exception (Storage_Error'Identity, Too_Large);
         end if;
         Item.Buffer.Length := Item.Buffer.Length + 1;
         Item.Buffer.Buffer (Item.Buffer.Length) :=
            Character'Val (Data (Pointer));
         State       := State   - 1;
         Pointer     := Pointer + 1;
         Item.Length := Item.Length + 1;
         exit when State = 0;
      end loop;
   end Feed;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Stream_Element_Array
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      elsif Data'Last - Pointer < Value'Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      else
         Value   := Data (Pointer..Pointer + Value'Length - 1);
         Pointer := Pointer + Value'Length;
      end if;
   end Get;

   function Get_ASN1_Type
            (  Item : Implicit_Indefinite_Unsigned_Data_Item
            )  return ASN1_Type is
   begin
       return Integer_Tag;
   end Get_ASN1_Type;

   function Get_Buffer
            (  Item : Implicit_Indefinite_Unsigned_Data_Item
            )  return External_String_Buffer_Ptr is
   begin
      return Item.Buffer;
   end Get_Buffer;

   function Get_Length
            (  Item : Implicit_Indefinite_Unsigned_Data_Item
            )  return Natural is
   begin
      return Item.Length;
   end Get_Length;

   function Get_Value
            (  Item : Implicit_Indefinite_Unsigned_Data_Item
            )  return Stream_Element_Array is
   begin
      if Item.Buffer = null then
         return (1..0 => 0);
      else
         declare
            Content : Stream_Element_Array
                      (  1
                      .. Stream_Element_Count (Item.Length)
                      );
            pragma Import (Ada, Content);
            for Content'Address use
                Item.Buffer.Buffer (Item.Start)'Address;
         begin
            return Content;
         end;
      end if;
   end Get_Value;

   function Is_Implicit (Item : Indefinite_Unsigned_Data_Item)
     return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Is_Implicit (Item : Implicit_Indefinite_Unsigned_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   procedure Put_Unchecked
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Stream_Element_Array
             )  is
      Length : Stream_Element_Offset := Value'Length;
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      if Value'Length = 0 or else Value (Value'First) > 127 then
         Length := Length + 1;
      end if;
      if Data'Last - Pointer < Length - 1 then
         Raise_Exception (End_Error'Identity, No_Room);
      end if;
      if Value'Length = 0 or else Value (Value'First) > 127 then
         Data (Pointer) := 0; -- The higher-order bit is not 0
         Pointer := Pointer + 1;
      end if;
      Data (Pointer..Pointer + Value'Length - 1) := Value;
      Pointer := Pointer + Value'Length;
   end Put_Unchecked;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Stream_Element_Array
             )  is
   begin
      if Value'Length > 0 then
         for Index in Value'Range loop
            if Value (Index) /= 0 then
               Put_Unchecked (Data, Pointer, Value (Index..Value'Last));
               return;
            end if;
         end loop;
      end if;
      Put_Unchecked (Data, Pointer, (1..0 => 0));
   end Put;

   function Self
            (  Item : Implicit_Indefinite_Unsigned_Data_Item'Class
            )  return Implicit_Indefinite_Unsigned_Data_Item_Ptr is
      package From_Address is
         new System.Address_To_Access_Conversions
             (  Implicit_Indefinite_Unsigned_Data_Item'Class
             );
      use From_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Indefinite_Unsigned_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length < 1 then
         if Length < 0 then
            Set_Implicit_Tag (ASN1_Data_Item (Item), Tag, Length);
         else
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
      else
         Item.Length := Natural (Length);
      end if;
   end Set_Implicit_Tag;

   procedure Set_Value
             (  Item  : in out Implicit_Indefinite_Unsigned_Data_Item;
                Value : Stream_Element_Array
             )  is
   begin
      if Item.Buffer = null then
         Uninitialized (Item);
      end if;
      declare
         Buffer : String  renames Item.Buffer.Buffer;
         Length : Integer renames Item.Buffer.Length;
         First  : Stream_Element_Offset;
      begin
         Item.Start := Length + 1;
         if Value'Length = 0 then
            Item.Length := 0; -- Zero value
            return;
         end if;
         First := Value'First;
         while Value (First) = 0 loop
            if First = Value'Last then
               Item.Length := 0; -- Zero value
               return;
            end if;
            First := First + 1;
         end loop;
         if (  Stream_Element_Offset
               (  Item.Buffer.Size
               -  Item.Buffer.Length
               )
            <  Value'Last - First + 1
            )  then
            Raise_Exception (Constraint_Error'Identity, Too_Large);
         end if;
         Item.Length := Integer (Value'Last - First + 1);
         for Index in First..Value'Last loop
            Buffer (Length) := Character'Val (Value (Index));
            Length := Length + 1;
         end loop;
      end;
   end Set_Value;

end GNAT.Sockets.Connection_State_Machine.ASN1.Indefinite_Unsigneds;
