--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Object_Identifiers                             Spring, 2019       --
--  Implementation                                                         --
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

with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine.ASN1.
             Object_Identifiers is

   procedure Uninitialized (Item : ASN1_Data_Item'Class) is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         (  "ASN.1 OID "
         &  Expanded_Name (Item'Tag)
         &  " was not properly initialized"
      )  );
   end Uninitialized;

   procedure Explicit_Encode is
      new Generic_Explicit_Encode (Implicit_External_OID_Data_Item);

   procedure Encode
             (  Item    : External_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Encode
      (  Implicit_External_OID_Data_Item (Item),
         Data,
         Pointer
      );
   end Encode;

   procedure Explicit_Encode is
      new Generic_Explicit_Encode
          (  Implicit_External_Relative_OID_Data_Item
          );

   procedure Encode
             (  Item    : External_Relative_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Encode
      (  Implicit_External_Relative_OID_Data_Item (Item),
         Data,
         Pointer
      );
   end Encode;

   procedure Encode
             (  Item    : Implicit_External_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put (Data, Pointer, Get_Value (Item));
   end Encode;

   procedure Encode
             (  Item    : Implicit_External_Relative_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put_Relative (Data, Pointer, Get_Value (Item));
   end Encode;

   procedure Encode
             (  Item    : Implicit_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put (Data, Pointer, Item.Value (1..Item.Length));
   end Encode;

   procedure Encode
             (  Item    : Implicit_Relative_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Put_Relative (Data, Pointer, Item.Value (1..Item.Length));
   end Encode;

   procedure Explicit_Encode is
      new Generic_Explicit_Encode (Implicit_OID_Data_Item);

   procedure Encode
             (  Item    : OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Encode (Implicit_OID_Data_Item (Item), Data, Pointer);
   end Encode;

   procedure Explicit_Encode is
      new Generic_Explicit_Encode (Implicit_Relative_OID_Data_Item);

   procedure Encode
             (  Item    : Relative_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Explicit_Encode
      (  Implicit_Relative_OID_Data_Item (Item),
         Data,
         Pointer
      );
   end Encode;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_External_OID_Data_Item
             )  is
      procedure Find
                (  Object : in out
                   Implicit_External_OID_Data_Item'Class;
                   Shared : Shared_Data_Item_Ptr
                )  is
         This : Shared_Data_Item_Ptr := Shared;
      begin
         loop
            if This = null then
               Raise_Exception
               (  Use_Error'Identity,
                  (  "Missing buffer for externally buffered "
                  &  "ASN.1 object identifier"
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

   procedure Expand (Item : in out Implicit_External_OID_Data_Item) is
      type OID_Ptr is access Subindentifier_Type;
      for OID_Ptr'Storage_Pool use Item.Buffer.Pool;
      Last : OID_Ptr;
   begin
      Last := new Subindentifier_Type;
      if Item.Length = 0 then
         Item.Value := Last.all'Unchecked_Access;
      end if;
      Item.Length := Item.Length + 1;
      declare
         type OID is
            array (1..Item.Length) of aliased Subindentifier_Type;
         Value : OID;
         pragma Import (Ada, Value);
         for Value'Address use Item.Value.all'Address;
      begin
         Value (Value'Last) := 0;
      end;
   end Expand;

   procedure Feed
             (  Item    : in out External_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check (Data (Pointer), (1 => Object_Identifier_Tag), True);
         Pointer := Pointer + 1;
         State   := Start_Length;
         Item.Value := null;
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
         State := Get_Length (State);
         if State = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         Item.Length := 0;
         Item.Value  := null;
         Expand (Item);
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      Feed
      (  Item    => Implicit_External_OID_Data_Item (Item),
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
   end Feed;

   procedure Feed
             (  Item    : in out External_Relative_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check (Data (Pointer), (1 => Relative_OID_Tag), True);
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
         State       := Get_Length (State);
         if State = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         Item.Value  := null;
         Item.Length := 0;
         Expand (Item);
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      Feed
      (  Item    => Implicit_External_Relative_OID_Data_Item (Item),
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
   end Feed;

   procedure Feed
             (  Item    : in out OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check (Data (Pointer), (1 => Object_Identifier_Tag), True);
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
         State := Get_Length (State);
         Item.Length := 1;
         Item.Value (1) := 0;
         if State = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         elsif Pointer > Data'Last then
            return;
         end if;
      end if;
      Feed
      (  Item    => Implicit_OID_Data_Item (Item),
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
   end Feed;

   procedure Feed
             (  Item    : in out Relative_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check (Data (Pointer), (1 => Relative_OID_Tag), True);
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
         State := Get_Length (State);
         Item.Length := 1;
         Item.Value (1) := 0;
         if State = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         elsif Pointer > Data'Last then
            return;
         end if;
      end if;
      Feed
      (  Item    => Implicit_Relative_OID_Data_Item (Item),
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_External_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         State := Stream_Element_Offset (Item.Length);
         if State = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         Item.Length := 0;
         Item.Value  := null;
         Expand (Item);
      end if;
      while Pointer <= Data'Last loop
         declare
            type OID is
               array (1..Item.Length) of aliased Subindentifier_Type;
            Value : OID;
            pragma Import (Ada, Value);
            for Value'Address use Item.Value.all'Address;
            This : Subindentifier_Type renames Value (Value'Last);
         begin
            State := State - 1;
            if Data (Pointer) > 127 then -- Heading octet
               if State = 0 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     Invalid_Length
                  );
               end if;
               This := (  (This - 1) * 128
                       +  Subindentifier_Type (Data (Pointer))
                       );
            else -- Last octet
               This := (  This * 128
                       +  Subindentifier_Type (Data (Pointer))
                       );
               if Item.Length < 2 then
                  declare
                     Pair : constant Subindentifier_Type := This;
                  begin
                     if Item.Length = 1 then
                        Expand (Item);
                     end if;
                     declare
                        type OID is
                           array (1..Item.Length)
                              of aliased Subindentifier_Type;
                        Value : OID;
                        pragma Import (Ada, Value);
                        for Value'Address use Item.Value.all'Address;
                     begin
                        Value (2) := Pair mod 40;
                        Value (1) := Pair / 40;
                     end;
                  end;
               end if;
               if State = 0 then
                  Pointer := Pointer + 1;
                  return;
               end if;
               Expand (Item);
            end if;
            Pointer := Pointer + 1;
         end;
      end loop;
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         State := Stream_Element_Offset (Item.Length);
         if State = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         Item.Length := 1;
         Item.Value (1) := 0;
      end if;
      while Pointer <= Data'Last loop
         declare
            This : Subindentifier_Type renames
                   Item.Value (Item.Length);
         begin
            State := State - 1;
            if Data (Pointer) > 127 then -- Heading octet
               if State = 0 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     Invalid_Length
                  );
               end if;
               This := (  (This - 1) * 128
                       +  Subindentifier_Type (Data (Pointer))
                       );
            else -- Last octet
               This := (  This * 128
                       +  Subindentifier_Type (Data (Pointer))
                       );
               if Item.Length < 2 then
                  Item.Value (2) := This mod 40;
                  Item.Value (1) := This / 40;
                  Item.Length    := 2;
               end if;
               if State = 0 then
                  Pointer := Pointer + 1;
                  return;
               elsif Item.Length >= Item.Value'Length then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     Too_Large
                  );
               elsif Item.Length >= Item.Size then
                  Raise_Exception
                  (  Data_Error'Identity,
                     Too_Large
                  );
               end if;
               Item.Length := Item.Length + 1;
               Item.Value (Item.Length) := 0;
            end if;
            Pointer := Pointer + 1;
         end;
      end loop;
   end Feed;

   procedure Feed
             (  Item    : in out
                          Implicit_External_Relative_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         State := Stream_Element_Offset (Item.Length);
         if State = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         Item.Length := 0;
         Item.Value  := null;
         Expand (Item);
      end if;
      while Pointer <= Data'Last loop
         declare
            type OID is
               array (1..Item.Length) of aliased Subindentifier_Type;
            Value : OID;
            pragma Import (Ada, Value);
            for Value'Address use Item.Value.all'Address;
            This : Subindentifier_Type renames Value (Value'Last);
         begin
            State := State - 1;
            if Data (Pointer) > 127 then -- Heading octet
               if State = 0 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     Invalid_Length
                  );
               end if;
               This := (  (This - 1) * 128
                       +  Subindentifier_Type (Data (Pointer))
                       );
            else -- Last octet
               This := (  This * 128
                       +  Subindentifier_Type (Data (Pointer))
                       );
               if State = 0 then
                  Pointer := Pointer + 1;
                  return;
               end if;
               Expand (Item);
            end if;
         end;
         Pointer := Pointer + 1;
      end loop;
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Relative_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         State := Stream_Element_Offset (Item.Length);
         if State = 0 then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         Item.Length := 1;
         Item.Value (1) := 0;
      end if;
      while Pointer <= Data'Last loop
         declare
            This : Subindentifier_Type renames
                   Item.Value (Item.Length);
         begin
            State := State - 1;
            if Data (Pointer) > 127 then -- Heading octet
               if State = 0 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     Invalid_Length
                  );
               end if;
               This := (  (This - 1) * 128
                       +  Subindentifier_Type (Data (Pointer))
                       );
            else -- Last octet
               This := (  This * 128
                       +  Subindentifier_Type (Data (Pointer))
                       );
               if State = 0 then
                  Pointer := Pointer + 1;
                  return;
               elsif Item.Length >= Item.Value'Length then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     Too_Large
                  );
               elsif Item.Length >= Item.Size then
                  Raise_Exception (Data_Error'Identity, Too_Large);
               end if;
               Item.Length := Item.Length + 1;
               Item.Value (Item.Length) := 0;
            end if;
         end;
         Pointer := Pointer + 1;
      end loop;
   end Feed;

   function Get
            (  Data    : Stream_Element_Array;
               Pointer : access Stream_Element_Offset;
               Length  : Stream_Element_Offset
            )  return Object_Identifier is
      Size : Natural := 256;
   begin
      loop
         declare
            Result : Object_Identifier (1..Size);
            Index  : Stream_Element_Offset := Pointer.all;
            Last   : Integer;
         begin
            Get (Data, Index, Length, Result, Last);
            Pointer.all := Index;
            return Result (1..Last);
         exception
            when Constraint_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Object_Identifier;
                Last    : out Integer
             )  is
      Index : Stream_Element_Offset := Pointer;
      Size  : Integer := 0;
      This  : Integer := 0;
   begin
      if Data'Last - Index < Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      elsif Length = 0 then
         Raise_Exception (Data_Error'Identity, Invalid_Length);
      elsif Value'Length < 2 then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      for Octet in 1..Length loop
         if Data (Index) > 127 then -- Heading octet
            if Octet = Length then
               Raise_Exception (Data_Error'Identity, Invalid_Length);
            end if;
            This := (This - 1) * 128 + Integer (Data (Index));
         else -- Last octet
            This := This * 128 + Integer (Data (Index));
            if Size < 2 then
               Value (Value'First..Value'First + 1) :=
                  (  Subindentifier_Type (This / 40),
                     Subindentifier_Type (This mod 40)
                  );
               Size := 2;
               This := 0;
            else
               if Size >= Value'Length then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     Too_Large
                  );
               end if;
               Value (Value'First + Size) := Subindentifier_Type (This);
               Size := Size + 1;
               This := 0;
            end if;
         end if;
         Index := Index + 1;
      end loop;
      Pointer := Index;
      Last    := Value'First + Size - 1;
   end Get;

   function Get_ASN1_Type
            (  Item : Implicit_External_OID_Data_Item
            )  return ASN1_Type is
   begin
      return Object_Identifier_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_OID_Data_Item
            )  return ASN1_Type is
   begin
      return Object_Identifier_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_Relative_OID_Data_Item
            )  return ASN1_Type is
   begin
      return Relative_OID_Tag;
   end Get_ASN1_Type;

   function Get_Buffer
            (  Item : Implicit_External_OID_Data_Item
            )  return External_String_Buffer_Ptr is
   begin
      return Item.Buffer;
   end Get_Buffer;

   function Get_Length
            (  Item : Implicit_External_OID_Data_Item
            )  return Natural is
   begin
      if Item.Buffer = null or else Item.Value = null then
         return 0;
      else
         return Item.Length;
      end if;
   end Get_Length;

   procedure Get_Relative
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Object_Identifier;
                Last    : out Integer
             )  is
      Index : Stream_Element_Offset := Pointer;
      Size  : Integer := 0;
      This  : Integer := 0;
   begin
      if Data'Last - Index < Length - 1 then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      end if;
      for Octet in 1..Length loop
         if Data (Index) > 127 then -- Heading octet
            if Octet = Length then
               Raise_Exception (Data_Error'Identity, Invalid_Length);
            end if;
            This := (This - 1) * 128 + Integer (Data (Index));
         else -- Last octet
            This := This * 128 + Integer (Data (Index));
            if Size >= Value'Length then
               Raise_Exception (Constraint_Error'Identity, Too_Large);
            end if;
            Value (Value'First + Size) := Subindentifier_Type (This);
            Size := Size + 1;
            This := 0;
            exit when Length = 0;
         end if;
         Index := Index + 1;
      end loop;
      Pointer := Index;
      Last    := Value'First + Size - 1;
   end Get_Relative;

   function Get_Relative
            (  Data    : Stream_Element_Array;
               Pointer : access Stream_Element_Offset;
               Length  : Stream_Element_Offset
            )  return Object_Identifier is
      Size : Natural := 256;
   begin
      loop
         declare
            Result : Object_Identifier (1..Size);
            Index  : Stream_Element_Offset := Pointer.all;
            Last   : Integer;
         begin
            Get_Relative (Data, Index, Length, Result, Last);
            Pointer.all := Index;
            return Result (1..Last);
         exception
            when Constraint_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Get_Relative;

   function Get_Value
            (  Item : Implicit_External_OID_Data_Item
            )  return Object_Identifier is
   begin
      if (  Item.Buffer = null
         or else
            Item.Value = null
         or else
            Item.Length = 0
         )  then
         return (1..0 => 0);
      end if;
      declare
         type OID is
            array (1..Item.Length) of aliased Subindentifier_Type;
         Value : OID;
         pragma Import (Ada, Value);
         for Value'Address use Item.Value.all'Address;
      begin
         return Object_Identifier (Value);
      end;
   end Get_Value;

   function Get_Value
            (  Item : Implicit_OID_Data_Item
            )  return Object_Identifier is
   begin
      return Item.Value (1..Item.Length);
   end Get_Value;

   function Is_Implicit
            (  Item : External_OID_Data_Item
            )  return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Is_Implicit
            (  Item : External_Relative_OID_Data_Item
            )  return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Is_Implicit
            (  Item : Implicit_External_OID_Data_Item
            )  return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit
            (  Item : Implicit_OID_Data_Item
            )  return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit
            (  Item : OID_Data_Item
            )  return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Is_Implicit
            (  Item : Relative_OID_Data_Item
            )  return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Length_Of
            (  Item : Subindentifier_Type
            )  return Stream_Element_Count is
      This   : Subindentifier_Type := Item;
      Length : Stream_Element_Count := 0;
   begin
      loop
         Length := Length + 1;
         This   := This / 128;
         exit when This = 0;
      end loop;
      return Length;
   end Length_Of;

   function Length_Of
            (  Identifier : Object_Identifier
            )  return Stream_Element_Count is
      Length : Stream_Element_Count := 0;
   begin
      for Index in Identifier'Range loop
         Length := Length + Length_Of (Identifier (Index));
      end loop;
      return Length;
   end Length_Of;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Item    : Subindentifier_Type
             )  is
      This   : Subindentifier_Type := Item;
      Buffer : Stream_Element_Array (1..80);
      First  : Stream_Element_Offset := Buffer'Last + 1;
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer > Data'Last + 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      loop
         First := First - 1;
         Buffer (First) := Stream_Element (This mod 128) or 128;
         This := This / 128;
         exit when This = 0;
      end loop;
      Buffer (Buffer'Last) := Buffer (Buffer'Last) and 16#7F#;
      if Pointer + Buffer'Last - First > Data'Last then
         Raise_Exception (End_Error'Identity, No_Room);
      end if;
      Data (Pointer..Pointer + Buffer'Last - First) :=
         Buffer (First..Buffer'Last);
      Pointer := Pointer + Buffer'Last - First + 1;
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Object_Identifier
             )  is
      First : Subindentifier_Type;
      Index : Stream_Element_Offset := Pointer;
   begin
      if Value'Length < 2 then
         Raise_Exception
         (  Constraint_Error'Identity,
            Invalid_Length
         );
      end if;
      First := Value (Value'First) * 40 + Value (Value'First + 1);
      Put (Data, Index, First);
      for Item in Value'First + 2..Value'Last loop
         Put (Data, Index, Value (Item));
      end loop;
      Pointer := Index;
   end Put;

   procedure Put_Relative
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Object_Identifier
             )  is
      Index : Stream_Element_Offset := Pointer;
   begin
      for Item in Value'Range loop
         Put (Data, Index, Value (Item));
      end loop;
      Pointer := Index;
   end Put_Relative;

   function Self
            (  Item : Implicit_External_OID_Data_Item'Class
            )  return Implicit_External_OID_Data_Item_Ptr is
      package From_Address is
         new System.Address_To_Access_Conversions
             (  Implicit_External_OID_Data_Item'Class
             );
      use From_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_External_OID_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length < 0 then
         Set_Implicit_Tag (ASN1_Data_Item (Item), Tag, Length);
      else
         Item.Length := Natural (Length);
      end if;
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_OID_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length < 0 then
         Set_Implicit_Tag (ASN1_Data_Item (Item), Tag, Length);
      else
         Item.Length := Natural (Length);
      end if;
   end Set_Implicit_Tag;

   procedure Set_Value
             (  Item  : in out Implicit_OID_Data_Item;
                Value : Object_Identifier
             )  is
   begin
      if Value'Length > Item.Size then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      else
         Item.Value (1..Value'Length) := Value;
         Item.Length := Value'Length;
      end if;
   end Set_Value;

   procedure Set_Value
             (  Item  : in out Implicit_External_OID_Data_Item;
                Value : Object_Identifier
             )  is
   begin
      if Item.Buffer = null then
         Uninitialized (Item);
      else
         declare
            type OID is
               array (Value'Range) of aliased Subindentifier_Type;
            type OID_Ptr is access all OID;
            for OID_Ptr'Storage_Pool use Item.Buffer.Pool;
            Ptr : OID_Ptr;
         begin
            Ptr := new OID;
            Ptr.all := OID (Value);
            Item.Length := Value'Length;
            Item.Value  := Ptr (1)'Unchecked_Access;
         end;
      end if;
   end Set_Value;

end GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;
