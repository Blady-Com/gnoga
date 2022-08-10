--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Lengths                                Spring, 2019       --
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

with Ada.Unchecked_Conversion;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Lengths is
   Too_Large   : constant String := "ASN.1 length is too large";
   Invalid_EOC : constant String := "Invalid indefinite ASN.1 " &
                                    "object termination";

   type Embedded_Length is mod 2**24;
   type Embedded_Length_Type is record
      Length : Embedded_Length;
      Count  : Stream_Element;
   end record;
   for Embedded_Length_Type use record
      Length at 0 range  0..23;
      Count  at 0 range 24..31;
   end record;
   for Embedded_Length_Type'Size use Unsigned_32'Size;

   procedure Call
             (  Item    : in out Measured_Data_Item;
                Callee  : in out Data_Item'Class;
                Tag     : Tag_Type;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Item.Tag := Tag;
      Item.List.List (1) := Callee'Unchecked_Access;
      Item.Self.Caller   := null;
      Item.Self.State    := 0;
      Item.Self.Current  := 1;
      Item.Self.List (1) := Item'Unchecked_Access;
      Call (Client, Pointer, Item.Self'Unchecked_Access, State);
      Item.Fed_Count := Client.Fed;
      State := 0;
   end Call;

   procedure Definite_Encode
             (  Item    : Abstract_ASN1_Data_Item'Class;
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
      elsif Data'Last - Pointer < 1 then
         Raise_Exception (End_Error'Identity, No_Room);
      end if;
      declare
         Index  : Stream_Element_Offset := Pointer + 1;
         Buffer : Stream_Element_Array (1..256);
         Length : Stream_Element_Offset := 1;
      begin
         Encode (Item, Data, Index);
         Put (Buffer, Length, Index - Pointer - 1);
         Length := Length - 1; -- The length's length
         if Length - 1 > Data'Last - Index then
            Raise_Exception (End_Error'Identity, No_Room);
         end if;
         if Length = 1 then
            Data (Pointer) := Buffer (1);
            Pointer := Index;
         else
            Data (Pointer + Length..Index - 2 + Length) :=
               Data (Pointer + 1..Index - 1);
            Data (Pointer..Pointer + Length - 1) := Buffer (1..Length);
            Pointer := Index + Length - 1;
         end if;
      end;
   end Definite_Encode;

   procedure Generic_Explicit_Encode
             (  Item    : Data_Item_Type;
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
      elsif Data'Last - Pointer < 1 then
         Raise_Exception (End_Error'Identity, No_Room);
      end if;
      declare
         This   : ASN1_Data_Item'Class renames
                  ASN1_Data_Item'Class (Item);
         Index  : Stream_Element_Offset := Pointer;
         Start  : Stream_Element_Offset;
         Buffer : Stream_Element_Array (1..256);
         Length : Stream_Element_Offset := 1;
      begin
         Put
         (  Data,
            Index,
            (Universal_Tag, Get_ASN1_Type (This), False),
            Always_Constructed (This)
         );
         Index := Index + 1; -- Reserve place for 1-octet length
         Start := Index;
         Encode (Item, Data, Index);
         Put (Buffer, Length, Index - Start); -- Encode length outside
         Length := Length - 1; -- The length's length
         if Length - 1 > Data'Last - Index then
            Raise_Exception (End_Error'Identity, No_Room);
         end if;
         if Length = 1 then
            Data (Start - 1) := Buffer (1);
            Pointer := Index;
         else
            Data (Start + Length - 1..Index + Length - 2) :=
               Data (Start..Index - 1);
            Data (Start - 1..Start + Length - 2) := Buffer (1..Length);
            Pointer := Index + Length - 1;
         end if;
      end;
   end Generic_Explicit_Encode;

   function From_State
            (  State : Stream_Element_Offset
            )  return Embedded_Length_Type is
      function Convert is
         new Ada.Unchecked_Conversion
             (  Unsigned_32,
                Embedded_Length_Type
             );
   begin
      return Convert (Unsigned_32 (-State));
   end From_State;

   procedure Generic_Put
             (  Data        : in out Stream_Element_Array;
                Pointer     : in out Stream_Element_Offset;
                Tag         : ASN1_Type;
                Value       : Value_Type;
                Constructed : Boolean := False
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1

         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      elsif Data'Last - Pointer < 1 then
         Raise_Exception (End_Error'Identity, No_Room);
      end if;
      declare
         Index  : Stream_Element_Offset := Pointer;
         Start  : Stream_Element_Offset;
         Buffer : Stream_Element_Array (1..256);
         Length : Stream_Element_Offset := 1;
      begin
         Put (Data, Index, (Universal_Tag, Tag, False), Constructed);
         Index := Index + 1; -- Reserve place for 1-octet length
         Start := Index;
         Put (Data, Index, Value);
         Put (Buffer, Length, Index - Start); -- Encode length outside
         Length := Length - 1; -- The length's length
         if Length - 1 > Data'Last - Index then
            Raise_Exception (End_Error'Identity, No_Room);
         end if;
         if Length = 1 then
            Data (Start - 1) := Buffer (1);
            Pointer := Index;
         else
            Data (Start + Length - 1..Index + Length - 2) :=
               Data (Start..Index - 1);
            Data (Start - 1..Start + Length - 2) := Buffer (1..Length);
            Pointer := Index + Length - 1;
         end if;
      end;
   end Generic_Put;

   function To_State
            (  Status : Embedded_Length_Type
            )  return Stream_Element_Offset is
      function Convert is
         new Ada.Unchecked_Conversion
             (  Embedded_Length_Type,
                Unsigned_32
             );
   begin
      return -Stream_Element_Offset (Convert (Status));
   end To_State;

   function Is_Indefinite
            (  State : Stream_Element_Offset
            )  return Boolean is
   begin
      return State = Stream_Element_Offset'First;
   end Is_Indefinite;

   function Is_Length_Ready
            (  State : Stream_Element_Offset
            )  return Boolean is
   begin
      return
      (  State = Stream_Element_Offset'First
      or else
         (State < 0 and then From_State (State).Count = 1)
      );
   end Is_Length_Ready;

   function Get_Length
            (  State : Stream_Element_Offset
            )  return Stream_Element_Count is
   begin
      return Stream_Element_Count (From_State (State).Length);
   end Get_Length;

   procedure Embedded_Feed
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                State   : in out Stream_Element_Offset
             )  is
      Status : Embedded_Length_Type := From_State (State);
   begin
      if State >= 0 or else State = Stream_Element_Offset'First then
         Raise_Exception (Data_Error'Identity, "Invalid length state");
      end if;
      if Status.Count = 127 then
         if 0 = (Data (Pointer) and 16#80#) then -- Single octet length
            State := To_State
                     (  (  Length => Embedded_Length (Data (Pointer)),
                           Count  => 1
                     )  );
            Pointer := Pointer + 1;
            return;
         else
            Status.Count := Data (Pointer) and 16#7F#;
            if Status.Count = 16#7F# then
               Raise_Exception (Data_Error'Identity, Invalid_Length);
            elsif Status.Count = 16#7E# then
               Raise_Exception (Constraint_Error'Identity, Too_Large);
            elsif Status.Count = 0 then -- Indefinite length
               State   := Stream_Element_Offset'First;
               Pointer := Pointer + 1;
               return;
            end if;
            Status.Count := Status.Count + 1;
            Pointer := Pointer + 1;
         end if;
      end if;
      declare
         Length : Integer_32 := Integer_32 (Status.Length);
      begin
         while Status.Count > 1 and then Pointer <= Data'Last loop
            Length := Length * 256 + Integer_32 (Data (Pointer));
            if Length > Integer_32 (Embedded_Length'Last) then
               Raise_Exception (Constraint_Error'Identity, Too_Large);
            end if;
            Pointer := Pointer + 1;
            Status.Count := Status.Count - 1;
         end loop;
         Status.Length := Embedded_Length (Length);
      end;
      State := To_State (Status);
      if State >= 0 then
         Raise_Exception (Program_Error'Identity, "Internal error");
      end if;
   end Embedded_Feed;

   procedure End_Of_Subsequence
             (  Item    : in out Measured_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      Count : constant Stream_Element_Offset :=
              Stream_Element_Offset (Client.Fed - Item.Fed_Count);
   begin
      if Item.Untagged then
         State := 0;
      elsif State = -3 then
         State := 0;
      elsif Item.Definite then
         if Count /= Item.Length then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         State := 0;
      else
         State := -1;
      end if;
      Item.Total := Item.Total + Count;
   end End_Of_Subsequence;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Measured_Data_Item
             )  is
   begin
      null;
   end Enumerate;

   procedure Feed
             (  Item    : in out Length_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Item.Value := 0;
         if 0 = (Data (Pointer) and 16#80#) then -- Single octet length
            Item.Value := Stream_Element_Count (Data (Pointer));
            Pointer    := Pointer + 1;
            State      := 0;
            return;
         else
            State := Stream_Element_Offset (Data (Pointer) and 16#7F#);
            if State = 16#7F# or else State = 0 then
               Raise_Exception (Data_Error'Identity, Invalid_Length);
            end if;
            Pointer := Pointer + 1;
         end if;
      end if;
      while State > 0 and then Pointer <= Data'Last loop
         begin
            Item.Value :=
               (  Item.Value * 256
               +  Stream_Element_Offset (Data (Pointer))
               );
         exception
            when Constraint_Error =>
               Raise_Exception (Constraint_Error'Identity, Too_Large);
         end;
         State   := State - 1;
         Pointer := Pointer + 1;
      end loop;
   end Feed;

   procedure Feed
             (  Item    : in out Measured_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      procedure Call_Item is
      begin
         Item.List.Caller  := null;
         Item.List.State   := 0;
         Item.List.Current := 1;
         if Item.List.List (1).all in ASN1_Data_Item'Class then
            declare
               Callee : ASN1_Data_Item'Class renames
                        ASN1_Data_Item'Class (Item.List.List (1).all);
            begin
--                 if Item.Tag.Class = Universal_Tag then
--                    if Natural (Get_ASN1_Type (Callee)) /= Item.Tag.Value
--                    then
--                       Raise_Exception
--                       (  Data_Error'Identity,
--                          (  "ASN.1 universal tag "
--                          &  Image (Item.Tag)
--                          &  " does not match the object type "
--                          &  Image (Get_ASN1_Type (Callee))
--                       )  );
--                    end if;
--                 end if;
               if not Item.Definite and then Is_Implicit (Callee) then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "ASN.1 implicitly encoded object has "
                     &  "no definite length"
                  )  );
               elsif Item.Untagged then
                  Set_Untagged (Callee);
               else
                  Set_Implicit_Tag (Callee, Item.Tag, Item.Length);
               end if;
            end;
         elsif Item.List.List (1).all in
               Abstract_ASN1_Data_Item'Class then
            declare
               Callee : Abstract_ASN1_Data_Item'Class renames
                        Abstract_ASN1_Data_Item'Class
                        (  Item.List.List (1).all
                        );
            begin
--                 if Item.Tag.Class = Universal_Tag then
--                    Raise_Exception
--                    (  Data_Error'Identity,
--                       "An ASN.1 choice has universal tag"
--                    );
--                 end if;
               if Item.Untagged then
                  Set_Untagged (Callee);
               else
                  Set_Implicit_Tag (Callee, Item.Tag, Item.Length);
               end if;
            end;
         else
            Raise_Exception
            (  Data_Error'Identity,
               "Tagged object is not ASN.1"
            );
         end if;
         if Item.Length = 0 and then not Item.Untagged then
            Item.Fed_Count := Client.Fed;
            End_Of_Subsequence (Item, Data, Pointer, Client, State);
         else
            Call (Client, Pointer, Item.List'Unchecked_Access, -1);
            Item.Fed_Count := Client.Fed;
            State := 0;
         end if;
      end Call_Item;
   begin
      if State = 0 then
         Item.Total := 0;
         if Item.Untagged then
            Item.Length := 0;
            Call_Item;
            return;
         end if;
         Item.Length := Stream_Element_Count (Data (Pointer));
         case Item.Length is
            when 16#80# => -- Indefinite length
               Item.Definite := False;
               Pointer       := Pointer    + 1;
               Item.Total    := Item.Total + 1;
               Item.Length   := -1;
               Call_Item;
               return;
            when 16#00#..16#7F# => -- Single octet length
               Item.Definite := True;
               Pointer       := Pointer    + 1;
               Item.Total    := Item.Total + 1;
               Call_Item;
               return;
            when others => -- Several octets length
               State := Item.Length mod 16#80#;
               if State = 16#7F# or else State = 0 then
                  Raise_Exception (Data_Error'Identity, Invalid_Length);
               end if;
               Item.Length   := 0;
               Item.Definite := True;
               Pointer       := Pointer    + 1;
               Item.Total    := Item.Total + 1;
         end case;
      end if;
      while Pointer <= Data'Last loop
         if State > 0 then -- Getting length
            begin
               Item.Length :=
                  (  Item.Length * 256
                  +  Stream_Element_Offset (Data (Pointer))
                  );
            exception
               when Constraint_Error =>
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     Too_Large
                  );
            end;
            State      := State - 1;
            Pointer    := Pointer + 1;
            Item.Total := Item.Total + 1;
            if State = 0 then
               Call_Item;
               return;
            end if;
         elsif State = -1 then -- The first zero
            if 0 /= Data (Pointer) then
               Raise_Exception (Data_Error'Identity, Invalid_EOC);
            end if;
            Pointer    := Pointer + 1;
            Item.Total := Item.Total + 1;
            State      := -2;
         else -- The second zero
            if 0 /= Data (Pointer) then
               Raise_Exception (Data_Error'Identity, Invalid_EOC);
            end if;
            Pointer    := Pointer + 1;
            Item.Total := Item.Total + 1;
            State      := 0;
            return;
         end if;
      end loop;
   end Feed;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Stream_Element_Count
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
      elsif Pointer <= Data'Last then
         if 0 = (Data (Pointer) and 16#80#) then -- Single octet length
            Value   := Stream_Element_Count (Data (Pointer));
            Pointer := Pointer + 1;
            return;
         end if;
         declare
            Length : constant Stream_Element_Count :=
                     Stream_Element_Count (Data (Pointer) and 16#7F#);
            Result : Stream_Element_Count := 0;
         begin
            if Length = 16#7F# then
               Raise_Exception (Data_Error'Identity, Invalid_Length);
            elsif Data'Last - Pointer >= Length then
               for Octet in 1..Length loop
                  begin
                     Result :=
                        (  Result * 256
                        +  Stream_Element_Offset
                           (  Data (Pointer + Octet)
                        )  );
                  exception
                     when Constraint_Error =>
                        Raise_Exception
                        (  Constraint_Error'Identity,
                           Too_Large
                        );
                  end;
               end loop;
               Pointer := Pointer + Length + 1;
               Value   := Result;
               return;
            end if;
         end;
      end if;
      Raise_Exception
      (  End_Error'Identity,
         "Non-terminated or missing ASN.1 length"
      );
   end Get;

   function Get_Length
            (  Item : Measured_Data_Item
            )  return Stream_Element_Count is
   begin
      return Item.Total;
   end Get_Length;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Stream_Element_Count
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
      if Value <= 127 then -- Single octet length
         if Pointer > Data'Last then
            Raise_Exception (End_Error'Identity, No_Room);
         end if;
         Data (Pointer) := Stream_Element (Value);
         Pointer := Pointer + 1;
      else
         declare
            Item   : Stream_Element_Count  := Value;
            Length : Stream_Element_Offset := 1;
         begin
            loop
               Item := Item / 256;
               exit when Item = 0;
               Length := Length + 1;
            end loop;
            if Data'Last - Pointer < Length then
               Raise_Exception (End_Error'Identity, No_Room);
            end if;
            Data (Pointer) := Stream_Element (Length) or 16#80#;
            Item := Value;
            for Octet in reverse Pointer + 1..Pointer + Length loop
               Data (Octet) := Stream_Element (Item mod 256);
               Item := Item / 256;
            end loop;
            Pointer := Pointer + Length + 1;
         end;
      end if;
   end Put;

   function Start_Length return Stream_Element_Offset is
   begin
      return To_State ((Length => 0, Count => 127));
   end Start_Length;

   procedure Set_Untagged
             (  Item     : in out Measured_Data_Item;
                Untagged : Boolean
             )  is
   begin
      Item.Untagged := Untagged;
   end Set_Untagged;

end GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
